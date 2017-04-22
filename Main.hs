{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE LambdaCase                 #-}
module Main where

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Maybe
import Data.Pool
import Data.Proxy
import Database.PostgreSQL.LibPQ
import Database.Selda
import Database.Selda.Backend
import Database.Selda.Generic
import Database.Selda.PostgreSQL
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server

data Config = Config {
  pgPool :: Pool Connection
}

makeConfig :: PGConnectInfo -> IO Config
makeConfig connString =
  Config <$> createPool start finish 10 (0.5) 10
    where
      start = connectdb (pgConnString connString)

newtype App a = App {
  runApp :: ReaderT Config Handler a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Config
           , MonadError ServantErr
           , MonadIO
           , MonadBase IO
           )

newtype AppSt a = AppSt {
  runAppSt :: StM (ReaderT Config (ExceptT ServantErr IO)) a
}
instance MonadBaseControl IO App where
  type StM App a = AppSt a
  liftBaseWith f = App
                 $ liftBaseWith
                 $ \g' -> f
                 $ \m -> fmap AppSt
                 $ g' $ runApp m
  restoreM = App . restoreM . runAppSt

instance MonadSelda App where
    seldaBackend = do
      Config pool <- ask
      withResource pool $ \conn ->
        pure (pgBackend conn)

data Todo = Todo {
    todoId    :: Int
  , name      :: Text
  , completed :: Bool
  , deleted   :: Bool
  } deriving (Generic, Show)

instance ToJSON Todo
instance FromJSON Todo

todos :: GenTable Todo
todos = genTable "todo" [ todoId :- autoPrimaryGen ]

-- | GET
getTodosDB :: MonadSelda m => m [Todo]
getTodosDB =
  map fromRel <$> query (select (gen todos))

-- | GET
getTodoDB :: MonadSelda m => Int -> m (Maybe Todo)
getTodoDB tid = do
  (fromRel <$>) . listToMaybe <$> (query $ do
    t <- select (gen todos)
    restrict (t ! todoId .== literal tid)
    return t)

-- | POST
newTodoDB :: MonadSelda m => Text -> m Int
newTodoDB name = insertGen todos [ Todo def name False False ]

-- | PUT
updateTodoDB :: MonadSelda m => Int -> Text -> m Int
updateTodoDB tid name =
  update (gen todos)
    (\t -> t ! todoId .== literal tid)
    (\t -> t ! todoId
       :*: literal name
       :*: t ! completed
       :*: t ! deleted)

-- | PATCH
completeTodoDB :: MonadSelda m => Int -> m ()
completeTodoDB tid =
  update_ (gen todos)
    (\t -> t ! todoId .== literal tid)
    (\t -> t ! todoId
       :*: t ! name
       :*: literal True
       :*: t ! deleted)

-- | DELETE
deleteTodoDB :: MonadSelda m => Int -> m ()
deleteTodoDB tid =
  update_ (gen todos)
    (\t -> t ! todoId .== literal tid)
    (\t -> t ! todoId
       :*: t ! name
       :*: t ! completed
       :*: literal True)

-- | Toggles todo completion
type CompleteTodo = Capture "todo-id" Int :> Patch '[JSON] Todo

completeTodo :: Int -> App Todo
completeTodo todoId = do
  -- | Validation: Check existence, oscillate value.
  completeTodoDB todoId
  getTodoDB todoId >>= \case
    Nothing -> throwError err404
    Just todo -> pure todo

-- | New Todo
type NewTodo = ReqBody '[JSON] Text :> Post '[JSON] Todo

newTodo :: Text -> App Todo
newTodo todoName = do
  -- | Does this actually return the correct todoId?
  todoId <- newTodoDB todoName
  getTodoDB todoId >>= \case
    Nothing -> throwError err404
    Just todo -> pure todo

-- | Updates Todo
type UpdateTodo = Capture "todo-id" Int :> Capture "body" Text :> Put '[JSON] Todo

updateTodo :: Int -> Text -> App Todo
updateTodo todoId todoName = do
  -- | Validation.. allow empty string?
  getTodoDB todoId >>= \case
    Nothing -> throwError err404
    Just _ -> do
      () <$ updateTodoDB todoId todoName
      getTodoDB todoId >>= \case
        Nothing -> throwError err404
        Just todo -> pure todo

-- | Deletes Todo
type DeleteTodo = Capture "todo-id" Int :> Delete '[JSON] Todo

deleteTodo :: Int -> App Todo
deleteTodo todoId = do
  getTodoDB todoId >>= \case
    Nothing -> throwError err404
    Just _ -> deleteTodoDB todoId
  getTodoDB todoId >>= \case
    Nothing -> throwError err404
    Just x -> pure x

-- | Retrieve Todo
type GetTodo = Capture "todo-id" Int :> Get '[JSON] Todo

getTodo :: Int -> App Todo
getTodo todoId = do
  getTodoDB todoId >>= \case
    Nothing -> throwError err404
    Just todo -> pure todo

-- | Retrieve all Todos
type GetAllTodos = Get '[JSON] [Todo]

getTodos :: App [Todo]
getTodos = getTodosDB

-- | Todo API
type TodoAPI =
       NewTodo
  :<|> CompleteTodo
  :<|> UpdateTodo
  :<|> DeleteTodo
  :<|> GetTodo
  :<|> GetAllTodos

todoHandlers :: ServerT API App
todoHandlers =
       newTodo
  :<|> completeTodo
  :<|> updateTodo
  :<|> deleteTodo
  :<|> getTodo
  :<|> getTodos

-- | API
type API = "todo" :> TodoAPI

-- | Vroom' vroom'
main :: IO ()
main = do
  config <- makeConfig ("dmj" `on` "127.0.0.1")
  run 3000 $ serve (Proxy @ API) (transform config `enter` todoHandlers)
  where
    transform :: Config -> App :~> Handler
    transform config =
      Nat $ \(App x) -> runReaderT x config


