{ mkDerivation, base, resource-pool, selda, selda-postgresql
, selda-sqlite, stdenv, servant, servant-server
}:
mkDerivation {
  pname = "selda-example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base resource-pool selda selda-postgresql selda-sqlite servant servant-server
  ];
  license = stdenv.lib.licenses.bsd3;
}
