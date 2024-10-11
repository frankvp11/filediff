{ mkDerivation, base, bytestring, data-default
, data-memocombinators, directory, either, hashmap, lib, mtl
, rainbow, tasty, tasty-hunit, text, threads, time, transformers
, transformers-either, Zora
}:
mkDerivation {
  pname = "filediff";
  version = "2.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring data-default data-memocombinators directory either
    hashmap mtl rainbow tasty tasty-hunit text threads time
    transformers transformers-either Zora
  ];
  testHaskellDepends = [
    base directory either mtl tasty tasty-hunit text time transformers
    transformers-either
  ];
  homepage = "https://github.com/frankvp11/filediff";
  description = "Diffing and patching module";
  license = lib.licenses.bsd3;
}