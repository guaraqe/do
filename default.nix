{ mkDerivation, base, containers, data-fix, directory, hnix
, microlens, optparse-applicative, process, stdenv, tasty
, tasty-hunit, text, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "do";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-fix directory hnix microlens
    optparse-applicative process text time transformers
    unordered-containers
  ];
  executableHaskellDepends = [ base directory text ];
  testHaskellDepends = [ base directory tasty tasty-hunit text ];
  description = "A tool for defining command line interfaces in folders";
  license = stdenv.lib.licenses.bsd3;
}
