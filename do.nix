{

  configure = {
    help = "Configure the project using Nix";
    script = ''
      cabal2nix . > default.nix
      nix-shell --command "cabal configure --enable-tests"
    '';
  };

  build = {
    help = "Build the project";
    script = ''
      cabal build
    '';
  };

  test = {
    help = "Test the project";
    script = ''
      cabal test
    '';
  };

  repl = {
    help = "Open the project in the repl";
    script = ''
      cabal repl lib:do
    '';
  };

  ghcid = {
    help = "Open the project in the ghcid";
    script = ''
      ghcid --command "cabal repl lib:do"
    '';
  };

}
