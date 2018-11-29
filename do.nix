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

}
