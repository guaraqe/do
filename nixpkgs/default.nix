let
  nixpkgsSrc = builtins.fetchGit {
    name = "nixpkgs-unstable";
    url = https://github.com/NixOS/nixpkgs-channels/;
    ref = "nixos-18.09";
    rev = "c1427bf45ffd9f738cb811f980a7a4accf342783";
  };
in
  import nixpkgsSrc {}
