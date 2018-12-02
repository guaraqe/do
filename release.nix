let
  pkgs = import ./nixpkgs;
  dontCheck = pkgs.haskell.lib.dontCheck;
in
{
  # The test suite uses `nix-store` under the hood, which is not
  # possible for the moment.
  do = dontCheck (pkgs.haskellPackages.callPackage ./. {});
}
