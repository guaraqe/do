let
  pkgs = import ./nixpkgs;
in
{
  do = pkgs.haskellPackages.callPackage ./. {};
}
