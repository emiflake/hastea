{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    # Haskell miscellaneous tools for emacs
    stack
    hlint
    ghcid
    gnumake
    ghc
    ispell
    haskellPackages.hindent

    # for `nix-shell --run "make server"`
    haskellPackages.wai-app-static
  ];
}
