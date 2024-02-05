{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    # haskell toolchain
    pkgs.cabal-install
    pkgs.ghc
    pkgs.hlint
    pkgs.haskell-language-server
    pkgs.stack

    # system requirements
    pkgs.zlib

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
