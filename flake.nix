{
  description = "Filediff Haskell package as a Nix flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, haskellNix, ... }: let
    pkgs = import nixpkgs {
      overlays = [ haskellNix.overlay ];
      system = "x86_64-linux"; # Adjust for your system
    };
  in {
    packages = {
      filediff = pkgs.haskell-nix.mkCabalProject {
        src = ./.;
        compiler-nix-name = "ghc9.8.1"; # Specify your GHC version
      };
    };
  };
}
