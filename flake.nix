{
  description = "My emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            emacs-overlay.overlays.emacs
            emacs-overlay.overlays.package
          ];
        };
      in {
        defaultPackage = pkgs.emacsWithPackagesFromUsePackage {
          config = ./init.org;
          defaultInitFile = true;
          alwaysEnsure = true;
          alwaysTangle = true;
        };
      });
}
