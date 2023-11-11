{
  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, clj-nix }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages = {
        default = clj-nix.lib.mkCljApp {
          pkgs    = nixpkgs.legacyPackages.${system};
          modules = [
            {
              projectSrc         = ./.;
              name               = "com.github.slotThe/advent";
              main-ns            = "clojure-solutions.core";
              nativeImage.enable = true;
            }
          ];
        };
      };
    });
}
