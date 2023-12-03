{
  inputs = {
    systems.url = "github:nix-systems/default";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs = {
        nixpkgs.follows     = "nixpkgs";
        systems.follows     = "systems";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = { self, nixpkgs, flake-utils, poetry2nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          inherit (poetry2nix.lib.mkPoetry2Nix { inherit pkgs; }) mkPoetryApplication;
      in {
        packages = {
          python-solutions = mkPoetryApplication { projectDir = self; };
          default = self.packages.${system}.python-solutions;
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.python-solutions ];
          packages   = [ pkgs.poetry ];
          nativeBuildInputs = [ pkgs.nodejs_21 ];
        };
      });
}
