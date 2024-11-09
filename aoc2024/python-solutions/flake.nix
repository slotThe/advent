{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  outputs = { self, nixpkgs }:
    let
      system  = "x86_64-linux";
      pkgs    = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          python3
          python3Packages.pip
          python3Packages.virtualenv
          pythonManylinuxPackages.manylinux2014Package
        ];
	      shellHook = ''
          export LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib.outPath}/lib:${pkgs.pythonManylinuxPackages.manylinux2014Package}/lib:$LD_LIBRARY_PATH";
          test -d .nix-venv || ${pkgs.python3.interpreter} -m venv .nix-venv
          source .nix-venv/bin/activate
        '';
      };
    };
}
