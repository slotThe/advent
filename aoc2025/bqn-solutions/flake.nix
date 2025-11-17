{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      cbqn = pkgs.cbqn.overrideAttrs(old: rec {
        version = "d2ef23631ab5da5c662dd2307a804db7b46cc82b";
        src = pkgs.fetchFromGitHub {
          owner = "dzaima";
          repo = "CBQN";
          rev = version;
          hash = "sha256-cRaXw7DKGj26VR2mg8qYkl9udQaXg6/e0m138SUTJvI=";
        };
        doInstallCheck = false;
      });
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          cbqn
        ];
      };
    };
}
