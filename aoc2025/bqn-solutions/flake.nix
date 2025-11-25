{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      cbqn = pkgs.cbqn.overrideAttrs(old: rec {
        version = "v0.10.0";
        src = pkgs.fetchFromGitHub {
          owner = "dzaima";
          repo = "CBQN";
          rev = version;
          hash = "sha256-RZIxIRlx1SSYP+WrMRvg6nUqqs4zqEaGPvFyY3WFgbU=";
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
