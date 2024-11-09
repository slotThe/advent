{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      system  = "x86_64-linux";
      pkgs    = nixpkgs.legacyPackages.${system};
    in {
      # nix develop
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          (dune_3.overrideAttrs(old: {
              version = "3.17.0";
              src = fetchFromGitHub {
                owner = "ocaml";
                repo = "dune";
                rev = "b61e9d612642dbd1fd58d25c90126d607e69bd43";
                hash = "sha256-XLJ3anchLhogOD2AiAi12950xRWeitERTZ21YpaY5SU=";
              };
            }))
          ocaml
          ocamlPackages.findlib
          ocamlPackages.utop
          ocamlPackages.merlin
          ocamlPackages.odoc
          ocamlPackages.ounit
          ocamlPackages.ocaml-lsp
          ocamlPackages.base
          ocamlPackages.core
          ocamlPackages.stdio
          ocamlPackages.angstrom
          ocamlPackages.re
          ocamlPackages.ppx_jane
          ocamlformat
        ];
      };
    };
}
