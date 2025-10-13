{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  outputs = { self, nixpkgs }:
    let
      system  = "x86_64-linux";
      pkgs    = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          dune_3
          opam
          ocaml
          ocamlPackages.findlib
          ocamlPackages.utop
        ];
        shellHook = ''
          export LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib.outPath}/lib:$LD_LIBRARY_PATH";
          test -d _opam || opam switch create .
          eval $(opam env)
        '';
      };
    };
}
