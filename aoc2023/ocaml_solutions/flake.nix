{
  inputs = {
    nixpkgs.follows = "opam-nix/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs }:
    let package = "ocaml_solutions";
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          ocaml_solutions =
            opam-nix.lib.${system}.buildDuneProject { } package ./.
              { ocaml-base-compiler = "*"; };
          overlay = final: prev: {};
      in {
        legacyPackages = ocaml_solutions.overrideScope' overlay;

        # Executed by `nix build`
        packages.default = self.legacyPackages.${system}.${package};

        # Used by `nix develop`
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs.ocamlPackages; [
            merlin
            ocaml-lsp
          ];
          inputsFrom = [ self.packages.${system}.default ];
        };
      });
}
