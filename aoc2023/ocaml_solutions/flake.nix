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
          queries = {
            ocaml-lsp-server = "*";
            ocaml-base-compiler = "*";
            merlin = "*";
          };
          main = opam-nix.lib.${system}.buildDuneProject { } package ./. queries;
          overlay = final: prev: {
            ${package} = prev.${package}.overrideAttrs (_: {
              # Don't leak OCaml dependencies into dependent environments.
              doNixSupport = false;
            });
          };
          legacyPackages = main.overrideScope' overlay;
          devPackages = builtins.attrValues
            (pkgs.lib.getAttrs (builtins.attrNames queries) legacyPackages);
      in rec {
        inherit legacyPackages;

        # Executed by `nix build`
        packages.default = self.legacyPackages.${system}.${package};

        # Used by `nix develop`
        devShells.default = pkgs.mkShell {
          buildInputs = devPackages;
          inputsFrom = [ packages.default ];
        };
      });
}
