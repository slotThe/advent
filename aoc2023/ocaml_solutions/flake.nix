{
  inputs = {
    unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.follows = "opam-nix/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, unstable }:
    let package = "ocaml_solutions";
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          queries = {
            ocaml-base-compiler = "5.1.0";
            merlin = "*";
            ocamlformat = "*";
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
          buildInputs = devPackages ++ [
            unstable.legacyPackages.${system}.ocamlPackages.ocaml-lsp
          ];
          inputsFrom = [ packages.default ];
        };
      });
}
