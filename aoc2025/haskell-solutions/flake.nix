{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      package = "haskell-solutions";
      system  = "x86_64-linux";
      pkgs    = nixpkgs.legacyPackages.${system};
      overlay = final: prev: {
        main = prev.callCabal2nix package ./. { };
        haskell-aoc-util = prev.callCabal2nix "haskell-aoc-util" ../../util/haskell-aoc-util { };
        sbv = pkgs.haskell.lib.dontCheck (prev.callCabal2nix "sbv" (builtins.fetchGit {
          url = "http://github.com/LeventErkok/sbv";
          rev = "c5fa5fbd60d822d52cf111d4bb62490220268f84";
        }) {});
      };
      haskellPackages = pkgs.haskellPackages.extend overlay;
    in {
      # nix build
      packages.${system}.default = haskellPackages.main;

      # nix develop
      devShells.${system}.default = haskellPackages.shellFor {
        packages = p: [ p.main ];
        buildInputs = with haskellPackages; [
          ghc
          cabal-install
          haskell-language-server
          pkgs.z3
        ];
      };
    };
}
