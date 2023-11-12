{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let system  = "x86_64-linux";
        pkgs    = nixpkgs.legacyPackages.${system};
        overlay = final: prev: {
          haskell-solutions = prev.callCabal2nix "haskell-solutions" ./. { };
          haskell-aoc-util  = prev.callCabal2nix "haskell-aoc-util" ../../util/haskell-aoc-util { };
        };
        haskellPackages = pkgs.haskellPackages.extend overlay;
    in {
      devShells.${system}.default = haskellPackages.shellFor {
        packages = p: [ p.haskell-solutions ];
        buildInputs = with haskellPackages; [
          cabal-install
          haskell-language-server
        ];
      };
    };
}
