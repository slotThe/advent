{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let system  = "x86_64-linux";
        pkgs    = nixpkgs.legacyPackages.${system};
        overlay = final: prev: {
          advent-of-code = prev.callCabal2nix "advent-of-code" ./. { };
        };
        haskellPackages = pkgs.haskellPackages.extend overlay;
    in {
      devShells.${system}.default = haskellPackages.shellFor {
        packages = p: [ p.advent-of-code ];
        buildInputs = with haskellPackages; [
          cabal-install
          haskell-language-server
        ];
      };
    };
}
