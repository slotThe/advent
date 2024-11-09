{
  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let overlays  = [ (import rust-overlay) ];
          pkgs      = import nixpkgs { inherit system overlays; };
          toolchain = (pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default)).override {
            extensions = [ "rustc-codegen-cranelift-preview" ];
          };

      in with pkgs; {
        devShells.default = mkShell {
          buildInputs = [
            toolchain
            rust-analyzer
          ];
        };
      }
    );
}
