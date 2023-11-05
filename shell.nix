# A big-ass shell for most of the monorepo—because why not?

with (import <nixpkgs> {});

mkShell {
  buildInputs = [
    # Haskell
    cabal-install
    ghc
    haskell-language-server

    # Rust
    cargo
    rustc
    rust-analyzer
    clippy
    rustfmt

    # Clojure
    jdk
    clojure
    leiningen
  ];
}
