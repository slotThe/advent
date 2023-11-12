with (import <nixpkgs> {});

mkShell {
  buildInputs = [
    jdk
    clojure
    leiningen
  ];
}
