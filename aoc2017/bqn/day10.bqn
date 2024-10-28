⟨Split⟩←•Import"../../util/bqn_util/util.bqn"

inp ← ⌽ •BQN '⟨'∾'⟩'∾˜ ⊑•FLines"../inputs/day10.txt"
R ← { n 𝕊 xs‿p‿ss: # one round; 𝕨: one length; 𝕩: ⟨list of marks, position, skip size⟩
  l←≠xs
  ⟨ ⌽⌾((l|p+↕n)⊸⊏)xs , l|p+n+ss , ss+1 ⟩
}
×´ 2↑ ⊑ (↕256)‿0‿0 R´ inp # ⇒ 13760

inp2 ← ⊑•FLines"../inputs/day10.txt"
X ← ⊑⋈⊸(16•bit._xor)´ # XOR two 16 bit signed ints together
DecToHex ← { "0123456789abcdef" ⊏˜ 16(⌊∘÷˜ ⋈ |)𝕩 }
KnotHash ⇐ {
  ls ← ⌽ ⟨17, 31, 73, 47, 23⟩∾˜ 𝕩-@
  ∾ DecToHex∘X¨ 16(/⥊˜)⊸⊔ ⊑ R´⟜ls⍟64 (↕256)‿0‿0
}
KnotHash inp2 # ⇒ "2da93395f1a6bb3472203252e3b17fe5"
