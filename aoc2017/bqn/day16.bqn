⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
P ← {
  (1↓𝕩) {
    x 𝕊 's': •ParseFloat x;
    x 𝕊 'x': •ParseFloat¨ '/' Split x;
    x 𝕊 'p': 1∾'/' Split x
  } ⊑𝕩
}
M ← {
  ·‿a‿b 𝕊 x: (⊑x⊐a)‿(⊑x⊐b) M x;
    i‿j 𝕊 x: ⌽⌾(i‿j⊸⊏)x;
      n 𝕊 x: (-n)⌽x
}
S ← ⊢M´⟜(P¨⌽','Split⊑•FLines"../inputs/day16.txt")

•Show S('a'+↕16)

h ← ⟨⟩•HashMap⟨⟩
S •_while_ { b←¬h.Has𝕩 ⋄ 𝕩 h.Set 1 ⋄ b } ('a'+↕16)
•Show S⍟(1+(h.Count 1)|10e9) ¯1⊑h.Keys 1
