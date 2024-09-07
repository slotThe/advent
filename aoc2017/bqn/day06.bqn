⟨Split, tab⟩ ← •Import "../../util/bqn_util/util.bqn"
Sol ← {
  h ← ⟨⟩•HashMap⟨⟩
  dup ← {
   m ← ⌈´𝕩
   𝕩+ +˝↑‿(≠𝕩)⥊(0⥊˜⊑𝕩⊐m)∾⟨-m⟩∾m⥊1
   #            before    at  after
   } •_while_ {
     r←¬h.Has𝕩 ⋄ 𝕩 h.Set 1 ⋄ r
   } 𝕩
  c ← h.Count 1
  ⟨c, ⊑ c - / dup⊸≡¨ h.Keys 1⟩
}
•Show Sol •ParseFloat¨ tab Split ⊑•FLines "../inputs/day06.txt"
