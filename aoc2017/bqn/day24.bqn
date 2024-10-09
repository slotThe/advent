⟨Split,_Fix,cr⟩ ← •Import"../../util/bqn_util/util.bqn"
inp ← •ParseFloat¨∘('/'⊸Split)¨ •FLines"../inputs/day24.txt"

SM ← ¬∘∊/⊣ # Set minus

# Select all elements in 𝕩 = ⟨⟨x₁₁, x₁₂⟩, ⟨x₂, y₂⟩, …⟩
# where one of the xᵢ₋'s matches 𝕨.
Sel ← (>∊¨)/⊢

F ← { t Rec xs: # Target value t, list xs
  {(+´𝕩) {r𝕊⟨⟩: r; 𝕨+⌈´𝕩} (𝕩⊑˜t=⊑𝕩) Rec xs SM ⟨𝕩⟩}¨t Sel xs
}
⌈´∾{𝕊0‿b: b+ b F inp SM ⟨0‿b,b‿0⟩}∘∧¨0 Sel inp # ⇒ 1656

MF ← ⌈´∘(⌈´⊸=∘(⊑¨)⊸/) # Max by the first element, with second element as tiebreaker.
F2 ← { i‿t Rec xs: # Step i, target value t, list xs
  {(+´𝕩) {r𝕊⟨⟩: ⟨i,r⟩; 0‿𝕨+MF𝕩} ⟨i+1, 𝕩⊑˜t=⊑𝕩⟩ Rec xs SM ⟨𝕩⟩}¨t Sel xs
}
⌈´∾{𝕊0‿b: 0‿b⊸+¨ 1‿b F2 inp SM ⟨0‿b,b‿0⟩}∘∧¨0 Sel inp # ⇒ ⟨ 30 1642 ⟩
