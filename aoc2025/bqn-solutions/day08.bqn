⟨Ints⟩←•Import"../../util/bqn_util/util.bqn"
inp ← Ints¨ •FLines "../inputs/day08.txt" ⋄ D←{⟨+´×˜(⊑𝕨)-⊑𝕩, 𝕨, 𝕩⟩}

set ← ↕≠inp # The simplest disjoint set implementation possible
R ← { 𝕩=(𝕩⊑set)? 𝕩; 𝕊𝕩⊑set } # Representative of a cycle
U ← { {𝕊a‿b: a≠b? set a⌾(b⊸⊑)↩ ⋄ 1; 0} R¨𝕩 } # Union of two cycles

il ← 1⊸⊑¨∘(1⊸↓)¨ ∧ (<⌜˜↕≠inp) /○⥊ (D⌜˜ ⋈¨⟜(↕∘≠) inp) # input list
U¨1000↑il ⋄ !97384= ×´3↑∨/⁼⊐R¨set

# 𝕩 is the number of disjoint cycles; resume from p1.
k←1000 ⋄ {U k⊑il? 𝕩-1; 𝕩}•_while_{k+↩1 ⋄ 𝕩≥2} (+´∊⊐R¨set)
!9003685096= ×´⊑¨inp⊏˜il⊑˜k-1
