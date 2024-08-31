inp ← •ParseFloat¨ •FLines "../inputs/day05.txt"
Sim ← { thresh 𝕊 v‿i‿s: # vector‿index‿step
  n ← i+i⊑v
  v (⟨1, ¯1⟩ ⊑˜ thresh≤i⊑v)⊸+⌾(i⊸⊑)↩
  v‿n‿(s+1)
}
•Show ¯1⊑ (∞ Sim •_while_ {·𝕊v‿i‿s: i<≠v ∧ i>0} inp‿0‿0)
•Show ¯1⊑ (3 Sim •_while_ {·𝕊v‿i‿s: i<≠v ∧ i>0} inp‿0‿0)
