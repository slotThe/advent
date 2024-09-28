inp ← •ParseFloat¨ •FLines "../inputs/day05.txt"
n←≠inp
Sim ← { thresh 𝕊 i‿s: # index‿step
  n ← i+i⊑inp
  inp (thresh ≤ i⊑inp)◶⟨1⊸+, ¯1⊸+⟩⌾(i⊸⊑)↩
  n‿(s+1)
}

•Show ¯1⊑ ∞ Sim •_while_{ ·𝕊i‿s: i<n∧i≥0 } 0‿0   # ⇒ 351282

inp ← •ParseFloat¨ •FLines "../inputs/day05.txt" # Reset input
•Show ¯1⊑ (3 Sim •_while_ {·𝕊i‿s: i<n∧i≥0} 0‿0)  # ⇒ 24568703
