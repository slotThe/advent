inp ← •ParseFloat¨ •FLines "../inputs/day05.txt"
Sim ← { thresh 𝕊 ins‿i‿s: # instructions‿index‿step
  n ← i+i⊑ins
  ins ((thresh ≤ i⊑ins)⊑⟨1⊸+, ¯1⊸+⟩)⌾(i⊸⊑)↩
  ins‿n‿(s+1)
}

¯1⊑ ∞ Sim•_while_{ ·𝕊ins‿i‿s: (i<≠ins)∧i≥0 } inp‿0‿0 # ⇒ 351282

¯1⊑ 3 Sim•_while_{ ·𝕊ins‿i‿s: (i<≠ins)∧i≥0 } inp‿0‿0 # ⇒ 24568703
