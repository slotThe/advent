Turn ← { # Given direction 𝕩, turn 𝕨 steps to the right.
  ts ← ⟨¯1‿0, 0‿1, 1‿0, 0‿¯1⟩
  (⊑4|𝕨+ts⊐<𝕩)⊑ts
}
# Simulate a burst.
#   mod‿infectedTarget‿TurnOffset 𝕊 grid‿infectCount‿position‿direction
Sim ← { m‿t‿TO 𝕊 g‿i‿p‿d:
  pv ← 0 g.Get p      # Don't keep track of clean (= 0) cells.
  nd ← (TO pv) Turn d
  p g.Set m|pv+1      # New value for p.
  ⟨g, i+t=pv, p+nd, nd⟩
}

inp ← >•FLines "../inputs/day22.txt"
Start ← {⟨(⥊↕≢inp)•HashMap𝕩, 0, ⌊(≢inp)÷2, ¯1‿0⟩}

# The easy part: clean = 0 and infected = 1.
1⊑ ⟨2, 0, {𝕊0:¯1;𝕩}⟩ Sim⍟10_000 Start(⥊".#"⊐inp) # ⇒ 5460
# Clean = 0, weakened = 1, infected = 2, flagged = 3.
1⊑ ⟨4, 1, -⟜1⟩ Sim⍟10_000_000 Start(⥊". #"⊐inp)  # ⇒ 2511702
