⟨Split,cr⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← >•FLines "../inputs/day22.txt"
start ← ⟨0, ⌊(≢inp)÷2, ¯1‿0⟩

Turn ← { # Given direction 𝕩, turn 𝕨 steps to the right.
  ts ← ⟨¯1‿0, 0‿1, 1‿0, 0‿¯1⟩
  (4|𝕨+⊑ts⊐<𝕩)⊑ts
}

# Simulate a burst.
#   mod‿infectedTarget‿TurnOffset 𝕊 infectCount‿position‿direction
Sim ← { m‿t‿TO 𝕊 i‿p‿d:
  pv ← 0 h.Get p      # Don't keep track of clean (= 0) cells.
  nd ← (TO pv) Turn d
  p h.Set m|pv+1      # New value for p.
  ⟨i+t=pv, p+nd, nd⟩
}

# The easy part: clean = 0 and infected = 1.
h ← (⥊↕≢inp)•HashMap(⥊".#"⊐inp)
⊑ ⟨2, 0, {𝕊0:¯1;𝕩}⟩ Sim⍟10_000 start # ⇒ 5460

# Clean = 0, weakened = 1, infected = 2, flagged = 3.
h ← (⥊↕≢inp)•HashMap(⥊". #"⊐inp)
⊑ ⟨4, 1, -⟜1⟩ Sim⍟10_000_000 start # ⇒ 2511702
