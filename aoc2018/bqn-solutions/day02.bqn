inp ← •FLines"../inputs/day02.txt"

!6200≡ ⊑ ×´+´ 2‿3⊸∊∘(/⁼⊐)¨ inp

!"xpysnnkqrbuhefmcajodplyzw"≡ 1⊑⍷⥊{a𝕊b:
  {𝕩=1? ∾""⌾((⊑ds⊐1)⊸⊑)a; @}+´ds←a≠b
}⌜˜inp
