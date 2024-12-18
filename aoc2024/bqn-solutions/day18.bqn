⟨Ints,_Fix⟩ ← •Import "../../util/bqn_util/util.bqn"
points ← Ints¨ •FLines"../inputs/day18.txt"
grid ← 1⌾(0‿0⊸⊑) 71‿71⥊0
free ← 0¨⌾((1024↑points)⊸⊑) 71‿71⥊1 # Points that we can step one get a 1.

# A single flood fill step of 𝕩, where 𝕨 are points we can step on. ⊢ is
# important for the fixed point.
FF ← { 𝕨∧ ∨´⟨⊢,»˘,«˘,»,«⟩{𝕎𝕩}¨<𝕩 }

!416≡ ⊑{𝕊i‿g: ⟨i+1, free FF g⟩}•_while_{𝕊·‿g: ¬70‿70⊑g} ⟨0, grid⟩

fm ← free
!50‿23≡ points⊑˜1-˜{ 𝕊i:
  (i+1) ⊣ fm 0⌾((i⊑points)⊸⊑)↩ # Mutation is *much* faster here
}•_while_{ 𝕊·: 70‿70⊑ fm⊸FF _Fix grid } 1024
