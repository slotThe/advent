⟨Ints⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← 2⊸(↑⋈↓)∘Ints¨•FLines"../inputs/day14.txt"
bounds ← 101‿103

res ← bounds⥊0
{ 𝕊p‿v: res 1⊸+⌾((bounds|p+100×v)⊸⊑)↩ }¨inp

⟨rl‿rh, cl‿ch⟩ ← (⌊⋈⌈)÷⟜2¨bounds
!230435667≡×´⥊ +´∘⥊¨⟨ (rl⥊0)∾¯1∾rh⥊1, (cl⥊0)∾¯1∾ch⥊1 ⟩⊔res
7709 # Guessed
