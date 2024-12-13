⟨Ints,SplitF⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← Ints¨¨⟨⟩SplitF•FLines"../inputs/day13.txt"

Inv2x2 ← { 𝕊a‿b‿c‿d:
  "Not invertible"!0≠det←(a×d)-b×c
  [[d,-b],[-c,a]]×÷det
}
IntegerSol ← { ∧´0.0001>|𝕩-˜a‿b←⌊0.5+𝕩? b+3×a ; 0 }
Solve ← +´ IntegerSol∘{ 𝕊a‿b‿g: +˝×⟜g Inv2x2 a∾b}⎊0¨

!29436≡Solve inp
!103729094227877≡Solve{𝕊x‿b‿goal: x‿b‿(goal+10_000_000_000_000)}¨inp
