⟨Ints⟩ ← •Import "../../util/bqn_util/util.bqn"
inp←(⊑⋈1⊸↓)¨Ints¨•FLines"../inputs/day07.txt"
Sim←{ goal‿xs𝕊ops:
  ⊑goal× goal∊ ⟨⊑xs⟩{ a𝕊acc: ≤⟜goal⊸/ ∾{𝕊b: {b𝕏a}¨ops}¨acc }´⌽1↓xs
}
+´inp Sim¨<+‿×                        # ⇒ 3312271365652
+´inp Sim¨<⟨+, ×, •ParseFloat∾○•Repr⟩ # ⇒ 509463489296712
