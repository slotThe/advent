⟨Ints⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← Ints¨•FLines"../inputs/day02.txt"

S ← { ∧´∊⟜1‿2‿3(|∘-˝˘2⊸↕)𝕩 ∧ (≡⟜∨ ∨ ≡⟜∧)𝕩 }
+´S¨inp                       # ⇒ 213

⊑+´{ 1∊S¨(<˘≠⌜˜↕≠𝕩)/¨<𝕩 }¨inp # ⇒ 285
