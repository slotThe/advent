⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← (•BQN¨':'⊸Split)¨ •FLines "../inputs/day13.txt"
+´{l‿n: l×n×0=l|˜2×n-1}¨inp # ⇒ 1316
+⟜1 •_while_ {𝕊d: ⊑0∊{l‿n: (l+d)|˜2×n-1}¨inp} 0 # ⇒ 3840052
