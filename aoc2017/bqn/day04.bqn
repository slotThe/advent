⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
_Sol ← { +´ (∧´∊∘𝔽∘(' '⊸Split))¨ •FLines "../inputs/day04.txt" }
⊢  _Sol
∧¨ _Sol
