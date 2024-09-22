⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
a‿b ← •ParseFloat∘(¯1⊸⊑)∘(' '⊸Split)¨ •FLines "../inputs/day15.txt"

C ← { # So they are independent
 res ← ⟨⟩
 𝕨⊸{res ∾↩ 2147483647|(¯1⊑⥊𝕩)×𝕨}⍟40e6 𝕩
 res 65536⊸|↩
}
as ← 16807 C a
bs ← 48271 C b

•Show +´as=bs

T ← 5e6⊸↑⊢/˜(0⊸=|)
•Show +´(8 T bs)=(4 T as)
