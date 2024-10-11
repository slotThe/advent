⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
a‿b ← •ParseFloat∘(¯1⊸⊑)∘(' '⊸Split)¨ •FLines "../inputs/day15.txt"

Gen ← { fac𝕊n: # So they are independent
 res ← ⟨⟩
 {res∾↩2147483647|(¯1⊑⥊𝕩)×fac}⍟40e6 n
 res 65536⊸|↩
}
as ← 16807 Gen a
bs ← 48271 Gen b
+´as=bs             # ⇒ 631

F ← 5e6↑(0⊸=|)/⊢
+´(8 F bs)=(4 F as) # ⇒ 279
