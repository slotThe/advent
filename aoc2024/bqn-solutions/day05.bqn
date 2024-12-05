⟨Split,_Fix⟩ ← •Import "../../util/bqn_util/util.bqn"
ord‿upd ← {
  n ← ⊑/⟨⟩⊸≡¨inp←•FLines"../inputs/day05.txt"
  P ← •ParseFloat¨¨∘(Split¨)
  ⟨ '|'P n↑inp, ','P(n+1)↓inp ⟩
}
Out ← { a‿b𝕊xs: (⊑⎊¯∞/a=xs) < ⊑⎊∞/b=xs } # Not intersecting
MI ← (⌊2÷˜≠)⊸⊑   # Middle index
bad‿good ← { ∧´ord Out¨<𝕩 }¨⊸⊔ upd

!4578≡+´MI¨good

S ← { a‿b𝕊xs: {¬a‿b Out 𝕩}◶⟨⊢, {⌽⌾(⟨⊑/a=𝕩,⊑/b=𝕩⟩⊑¨<)𝕩}⟩ xs } # Switch
!6179≡+´MI∘(ord⊸(S´˜)_Fix)¨bad
