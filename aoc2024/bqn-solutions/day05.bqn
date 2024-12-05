⟨SplitF,Ints,_Fix⟩ ← •Import "../../util/bqn_util/util.bqn"
ord‿upd ← Ints¨¨⟨⟩SplitF•FLines"../inputs/day05.txt"
Out ← { a‿b𝕊xs: (⊑⎊¯∞/a=xs) < ⊑⎊∞/b=xs } # Not intersecting
bad‿good ← { ∧´ord Out¨<𝕩 }¨⊸⊔ upd
S ← { a‿b𝕊xs: {¬a‿b Out 𝕩}◶⟨⊢, {⌽⌾(⟨⊑/a=𝕩,⊑/b=𝕩⟩⊑¨<)𝕩}⟩ xs } # Switch
!4578‿6179≡ (+´{𝕩⊑˜⌊2÷˜≠𝕩}¨)¨⟨good, (ord⊸(S´˜)_Fix)¨bad⟩
