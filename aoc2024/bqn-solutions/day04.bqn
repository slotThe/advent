⟨Split,lf⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← >•FLines"../inputs/day04.txt"

Diag ← { g𝕊i‿j: { 𝕊'X': "MAS"≡{⟨i+𝕩,j+𝕩⟩(⊑⎊@)g}¨1+↕3; 𝕊·: 0 }i‿j⊑g }
_Rot ← { +´𝔽¨⌽∘⍉⍟(↕4)𝕩 }
!2493≡+´⟨
  (+´(+´"XMAS"⊸⍷)˘)_Rot inp # Horizontal and vertical
  {+´⥊𝕩⊸Diag¨↕≢𝕩}  _Rot inp # Diagonal
⟩

XMAS ← {g𝕊i‿j:
  {𝕊'M':
    a‿s←{⟨i+𝕩,j+𝕩⟩(⊑⎊@)g}¨1+↕2
    ms‿sm←{𝕊a‿b:⟨i+a,j+b⟩(⊑⎊@)g}¨⟨0‿2, 2‿0⟩
    (<ms‿a‿s‿sm)∊⟨"MASS","SASM"⟩;
   𝕊·: 0
  } i‿j⊑g
}
!1890≡⊑+´⟨⊢,⌽⌽˘⟩{+´⥊(𝕎𝕩)⊸XMAS¨↕≢𝕩}¨<inp
