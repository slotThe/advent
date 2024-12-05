# 𝕨 Split 𝕩 splits the vector 𝕩 according to the separator 𝕨.
#
#     ' ' Split "  A string   containing a   sentence.   "
# ⟨ "A" "string" "containing" "a" "sentence." ⟩
Split ⇐ (¬-˜⊢×·+`»⊸>)∘≠⊔⊢

# Like split, but works for higher-dimensional things.
#
#     "z" SplitF ⟨"a", "b", "z", "c", "z", "d", "e", "f"⟩
# ⟨ ⟨ "a" "b" ⟩ ⟨ "c" ⟩ ⟨ "d" "e" "f" ⟩ ⟩
SplitF ⇐ { ((¯1+⊢×·+`»⊸<)¬𝕨⊸≡¨𝕩)⊔𝕩 }

# F _Fix y computes the fixed point of F starting at y (avoiding stack
# overflow).
_Fix ⇐ { 𝔽∘⊢⍟≢⟜𝔽_𝕣∘⊢⍟≢⟜𝔽𝕩 }

# Slurp out all integers in a string.
#     Ints "(12,-246,478|rstr)-45"
# ⟨ 12 ¯246 478 ¯45 ⟩
Ints ⇐ { •ParseFloat¨((¯1+⊢×·+`»⊸<)𝕩∊'-'∾'0'+↕10)⊔𝕩 }

tab ⇐ @+9
lf  ⇐ @+10
