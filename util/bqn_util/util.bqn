# 𝕨 Split 𝕩 splits the vector 𝕩 according to the separator 𝕨.
#
# >>> ' ' Split "  A string   containing a   sentence.   "
# ⟨ "A" "string" "containing" "a" "sentence." ⟩
Split ⇐ (¬-˜⊢×·+`»⊸>)∘≠⊔⊢

# F _Fix y computes the fixed point of F starting at y (avoiding stack
# overflow).
_Fix ⇐ { 𝔽∘⊢⍟≢⟜𝔽_𝕣∘⊢⍟≢⟜𝔽𝕩 }

tab ⇐ @+9
cr  ⇐ @+10
