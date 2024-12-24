⟨Split,SplitF⟩ ← •Import "../../util/bqn_util/util.bqn"
P ← { # "pbm OR djm -> kpj" → ⟨"kpj", "↩", "pbm", "∨", "djm"⟩
  from ← ⟨"OR", "AND", "XOR", "->"⟩
  to   ← ⟨"∨" , "∧"  , "≠"  , "↩" , ⊢⟩
  {⊑from⊐<𝕩}◶to¨ ⌽⌾(0‿1⊸⊏) ¯2⌽ ' 'Split𝕩
}
defs‿ins ← { # Register definitions, and actual instructions.
  defs‿ins ← ⟨⟩SplitF •FLines"../inputs/day24.txt"
  ⟨ {𝕊a‿b:a‿"←"‿b}¨':'Split¨defs , P¨ins ⟩
}

IC ← { ¯1↓∾∾⟜𝕨¨𝕩 }          # Intercalate: combine with a separator
outs ← ∾⟨0,2,4⟩⊸⊏¨ins       # All registers
!53190357879014≡ 2⊸×⊸+˜´⌽ •BQN "⋄"IC⟨
  "_fix←{𝔽∘⊢⍟≢⟜𝔽_𝕣∘⊢⍟≢⟜𝔽𝕩}"
  ("←0⋄")IC ⍷(⊑¨defs)∾outs  # Initially set all registers to 0.
  "{𝕊·:"                    # Start of fixed point computation
    "⋄"IC ∾¨defs            # Re-set all explicit definitions in each loop.
    "⋄"IC ∾¨ins             # Instructions.
    "‿"IC ∨ {'z'=⊑𝕩}¨⊸/outs # All z's
  "}_fix@"                  # Do the fixed-point computation.
⟩

# Using graphviz to manually repair the adder!
","IC ∧⟨"tdv", "z16", "z23", "bks", "z09", "hnd", "nrn", "tjp"⟩
