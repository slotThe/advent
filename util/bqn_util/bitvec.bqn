TryNum ← { 0=•Type𝕩? ToNum𝕩; 𝕩 }

BV    ⇐ { 𝕊p: p𝕊⟨⟩; 0𝕊r: r«64⥊0; p𝕊r: (⌊p÷2)𝕊(r∾˜2|p) } # Number to bit vector
ToNum ⇐ 2⊸×⊸+˜´⌽                                        # Bit vector to number

# Right shift of bit-vector 𝕨 by amount 𝕩; if 𝕩 is a bit vector, convert it to
# a value beforehand.
Rst ⇐ { (TryNum 𝕩)⥊⟜0⊸»𝕨 }

# Left shift of bit-vector 𝕨 by amount 𝕩; if 𝕩 is a bit vector, convert it to
# a value beforehand.
Lst ⇐ { «⍟(TryNum 𝕩) 𝕨 }

XOR ⇐ ≠

# Small numbers
b0 ⇐ BV 0
b1 ⇐ BV 1
b2 ⇐ BV 2
b3 ⇐ BV 3
b4 ⇐ BV 4
b5 ⇐ BV 5
b6 ⇐ BV 6
b7 ⇐ BV 7
b8 ⇐ BV 8
b9 ⇐ BV 9
