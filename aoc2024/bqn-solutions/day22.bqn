⟨Ints⟩ ← •Import "../../util/bqn_util/util.bqn"

BV  ← { 𝕊p: p𝕊⟨⟩; 0𝕊r: r«64⥊0; p𝕊r: (⌊p÷2)𝕊(r∾˜2|p) } # To bit-vector
Rst ← { 𝕩⥊⟜0⊸»𝕨 } # Right shift of bit-vector 𝕨 by amount 𝕩
Lst ← { «⍟𝕩 𝕨 }   # Left  shift of bit-vector 𝕨 by amount 𝕩
XOR ← ≠

prune ← BV 16777215
Step ← { 𝕊n:
  n ↩ prune ∧ n XOR n Lst 6
  n ↩ prune ∧ n XOR n Rst 5
  n ↩ prune ∧ n XOR n Lst 11  # Returns n when done
}

inp ← BV¨ Ints •FChars"../inputs/day22.txt"
steps ← (2⊸×⊸+˜´⌽)¨¨ Step⍟(1+↕2000)¨ inp
!13584398738≡ +´ ¯1⊸⊑¨ steps

# Combine hash-maps and apply 𝔽 to duplicate keys.
_merge ← { W _𝕣 hms:
  { 𝕊hm:
    { 𝕊k‿v: k c.Set (v W 0 c.Get k) }¨ (hm.Keys@)⋈¨(hm.Values@)
  }¨hms
  c
}

two ← +_merge {
  hm ← •HashMap˜⟨⟩
  (4↓ps) { i𝕊w: ¬hm.Has w? w hm.Set i;@ }¨ <˘4↕   1↓-⟜»   ps←10|𝕩
  # assocs | add only first occurrence   | wins | diffs | prices
  hm
}¨steps
!1612≡ ⌈´two.Values@
