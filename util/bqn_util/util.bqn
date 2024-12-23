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

# Indices of all 1's in the array 𝕩.
#
#     Ix >⟨⟨1,0,1⟩,⟨0,0,1⟩,⟨0,0,1⟩⟩
# ⟨ ⟨ 0 0 ⟩ ⟨ 0 2 ⟩ ⟨ 1 2 ⟩ ⟨ 2 2 ⟩ ⟩
#     Ix 4≥ 3‿3⥊↕9
# ⟨ ⟨ 0 0 ⟩ ⟨ 0 1 ⟩ ⟨ 0 2 ⟩ ⟨ 1 0 ⟩ ⟨ 1 1 ⟩ ⟩
Ix ⇐ /○⥊⟜↕⟜≢

# Combine hash-maps and apply 𝔽 to duplicate keys.
_merge ⇐ { W _𝕣 hms:
  c ← •HashMap˜⟨⟩
  { 𝕊hm:
    { 𝕊k‿v: c.Has k? k c.Set (v W c.Get k);
      𝕊k‿v: k c.Set v
    }¨ (hm.Keys@)⋈¨(hm.Values@)
  }¨hms
  c
}

tab ⇐ @+9
lf  ⇐ @+10
