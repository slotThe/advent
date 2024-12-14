inp ← '0'-˜ ⊑•FLines"../inputs/day09.txt"

Free ← 1⊸=⊑
File ← 0⊸=⊑
Muv ← { 𝕊grid:
  n ← 1-˜≠grid
  fs ← ≠File¨⊸/grid # Total files
  grid ⊣ { 𝕊·:
    fr ←   ⊑1⊐˜Free¨grid
    fi ← n-⊑1⊐˜File¨⌽grid
    grid ⌽⌾(fr‿fi⊸⊏)↩
  }•_while_{
    𝕊·: ∨´Free¨fs↑grid
  }@
}
!6259790630969≡+´ 1⊸⊑⊸×¨⟜(↕∘≠) File¨⊸/ Muv ∾{(2|𝕩)⊸∾¨𝕨⥊⌊𝕩÷2}¨⟜(↕∘≠)inp

files‿frees ← <˘⍉ ∘‿2⥊ (0∾+`inp)⋈¨(inp∾0)
!6289564433984≡+´{ 𝕊id:
  subt‿size ← id⊑files
  { 𝕊j: id≠j?          # found place
      subt ↩ ⊑j⊑frees
      frees ⟨size,-size⟩⊸+⌾(j⊸⊑)↩;
    𝕊·:@               # not moving
  }⊑1⊐˜ size⊸≤∘(1⊸⊑)¨ id↑frees
  id×(¯1+size+2×subt)×size÷2
}¨⌽↕≠files
