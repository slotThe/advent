⟨Ints⟩←•Import "../../util/bqn_util/util.bqn"
inp←Ints¨•FLines"../inputs/day09.txt"
A←{a‿b𝕊c‿d: (1+|b-d)×1+|a-c} ⋄ !4758598740=⌈´⥊A⌜˜inp

es←⋈¨⟜(1⊸⌽)inp ⋄ rs←∨⥊{⟨𝕨A𝕩,𝕩,𝕨⟩}⌜˜inp  # Edges, rectangles
Out ← { ⟨x1‿y1,x2‿y2⟩ 𝕊 ⟨h1‿v1, h2‿v2⟩: # Edge outside of rect bounds?
  ((y1⌊y2)≥v1⌈v2) ∨ ((y1⌈y2)≤v1⌊v2) ∨ ((x1⌊x2)≥h1⌈h2) ∨ (x1⌈x2)≤h1⌊h2
}
i←¯1 ⋄ f←0 ⋄ {𝕊·: ∧´(1↓i⊑rs)⊸Out¨es? f↩1; @}•_while_{𝕊·: i+↩1 ⋄ ¬f}@
!1474699155=⊑i⊑rs
