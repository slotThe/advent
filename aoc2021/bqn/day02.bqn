⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
input ← {d‿v: d‿(•ParseFloat v)}∘(' '⊸Split)¨ •FLines "../haskell/puzzle-input/day2.txt"
ToCoord ← { ⊑ 𝕩/˜ (<𝕨)⍷"forward"‿"up"‿"down" }

Parse ← { d‿v: d ToCoord (0‿1)‿(¯1‿0)‿(1‿0)×v }
•Show { h‿d ← +´Parse¨input ⋄ h×d }

Parse2 ← { d‿v:
  d ToCoord { h‿d‿a: ⟨v+h, d+a×v, a⟩ }‿{ h‿d‿a: ⟨h, d, a-v⟩ }‿{ h‿d‿a: ⟨h, d, a+v⟩ }
}
•Show {
  h‿d‿a ← { two ← {𝕏∘𝕎}´Parse2¨input ⋄ Two 0‿0‿0 }
  h×d
}
