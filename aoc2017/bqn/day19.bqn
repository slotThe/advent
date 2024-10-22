inp ← {(' '⥊˜≠𝕩)∾⌽⍉𝕩}⍟4 >•FLines"../inputs/day19.txt"

ND ← { pos 𝕊 dir: # Compute new direction for pos going in dir.
  ds ← (<¯1×dir)(¬∘⍷/⊢)⟨1‿0, 0‿1, ¯1‿0, 0‿¯1⟩ # Not where we came from
  ds/˜' '≠inp⊑˜ds+<pos                        # Not empty space
}

path ← {
  ·‿pth‿pos ← { 𝕊dir‿pth‿pos:
      {' '=inp⊑˜dir+pos? dir(⊑pos⊸ND)↩;@} # Change direction if necessary
      ⟨dir, pth∾pos⊑inp, pos+dir⟩
  }•_while_{
    𝕊dir‿·‿pos: 0< ≠pos ND dir   # While we can move somewhere
  } ⟨1‿0, "", ⟨1, ⊑'|'⊐˜1⊏inp⟩⟩
  pth∾pos⊑inp                    # Add last position to final path
}

•Show ∊⟜('A'+↕26)⊸/ path # ⇒ "RUEDAHWKSM"
•Show ≠path              # ⇒ 17264
