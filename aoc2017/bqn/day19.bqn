inp ← ⌽⍉∘({(' '⥊˜≠𝕩)∾⌽⍉𝕩}⍟3) >•FLines"../inputs/day19.txt"

ND ← { pos 𝕊 dir: # Compute new direction for pos going in dir.
  ds ← (<¯1×dir)(¬∘⍷/⊢)⟨1‿0, 0‿1, ¯1‿0, 0‿¯1⟩ # Not where we came from
  ds/˜' '≠inp⊑˜ds+<pos                        # Not empty space
}

path ← {
  ·‿pth‿pos ← {
    𝕊dir‿pth‿pos:                  # Continue case
      np ← dir+pos
      ' '≠np⊑inp? ⟨dir, pth∾pos⊑inp, np⟩;
    𝕊dir‿pth‿pos:                  # Change direction case
      dir ↩ ⊑ pos ND dir
      ⟨dir, pth∾pos⊑inp, pos+dir⟩
  } •_while_ {
    𝕊dir‿pth‿pos: 0≠ ≠pos ND dir   # We can move somewhere
  } ⟨1‿0, "", ⟨0, ⊑(⊏inp)⊐'|'⟩⟩
  pth∾pos⊑inp                      # Add last position to final path
}

•Show ∊⟜('A'+↕26)⊸/ path # ⇒ "RUEDAHWKSM"
•Show ≠path              # ⇒ 17264
