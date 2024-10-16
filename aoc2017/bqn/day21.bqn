⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
P ← { # Parse a line into from‿to pairs
  f‿t ← '='Split𝕩 ⋄ f ¯1⊸↓↩ ⋄ t 2⊸↓↩
  >∘(".#"⊸⊐¨)∘('/'⊸Split)¨ f‿t
}
E ← { 𝕊 f‿t: (⍷⥊⟨⌽˘,⊢⟩{𝕎𝕩}⌜(⍉⌽)⍟(↕4)f)⋈¨<t } # Expand to all of D₄
inp ← ∾ E∘P¨ •FLines "../inputs/day21.txt"

Widen ← { k‿substs 𝕊 grid: # k, k×k → (k+1)×(k+1) substitutions, grid
  ∾{ 𝕊sg: # A k×k submatrix of `grid'
    1⊑⊑ (sg≡⊑)¨⊸/substs
  }¨ (2⥊<⌊k÷˜↕≠grid)⊔grid
}
Sim ← {
  0=2|≠𝕩? ⟨2, {2=≠⊑𝕩}¨⊸/inp⟩ Widen 𝕩;
  0=3|≠𝕩? ⟨3, {3=≠⊑𝕩}¨⊸/inp⟩ Widen 𝕩
}

start ← >".#"⊸⊐¨⟨".#.", "..#", "###"⟩
+´⥊ Sim⍟5  start # ⇒ 155
+´⥊ Sim⍟18 start # ⇒ 2449665
