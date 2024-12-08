inp←>•FLines"../inputs/day08.txt"
_Solve ← { GetCoords _𝕣 grid:
  antennas ← ('.'⊸≠∧∊)⊸/∾<˘grid
  r‿c ← ≢grid
  ≠⍷∾{ 𝕊a:
   ps ← a(/○⥊⟜(↕≢)⍷)inp # All points for antenna a
   ∾{ 𝕊p‿np:
        { 𝕊x‿y: ⊑(y∊↕c)∧x∊↕r }¨⊸/ p GetCoords np  # Only take valid coords
   }¨ {𝕊p‿np: p≢np}¨⊸/ ⥊ps⋈⌜ps
  }¨antennas
}
{ p𝕊np: ⋈p-2×p-np }_Solve inp          # ⇒ 299
{ p𝕊np: {p-𝕩×p-np}¨1+↕≠inp }_Solve inp # ⇒ 1032
