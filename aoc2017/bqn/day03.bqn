inp ← •ParseFloat ⊑ •FLines "../inputs/day03.txt"

# The pattern goes a bit like this:
#
#     R U 2L 2D 3R 3U 4L 4D 5R 5U…
Coords ← { # 2×𝕩: Number of steps to go.
  dirs ← (2×𝕩)⥊⟨0‿1, 1‿0, 0‿¯1, ¯1‿0⟩ # r u l d
  natnat ← ∾(↕⋈¨↕)𝕩                   # 0 0 1 1 2 2 3 3 …
  +`∾natnat{<˘𝕨‿2⥊𝕩}¨dirs             # expand
}
+´| (inp-2) ⊑ Coords 1000 # ⇒ 552

# This is not an array algorithm in the slightest; sorry. It's what one would
# normally do: create a new grid, look for all neighbours, and then just add
# the values together. C'est la vie.

N ← { x‿y: ⥊(x+⟨¯1, 0, 1⟩)⋈⌜(y+⟨¯1, 0, 1⟩) }
Sim ← { # 𝕨 = grid = (x‿y)→n, …; 𝕩 = coords = ⟨x‿y, …⟩
  (¯1⊑𝕨.Values@)>inp?           # Newest value larger than input?
    ¯1⊑𝕨.Values@;
  grid 𝕊 coords:
    p‿cs ← 1(↑⋈↓)coords ⋄ p⊑↩   # Point and rest
    p grid.Set +´0⊸grid.Get¨N p # Get neighbour sums and set for point
    grid 𝕊 cs                   # Recurse
}
•Show {h←⟨⟩•HashMap⟨⟩ ⋄ 0‿0 h.Set 1 ⋄ h} Sim Coords 10 # ⇒ 330785
