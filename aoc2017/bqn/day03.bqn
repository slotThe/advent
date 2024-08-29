inp ← •ParseFloat ⊑ •FLines "../inputs/day03.txt"

# The pattern goes a bit like this:
#
#     R U 2L 2D 3R 3U 4L 4D 5R 5U…
Coords ← {
  dirs ← (2-˜2×𝕩)⥊⟨0‿1, 1‿0, 0‿¯1, ¯1‿0⟩ # r u l d
  natnat ← ∾1↓(↕⋈¨↕)𝕩 # 1 1 2 2 3 3 …
  ⟨0‿0⟩∾ +` ∾ natnat{⥊⋈˘ ∘‿2⥊ ¯1↓ 𝕩∾⍟⊢𝕨}¨dirs # expand
}
•Show +´| (inp-1) ⊑ Coords 1000 # magic number

# This is not an array algorithm in the slightest; sorry. It's what one would
# normally do: create a new grid, look for all neighbours, and then just add
# the values together. Without maps this is pretty inefficient, of course, but
# c'est la vie.
Neighbours ← { x‿y:
  n ← ⟨¯1, 0, 1⟩
  ⥊(x+n)⋈⌜(y+n)
}

Two ← {
  grid 𝕊 c:
    val ← 1⊑⊑grid # newest value
    val>inp? val;
  grid 𝕊 coords:   # grid = ⟨(x‿y)‿n, …⟩, coords = ⟨x‿y, …⟩
    p‿cs ← 1(↑⋈↓)coords ⋄ p ↩ ⊑p # point and rest
    n ← +´ 1⊑¨ ∾ {grid/˜ (𝕩⊸≡⊑)¨grid}¨ Neighbours p
    (⟨p‿n⟩∾grid)𝕊cs
}
•Show ⟨(0‿0)‿1⟩ Two 1↓(Coords 10) # magic number
