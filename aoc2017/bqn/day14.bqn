⟨Split,_Fix⟩ ← •Import "../../util/bqn_util/util.bqn"
⟨KnotHash⟩ ← •Import "./day10.bqn"

grid ← {
  HexToBin ← {  𝕊p: p𝕊⟨⟩;              # Start
               0𝕊r: r∾˜0⥊˜4-≠r;        # End: pad with zeros
               p𝕊r: (⌊p÷2)𝕊(r∾˜2|p)    # Conversion
             }¨∘("0123456789abcdef"⊸⊐) # Convert hex to decimal, then to binary
  Start ← (⊑•FLines"../inputs/day14.txt")∾"-"∾•Fmt
  Row ← ∾∘HexToBin∘KnotHash
  >Row∘Start¨↕128
}

•Show +´⥊grid # ⇒ 8222

{ 𝕊g:
  g (+`⊸×⌾⥊)↩                          # reshape with unique numbers for all 1's
  g {(×𝕩)×⌈´⟨»˘,«˘,»,«,⊢⟩{𝕎𝕩}¨<𝕩}_Fix↩ # See [Note Game of Life]
  1-˜+´∊⥊g                             # Count all unique numbers minus 0
} grid # ⇒ 1086

# [Note Game of Life]
#
# This is actually a pretty cool idea, sort of stolen from a game of life way
# of thinking. The idea is to compare a live cell with each of its four
# neighbours. Since every live cell is associated to a unique number, always
# take the biggest one. For example,
#
#     m←3‿3⥊0‿0‿2‿1‿3‿0‿4‿0‿0
# ┌─
# ╵ 0 0 2
#   1 3 0
#   4 0 0
#         ┘
#    {(×𝕩)×(»˘𝕩)⌈(«˘𝕩)⌈(»𝕩)⌈(«𝕩)⌈𝕩}m
# ┌─
# ╵ 0 0 2
#   4 3 0
#   4 0 0
#         ┘
# and the fixed point then does its thing.
