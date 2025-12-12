⟨Ints,SplitF⟩←•Import"../../util/bqn_util/util.bqn"
ps‿gs ← 6(↑⋈(⊑↓)) ⟨⟩ SplitF •FLines "../inputs/day12.txt" # Pieces and grids
a‿r←<˘⍉>2⊸(↑⋈↓)∘Ints¨gs ⋄ sp←{+´∾'#'=1↓𝕩}¨ps # Available and required space, space of pieces
!474= +´ (×´¨a) ≥ +´¨r×<sp
