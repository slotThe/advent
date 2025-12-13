⟨Ints,SplitF⟩←•Import"../../util/bqn_util/util.bqn"
a‿r ← <˘⍉> 2⊸(↑⋈↓)∘Ints¨ ⊑6↓ ⟨⟩ SplitF •FLines "../inputs/day12.txt"
!474= +´(×´¨⌊3÷˜a)≥(+´¨r) # Non-overlapping ok or too small only
