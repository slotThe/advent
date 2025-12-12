⟨Ints,SplitF⟩←•Import"../../util/bqn_util/util.bqn"
ps‿gs ← 6(↑⋈(⊑↓)) ⟨⟩ SplitF •FLines "../inputs/day12.txt" ⋄ gs 2⊸(↑⋈↓)∘Ints¨↩ # Pieces and grids
sp←{+´∾'#'=1↓𝕩}¨ps ⋄ p1←+´{𝕊a‿r: (×´a)≥+´r×n}¨gs ⋄ !474=p1  # Available and required board space
