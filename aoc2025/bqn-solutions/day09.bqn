⟨Ints⟩←•Import "../../util/bqn_util/util.bqn"
xy ← <˘⍉> Ints¨•FLines"../inputs/day09.txt" # ⟨ x's, y's ⟩

ax‿ay←⌈⌜˜¨xy ⋄ bx‿by←⌊⌜˜¨xy
!4758598740= ⌈´⥊g←(1+ay-by)×(1+ax-bx)

⟨lx,ux⟩‿⟨ly,uy⟩ ← (⌊⋈⌈)⟜(1⊸⌽)¨ xy # edges
!1474699155= ⌈´⥊ g∧ ∧˝⚇1(uy≤⌜by)∨(ly≥⌜ay)∨(ux≤⌜bx)∨(lx≥⌜ax) # all edges outside?
