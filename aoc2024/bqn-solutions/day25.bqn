⟨SplitF⟩ ← •Import "../../util/bqn_util/util.bqn"
keys‿locks ← ⟨»,«⟩{+˝'#'=𝕎𝕩}⚇0‿1 ('#'=⊑)¨⊸⊔ >¨ ⟨⟩SplitF•FLines"../inputs/day25.txt"
!3287≡ +´⥊ ∧´¨ 5≥ keys+⌜locks
•Show "Merry Christmas!"
