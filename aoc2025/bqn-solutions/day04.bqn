⟨_Fix⟩ ← •Import"../../util/bqn_util/util.bqn"
i ← >'@'=•FLines"../inputs/day04.txt"
D ← { 𝕩×4>+´⟨»,«,«˘,»˘,»˘«,«˘«,«˘»,»˘»⟩{𝕎𝕩}¨<𝕩 }
!1578=+´⥊D i
r←0 ⋄ {r(+´⥊m←D𝕩)⊸+↩ ⋄ 𝕩-m}_Fix i ⋄ !10132=r
