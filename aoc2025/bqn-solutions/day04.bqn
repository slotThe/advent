⟨_Fix⟩ ← •Import"../../util/bqn_util/util.bqn"
i ← >'@'=•FLines"../inputs/day04.txt"
D ← { 𝕩×4>+´⟨»,«,«˘,»˘,»˘«,«˘«,«˘»,»˘»⟩{𝕎𝕩}¨<𝕩 }
!1578 =+´⥊D i
!10132=+´⥊i-(-⟜D)_Fix i

# Fun visualisation:
# ⟨_showgrid⟩ ← •Import"../../util/bqn_util/util.bqn"
# !10132=+´⥊i-((-⟜D)_showgrid)_Fix i
