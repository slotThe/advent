⟨Ix⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← '0'-˜>•FLines"../inputs/day10.txt"

One ← {(inp=𝕨)×∨´⟨»˘,«˘,»,«⟩{𝕎𝕩}¨<𝕩}´⟜(⌽1+↕9)
!629≡+´⥊+´One∘{1⌾(𝕩⊸⊑)0⥊˜≢inp}¨ Ix inp=0

!1242≡+´⥊(inp=0){(inp=𝕨)×+´⟨»˘,«˘,»,«⟩{𝕎𝕩}¨<𝕩}´⌽1+↕9
