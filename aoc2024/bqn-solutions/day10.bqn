inp ← '0'-˜>•FLines"../inputs/day10.txt"
zeros ← /○⥊⟜↕⟜≢inp=0  # Indices of 0's

One ← {(inp=𝕨)×∨´⟨»˘,«˘,»,«⟩{𝕎𝕩}¨<𝕩}´⟜(⌽1+↕9)
!629≡+´⥊+´One∘{1⌾(𝕩⊸⊑)0⥊˜≢inp}¨zeros

!1242≡+´⥊(inp=0){(inp=𝕨)×+´⟨»˘,«˘,»,«⟩{𝕎𝕩}¨<𝕩}´⌽1+↕9
