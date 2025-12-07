inp←>•FLines"../inputs/day07.txt" ⋄ s←inp∊"^S" ⋄ st←inp∊"S"
one←0 ⋄ two←+´⥊ {one+↩+´×¨⥊n←s∧sx←»𝕩 ⋄ (sx∧¬s)+(»˘n)+«˘n}⍟(1-˜≠s) st
!1681=one ⋄ !422102272495018=two
