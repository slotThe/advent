inp←•ParseFloat∘(('+'=⊑)⊸↓)¨•FLines"../inputs/day01.txt"
+´inp    # One
{ Rec i: # Two
  b←¬∊a←+`i
  {𝕩=≠b? Rec i∾i; 𝕩⊑a}⊑b⊐1
} inp
