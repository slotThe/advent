⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
a‿b ← <˘⍉>{ ⊑/ (⊑𝕩⊸∊)¨ "AX"‿"BY"‿"CZ" }¨¨ ' ' Split¨ •FLines "../inputs/day2.txt"
Part1 ← +´{ 1+𝕩+3×3|𝕩-𝕨-1 }
Part2 ← +´{ 1+(3|𝕩+𝕨-1)+3×𝕩 } # 𝕨 = op, 𝕩 = res
•Show (a Part1 b)‿(a Part2 b)
