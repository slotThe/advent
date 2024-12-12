⟨Ints,_Fix⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← '@'-˜ >•FLines"../inputs/day12.txt" # '@'='A'-1
ts ← ⟨»˘,«˘,»,«⟩ # turns: right‿left‿down‿up

# Same idea as 2017/16, but more complicated.
regions ← { 𝕊p: p⌈´({inp=𝕏inp}¨ts){𝕨×𝕏p}¨ts }_Fix (+`⊸×⌾⥊)inp

Perim ← {𝕊r: +´{+´⥊r∧¬𝕏r}¨ts }
+´{ (Perim r) × +´⥊r←𝕩=regions }¨⍷⥊regions

Corners ← { 𝕊r: +´(2⌽ts){ +´⥊ e∧¬𝕎e←r∧¬𝕏r }¨ts }
+´{ (Corners r) × +´⥊r←𝕩=regions }¨⍷⥊regions
