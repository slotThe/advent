⟨SplitF,Split⟩←•Import"../../util/bqn_util/util.bqn"
rs‿ids←•ParseFloat⚇1 '-'⊸Split⚇1 ⟨⟩SplitF •FLines "../inputs/day05.txt" ⋄ ids ⊑¨↩
!874= 0{id𝕊a: a+0{x‿y𝕊a:a∨x≤id∧id≤y}´rs}´ids
# Inclusion–Exclusion, keeping track of sign of the summands
c←⟨⟩{x‿y𝕊o: ⟨⟨x,y⟩‿+⟩∾∾{𝕊⟨a,b⟩‿s: ¬(y<a)∨b<x ? ⟨𝕩, ⟨⟨x⌈a,y⌊b⟩,-S⟩⟩ ; ⟨𝕩⟩}¨o }´rs
!348548952146313= 0{xy‿s𝕊n: n+S¬´⌽xy}´c

# Slightly shorter and faster, but less fun:
# c←0 ⋄ n←0 ⋄ {𝕊a‿b: m←n⌈b ⋄ c↩c+(a≤n)⊑⟨b¬a,m-n⟩ ⋄ n↩m }¨∧rs ⋄ !348548952146313=c
