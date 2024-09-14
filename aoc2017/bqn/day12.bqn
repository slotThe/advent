⟨Split,_Fix,cr⟩ ← •Import "../../util/bqn_util/util.bqn"

P ← {
  h‿t ← 1((⊑↑)⋈1↓↓)' 'Split𝕩
  h (⋈○•BQN) ∾'⟨'∾t∾'⟩'
}
inp ← ⌽P¨•FLines"../inputs/day12.txt"

# What on earth is a graph? :]
GroupOf ← { # calculate the group of 𝕩 in 𝕨
  𝕨⊸{ 𝕩{ x‿cs 𝕊 seen: ⊑x∊seen? ⍷seen∾cs; 𝕩 }´𝕨 } _Fix 𝕩
}
•Show ≠ inp GroupOf ⟨0⟩

c ← 0
{ c +⟜1↩ ⋄ ((⊑¨𝕩)(¬∊)𝕩GroupOf⟨⊑¯1⊑𝕩⟩)/𝕩 } •_while_ (⟨⟩⊸≢) inp
•Show c
