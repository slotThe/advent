⟨Split,_Fix⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← (⊑⋈2⊸↓)∘(•BQN¨)∘(' '⊸Split)¨•FLines"../inputs/day12.txt"

# Calculate the group of the first element of as = ⟨⟨el, children⟩, …⟩
FGroup ← { 𝕊as: # What on earth is a graph? :]
   { a‿cs 𝕊 seen: ⊑a∊seen? ⍷seen∾cs; 𝕩 }´⟜as _Fix ⟨⊑⊑as⟩
}

≠FGroup inp  # ⇒ 380

c ← 0
{ c +⟜1↩ ⋄ (⊑¨⊸(¬∊)/⊣)⟜FGroup 𝕩 } •_while_ (⟨⟩⊸≢) inp
c            # ⇒ 181
