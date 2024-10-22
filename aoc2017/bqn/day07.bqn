⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← {
  ws ← ' ' Split 𝕩
  ⟨ ⊑ws, •BQN 1⊑ws, ','Split∾3↓ws ⟩ # name‿weight‿children
}¨ •FLines"../inputs/day07.txt"

•Show root ← ⊑ (⊑¨inp){(¬𝕨∊𝕩)/𝕨}(∾¯1⊑¨inp) # ⇒ "svugo"

h ← ⟨⟩•HashMap⟨⟩ ⋄ { n‿w‿ch: n h.Set w‿ch }¨inp # name -> weight, children
M ← =´2↑⊑¨ # Do the first two elements have matching cumulative weights?
SpotOutlier ← {
  w‿ch ← h.Get 𝕩
  sums ← ∧𝕊¨ch                   # Sort by ascending w+wc
  { 𝕊⟨⟩: w‿w;                    # No children
    𝕊s : M s? M⌽s?               # All child nodes's weights match
      ⟨w++´⊑¨s, w⟩;
    𝕊s : M s?                    # Last weight is higher than the [o]ther[w]eights
      wwc‿mw←¯1⊑s ⋄ ow‿·←  ⊑s ⋄ (mw-wwc-ow)!0;
    𝕊s : M⌽s?                    # First weight is lower than the [o]ther[w]eights
      wwc‿mw←  ⊑s ⋄ ow‿·←¯1⊑s ⋄ (mw-wwc-ow)!0
  } sums
}
•Show SpotOutlier⎊•CurrentError root # ⇒ 1152
