⟨Split, cr⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← •FLines "../inputs/day07.txt"
inp{ # Parse input
  ws ← ' ' Split 𝕩
  (⊑ws)‿(•BQN 1⊑ws)‿((-(','⊸=)∘(¯1⊸↑))⊸↓¨3↓ws) # name‿weight‿children
}¨↩

•Show root ← ⊑ (⊑¨inp)(¬∘∊/⊣)(∾¯1⊑¨inp) # ⇒ "svugo"

1↓⊑((<"svugo")≡¨⊑¨inp)/inp

h ← ⟨⟩•HashMap⟨⟩ ⋄ { n‿w‿ch: n h.Set w‿ch }¨inp # name -> weight, children
M ← =´2↑⊑¨ # Do the first two elements have matching cumulative weights?
SpotOutlier ← {
  w‿ch ← h.Get 𝕩
  sums ← ∧{w‿wc: ⟨w+wc,w⟩}¨ 𝕊¨ch # Sort by ascending w+wc
  { 𝕊⟨⟩: w‿0;                    # No children
    𝕊s : M s? M⌽s?               # All child nodes's weights match
      ⟨w, (≠sums)×⊑⊑s⟩;
    𝕊s : M s?                    # Last weight is higher than the [o]ther[w]eights
      wwc‿mw←¯1⊑s ⋄ ow‿·←  ⊑s ⋄ (mw-wwc-ow)!0;
    𝕊s : M⌽s?                    # First weight is lower than the [other][w]eights
      wwc‿mw←  ⊑s ⋄ ow‿·←¯1⊑s ⋄ (mw-wwc-ow)!0
  } sums
}
•Show SpotOutlier⎊•CurrentError root # ⇒ 1152
