⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
P ← {
  a‿"s": (-•ParseFloat a)⊸⌽;                # sA
  a‿"x": ⌽⌾((•ParseFloat¨'/'Split a)⊸⊏);    # xA/B
  a‿"p": { 𝕊xs: ⌽⌾((xs⊐∾'/'Split a)⊸⊏) xs } # pA/B
}
inp ← ⌽ P∘(1⊸(↓⋈↑))¨ ','Split ⊑•FLines"../inputs/day16.txt"

('a'+↕16){𝕎𝕩}´inp # ⇒ "eojfmbpkldghncia"

h  ← ⟨⟩•HashMap⟨⟩
d  ← {𝕎𝕩}´⟜inp•_while_{ (𝕩h.Set@) ⊢ ¬h.Has𝕩 } ('a'+↕16) # dup
di ← ⊑(h.Keys@)⊐<d                                      # dup index
{𝕎𝕩}´⟜inp⍟(di+(h.Count@)|10e9-di) d # ⇒ "iecopnahgdflmkjb"
