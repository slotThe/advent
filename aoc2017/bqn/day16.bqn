⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
P ← {
  (1↓𝕩) {
    a𝕊's': (-•ParseFloat a)⊸⌽;                 # sA
    a𝕊'x': ⌽⌾((•ParseFloat¨'/'Split a)⊸⊏);     # xA/B
    a𝕊'p': { 𝕊xs: ⌽⌾((xs⊐∾'/'Split a)⊸⊏) xs }  # pA/B
  } ⊑𝕩
}
inp ← ⌽P¨','Split⊑•FLines"../inputs/day16.txt"

('a'+↕16){𝕎𝕩}´inp # ⇒ "eojfmbpkldghncia"

h  ← ⟨⟩•HashMap⟨⟩
d  ← {𝕎𝕩}´⟜inp•_while_{ b←¬h.Has𝕩 ⋄ 𝕩 h.Set 1 ⋄ b } ('a'+↕16)
dn ← ⊑(h.Keys@)⊐<d
{𝕎𝕩}´⟜inp⍟(dn+(h.Count@)|10e9-dn) d # ⇒ "iecopnahgdflmkjb"
