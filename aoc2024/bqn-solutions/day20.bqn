⟨Ix,_Fix⟩ ← •Import "../../util/bqn_util/util.bqn"
inp‿s‿e ← {
  inp ← >•FLines"../inputs/day20.txt"
  s←⊑Ix'S'=inp ⋄ e←⊑Ix'E'=inp
  inp 1¨⌾(s‿e⊸⊑)∘("#."⊸⊐)↩
  inp‿s‿e
}
ixs    ← inp /○⥊ ↕nm←≢inp     # Only the 1's
n      ← -≠ixs
grid   ← n⌾(e⊸⊑) nm⥊0
moves  ← (××n⊸-) { inp× 𝕩⌊ ⌊´⟨»˘,«˘,»,«⟩{𝕎𝕩+1}¨<𝕩 }_Fix grid
dists  ← +´∘|∘-⌜˜ ixs
cheats ←      -⌜˜ ixs⊑moves
!1422‿1009299≡ { +´ 100≤ (dists≤𝕩) /○⥊ cheats - dists }¨2‿20
