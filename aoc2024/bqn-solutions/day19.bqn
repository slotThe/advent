⟨SplitF, Split⟩ ← •Import "../../util/bqn_util/util.bqn"
⟨towels⟩‿ptns ← ⟨⟩ SplitF •FLines"../inputs/day19.txt"
towels ↩ (' '=⊑)⊸↓¨ ','Split towels

memo ← ⟨""⟩•HashMap⟨1⟩
A ← { # Arrangements of towels for pattern 𝕩
  memo.Has 𝕩? memo.Get 𝕩;
  𝕊ptn: +´ { r ⊣ p memo.Set r←A p←(≠𝕩)↓ptn }¨ { 𝕩≡(≠𝕩)↑ptn }¨⊸/towels
}

!311≡ +´ 0< A¨ptns
!616234236468263≡ +´A¨ptns
