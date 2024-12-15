⟨Ix⟩ ← •Import "../../util/bqn_util/util.bqn"
inp←>•FLines"../inputs/day06.txt"
start←⟨⊑Ix'^'=inp, ¯1‿0⟩

Turn ← { # Given direction 𝕩, turn 𝕨 steps to the right.
  ts ← ⟨¯1‿0, 0‿1, 1‿0, 0‿¯1⟩
  (⊑4|𝕨+ts⊐<𝕩)⊑ts
}
IB ← { r‿c 𝕊 x‿y: (y<r)∧(y≥0)∧(x<c)∧x≥0 }
Sim ← {
  hm←⟨start⟩•HashMap⟨@⟩
  rc ← ≢>𝕩
  ret ← (>𝕩){
    grid 𝕊 p‿d:
      hm.Has⟨p+d,d⟩? "LOOP";
    grid 𝕊 p‿d:
      np ← p+d
       rc⊸IB◶⟨
       @                             # falling off
       { 𝕊'#': ⟨p, 1 Turn d⟩;        # hitting head
         𝕊·: np‿d ⊣ ⟨np,d⟩hm.Set@    # continuing
       }⊑⟜grid
       ⟩ np
  }•_while_{ ·𝕊"LOOP": 0; ·𝕊@: 0; 1 }start
  ⟨ret, ⍷⊑¨hm.Keys@⟩
}

path ← 1⊑Sim <inp
!4758≡≠path
!1670≡≠"LOOP"⊸≡¨⊸/⊑∘Sim¨{<'#'⌾(𝕩⊸⊑)inp}¨path
