⟨SplitF,Ints,_Fix⟩ ← •Import "../../util/bqn_util/util.bqn"
inp←>•FLines"../inputs/day06.txt"
start←⟨⊑'^'(/○⥊⟜(↕≢)⍷)inp, ¯1‿0⟩

Turn ← { # Given direction 𝕩, turn 𝕨 steps to the right.
  ts ← ⟨¯1‿0, 0‿1, 1‿0, 0‿¯1⟩
  (⊑4|𝕨+ts⊐<𝕩)⊑ts
}

seen←•HashMap˜⟨⟩
path←⟨⟩
Sim ← {
  seen.Delete¨seen.Keys@
  start seen.Set@
  r ← (>𝕩){
    grid 𝕊 p‿d:
      seen.Has⟨p+d,d⟩? "LOOP";
    grid 𝕊 p‿d:
      np ← p+d
      { 𝕊@: @;
        𝕊'#': ⟨p, 1 Turn d⟩;
        𝕊·: ⟨np,d⟩ ⊣ ⟨np,d⟩seen.Set@
      }np⊑⎊@grid
  }•_while_{ ·𝕊"LOOP": 0; ·𝕊@: 0; 1 }start
  r ⊣ path↩⍷⊑¨seen.Keys@
}
Sim <inp
≠path                                               # ⇒ 4758
≠"LOOP"⊸≡¨⊸/Sim¨{<'#'⌾(𝕩⊸⊑)inp}¨((⊑start)⊸≢¨⊸/)path # ⇒ 1670
