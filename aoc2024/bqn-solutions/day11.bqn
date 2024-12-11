⟨Ints⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← Ints •FChars"../inputs/day11.txt"

B ← {
  𝕊0: ⟨1⟩;
  𝕊n: 0=2|≠s←•Repr n? •ParseFloat¨(2÷˜≠s)(↑⋈↓)s;
  ⟨𝕩×2024⟩
}
Bs ← { 𝕊hm:
  v←⟨⟩
  { 𝕊k: c←hm.Get k ⋄ v∾↩<k⋈-c ⋄ {v∾↩<𝕩‿c}¨B k }¨hm.Keys@  # Collect changes
  { 𝕊k‿c: k hm.Set c+0 hm.Get k }¨v                       # Make changes
  { 𝕊k: 0=hm.Get k? hm.Delete k; @ }¨hm.Keys@             # Clean (not necessary, but faster)
  hm
}
!239714≡+´(Bs⍟25 inp •HashMap 1⥊˜≠inp).Values@
!284973560658514≡+´(Bs⍟75 inp •HashMap 1⥊˜≠inp).Values@
