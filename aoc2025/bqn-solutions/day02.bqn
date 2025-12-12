⟨Split,Ints⟩←•Import"../../util/bqn_util/util.bqn" ⋄ P←•ParseFloat ⋄ R←•Repr
inp ← ⊑∘Ints¨¨ '-'⊸Split¨ ','Split ⊑•FLines "../inputs/day02.txt"
Iv ← { n𝕊s‿e: # Invalids between s and e of repeat n
  rs←R s ⋄ Succ←{P∾(R𝕩)¨↕n} ⋄ h←rs↑˜⌊n÷˜≠rs
  ⍷ s⊸≤⊸/ ⊑{𝕊xs‿i:⟨xs∾<Succ i, i+1⟩}•_while_{𝕊·‿i: e≥Succ i} ⟨⟨⟩,P⎊1 h⟩
}
!54234399924=+´∾ 2⊸Iv¨inp
Div←{1↓(1+↕)⊸(⊣/˜0=|)≠R𝕩} ⋄ !70187097315=+´{+´⍷∾(⍷∾Div¨𝕩)Iv¨<𝕩}¨inp
