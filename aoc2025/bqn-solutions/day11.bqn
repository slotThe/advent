⟨Split⟩←•Import"../../util/bqn_util/util.bqn"
h‿t ← <˘⍉> ((¯1↓⊑)⋈1⊸↓)∘(' '⊸Split)¨ •FLines "../inputs/day11.txt" # head, tail
g←⍷h∾∾t ⋄ I←{⊑g⊐<𝕩} ⋄ t I¨¨↩ ⋄ h I¨↩   # global, index, turn strings to indices

hm ← •HashMap˜⟨⟩               # memo
P ← { ds𝕊s‿e: s‿e I¨↩          # Start, end, devices to include
  { 𝕊n‿md‿x: (∧´1⊑¨md)∧x=e? n; # Number of paths, mask of req devices, cur node
    hm.Has 𝕩? hm.Get 𝕩;
    𝕊n‿md‿x: r ⊣ 𝕩 hm.Set r←+´ 𝕊∘{⟨1,{𝕊i‿y:⟨i,y∨i=x⟩}¨md,𝕩⟩}¨ x⊑⎊⟨⟩t # Get neighbours and recurse
  } ⟨0, I⊸⋈⟜0¨ds, s⟩
}
!511            = ⟨⟩          P "you"‿"out"
!458618114529380= "fft"‿"dac" P "svr"‿"out"
