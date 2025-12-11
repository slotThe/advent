⟨Split⟩←•Import"../../util/bqn_util/util.bqn"
h‿ns ← <˘⍉> ((¯1↓⊑)⋈1⊸↓)∘(' '⊸Split)¨ •FLines "../inputs/day11.txt" # head, neighbours
g←⍷h∾∾ns ⋄ I←{⊑g⊐<𝕩} ⋄ ns I¨¨↩ # global, index, turn strings to indices

hm ← •HashMap˜⟨⟩             # memo
P ← { ds𝕊s‿e: s‿e I¨↩        # Start, end, devices to include
  { 𝕊md‿v: (∧´1⊑¨md)∧v=e? 1; # Mask of req devices, cur node
    hm.Has 𝕩? hm.Get 𝕩;
    𝕊md‿v: r ⊣ 𝕩 hm.Set r←+´𝕊∘{⟨{𝕊i‿y:⟨i,y∨i=v⟩}¨md,𝕩⟩}¨v⊑⎊⟨⟩ns # Get neighbours and recurse
  } ⟨I⊸⋈⟜0¨ds, s⟩
}
!511            = ⟨⟩          P "you"‿"out"
!458618114529380= "fft"‿"dac" P "svr"‿"out"
