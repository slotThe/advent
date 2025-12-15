⟨Ints,Split⟩←•Import"../../util/bqn_util/util.bqn"
inp ← { ⟨a←'#'=¯1↓1↓⊑𝕩, {1¨⌾(𝕩⊸⊏)0⥊˜≠a}∘Ints¨ ¯1↓1↓𝕩, Ints ¯1⊑𝕩⟩ }∘(' '⊸Split)¨ •FLines "../inputs/day10.txt"
!390= +´{𝕊g‿bs‿·: ⌊´≠¨ g⊸≡⟜(0⊸(≠´))¨⊸/ (1↓⥊↕(≠bs)⥊2)/¨<bs }¨inp # ≠ is XOR for bit vectors
!14677=14677 # Part 2 by chugging it into an SMT solver; see Haskell solution
