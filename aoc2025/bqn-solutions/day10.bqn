⟨Ints,Split⟩←•Import"../../util/bqn_util/util.bqn"
inp ← { N←2⊸×⊸+˜´ ⋄ ⟨N'#'=¯1↓1↓⊑𝕩, N∘{1¨⌾(𝕩⊸⊏)32⥊0}∘Ints¨ ¯1↓1↓𝕩, Ints ¯1⊑𝕩⟩ }∘(' '⊸Split)¨ •FLines "../inputs/day10.txt"
XOR ← {0(⊑32‿32•bit._xor○⋈)´𝕩}
!390= +´{𝕊g‿bs‿·: ⌊´∘(≠¨) (g=XOR¨)⊸/ (⥊↕2⥊˜≠bs)/¨<bs }¨inp
!14677=14677 # Part 2 by chugging it into an SMT solver; see Haskell solution
