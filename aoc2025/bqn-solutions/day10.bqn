⟨Ints,Split⟩←•Import"../../util/bqn_util/util.bqn"
P ← { d𝕊i: a←'#'=¯1↓1↓⊑i ⋄ c←Ints ¯1⊑i ⋄ b←{1¨⌾(𝕩⊸⊏)0⥊˜≠d⊑⟨a,c⟩}∘Ints¨¯1↓1↓i ⋄ ⟨a,b,c⟩ } # Parse
inp ← ' '⊸Split¨•FLines "../inputs/day10.txt"

!390= +´{𝕊g‿bs‿·: ⌊´≠¨ g⊸≡⟜(0⊸(≠´))¨⊸/ (1↓⥊↕(≠bs)⥊2)/¨<bs }¨ 0⊸P¨inp # ≠ is XOR for bit vectors

S←{ 𝕊·‿b‿j: # https://old.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
  m←•HashMap˜⟨⟩ ⋄ p←⟨(≠j)⥊0⟩•HashMap⟨⟨⟨0,(≠j)⥊0⟩⟩⟩ # memo and parity
  { 𝕊⟨x,c,v⟩: x p.Set (⟨⟩ p.Get x)∾<⟨c,v⟩}¨ {⟨2|x←+´𝕩,≠𝕩,x⟩}¨ (1↓⥊↕(≠b)⥊2)/¨<b
  { ∧´0=𝕩?0; m.Has𝕩?m.Get𝕩;
    Rec j: r ⊣ j m.Set r←∞⌊´{𝕊c‿x:c+2×Rec 2÷˜j-x}¨{∧´j≥1⊑𝕩}¨⊸/ ⟨⟩ p.Get 2|j
  }j
}
!14677= +´S¨1⊸P¨inp
