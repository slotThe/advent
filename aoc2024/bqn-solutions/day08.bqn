⟨Ix⟩ ← •Import "../../util/bqn_util/util.bqn"
inp←>•FLines"../inputs/day08.txt"
B ← { ∧´𝕨{(0≤𝕩)∧𝕩<𝕨}¨𝕩 } # In bounds? 𝕨 is rows‿cols and 𝕩 is x‿y
_Solve ← { Antinodes _𝕣 grid:
  antennas ← ('.'⊸≠∧∊)⊸/∾<˘grid
  ≠⍷(≢grid)⊸B¨⊸/ ∾{ 𝕊a:
    ps ← Ix a=inp           # All points for antenna a
    ∾(≠⌜˜∘(↕≠) /○⥊ Antinodes⌜˜) ps
  }¨antennas
}
{⟨𝕩-2×𝕩-𝕨⟩}_Solve inp                    # ⇒ 299
{ p𝕊np: {p-𝕩×p-np}¨(1+↕≠inp) }_Solve inp # ⇒ 1032
