⟨Split,_Fix⟩ ← •Import"../../util/bqn_util/util.bqn"
inp ← •ParseFloat¨∘('/'⊸Split)¨ •FLines"../inputs/day24.txt"

_sol ← { Mx _𝕣:               # Max function Mx → max strength
  ⌈´∾{ 𝕊0‿b:
    b+ 1⊑¨ 1‿b { i‿t Rec xs:  # Step i, Target value t, list xs → step, strength
      {⟨i,+´𝕩⟩+ Mx ⟨i+1, 𝕩⊑˜t=⊑𝕩⟩Rec xs(¬∘∊/⊣)⟨𝕩⟩}¨t((>∊¨)/⊢)xs
    } inp(¬∘∊/⊣)⟨0‿b,b‿0⟩
  }∘∧¨0((>∊¨)/⊢)inp
}

{0⌈´1⊑¨𝕩}    _sol # ⇒ 1656
{⊑∨(<0‿0)∾𝕩} _sol # ⇒ 1642
