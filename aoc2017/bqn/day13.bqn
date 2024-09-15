⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← (•BQN¨':'⊸Split)¨ •FLines "../inputs/day13.txt"
•Show +´{a‿b: a×b×0=a|˜2-˜b×2}¨inp
•Show +⟜1 •_while_ {𝕊d: 0=×´{a‿b: (a+d)|˜2-˜b×2}¨inp} 0
