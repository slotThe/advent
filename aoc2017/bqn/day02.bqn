⟨Split, tab⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← >(•ParseFloat¨ tab⊸Split)¨ •FLines "../inputs/day02.txt"

•Show +´ (⌈´-⌊´)˘ inp  # ⇒ 47136

ED ← {                 # Evenly divides
  t ← ÷⌜˜𝕩             # Division table
  ⊑ ((1≠t)∧t=⌊t)(/○⥊)t # Select integer that is not 1
}
•Show +´ ED˘ inp       # ⇒ 250
