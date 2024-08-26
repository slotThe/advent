⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← (•ParseFloat¨ (@+9)⊸Split)¨ •FLines "../inputs/day02.txt"

•Show +´ (⌈´-⌊´)¨ inp  # Part 1

Two ← {
  t ← ÷⌜˜𝕩             # Division table
  ⊑ ((1≠t)∧t=⌊t)(/○⥊)t # Select integer that is not 1
}
•Show +´ Two¨ inp
