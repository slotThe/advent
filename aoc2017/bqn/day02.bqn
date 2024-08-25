Split ← (¬-˜⊢×·+`»⊸>)∘≠⊔⊢
inp ← (•ParseFloat¨ (@+9)⊸Split)¨ •FLines "../inputs/day02.txt"

One ← { (⌈´𝕩)-⌊´𝕩 }
•Show +´ One¨ inp

Two ← {
  t ← ÷⌜˜𝕩             # Division table
  ⊑ ((1≠t)∧t=⌊t)(/○⥊)t # Select integer that is not 1
}
•Show +´ Two¨ inp
