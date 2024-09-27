⟨Split,cr⟩←•Import"../../util/bqn_util/util.bqn"
inp←' '⊸Split¨ •FLines"../inputs/day08.txt"

conv ← ⟨
  ⟨"if", "dec", "inc", ">=", "==", "<=", "!="⟩
  ⟨"×" , "-↩" , "+↩" , "≥" , "=" , "≤" , "≠" , ⊢⟩ # last is fallthrough
⟩
C ← (⊑1⊐˜(⊑conv)≡¨<)◶(1⊑conv) # Convert instructions to BQN code

r ← ∊⊸/ ⊑¨inp                               # all registers
d ← "h_r←0⋄"∾ ∾∾⟜"←0⋄"¨r                    # set everything to 0, add additional highest register
i ← (⊣∾"⋄"∾⊢)´({∾𝕩∾"⋄h_r ⌈↩ "∾⊑𝕩}C¨)¨inp    # Check highest after every instruction
•Show •BQN d∾i∾("⋄(⌈´"∾(⊣∾"‿"∾⊢)´r)∾")‿h_r" # ⇒ ⟨ 4416 5199 ⟩
