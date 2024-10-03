⟨Split,cr⟩←•Import"../../util/bqn_util/util.bqn"
inp←' '⊸Split¨ •FLines"../inputs/day08.txt"

# Convert instructions to BQN code: Given a list of tokens like
#
#     ⟨ "d" "dec" "683" "if" "qn" "==" "0" ⟩
#
# convert them—if needed—to their respective BQN token:
#
#     ⟨ "d" "-↩" "683" "×" "qn" "=" "0" ⟩
conv ← ⟨
  ⟨"if", "dec", "inc", ">=", "==", "<=", "!="⟩
  ⟨"×" , "-↩" , "+↩" , "≥" , "=" , "≤" , "≠" , ⊢⟩ # last is fallthrough
⟩
C ← (⊑1⊐˜(⊑conv)≡¨<)◶(1⊑conv)¨

r ← ∊⊸/ ⊑¨inp                       # all registers ⇒ ⟨ "d" "rak" … ⟩
•Show •BQN ∾⟨
  "h∞w←0⋄"∾∾∾⟜"←0⋄"¨r               # zero everything ⇒ "h∞w←0 ⋄ d←0 ⋄ rak←0 ⋄ …"
  (∾⟜"⋄"⊸∾)´({∾𝕩∾"⋄h∞w⌈↩"∾⊑𝕩}C)¨inp # Check highest after every instruction
                                    # ⇒ "d-↩683×qn=0 ⋄ h∞w⌈↩d ⋄ d-↩-220×h=0 ⋄ h∞w⌈↩d…"
  "⋄part1←⌈´"∾(∾⟜"‿"⊸∾)´r           # get max ⇒ "⋄ part1 ← ⌈´d‿rak‿…"
  "⋄⟨ part1, h∞w ⟩"
⟩ # ⇒ ⟨ 4416 5199 ⟩
