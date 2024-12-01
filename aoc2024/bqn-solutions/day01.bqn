⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
a‿b ← <∘∧˘⍉ >•ParseFloat¨¨' 'Split¨ •FLines"../inputs/day01.txt"
+´|a-b         # ⇒ 1579939
+´(+´b⊸=⊸×)¨a  # ⇒ 20351745
