⟨Ints⟩ ← •Import "../../util/bqn_util/util.bqn"
⟨BV,ToNum,Rst,Lst,XOR,b0,b1,b2,b3,b7⟩ ← •Import"../../util/bqn_util/bitvec.bqn"

⟨a,·,·⟩‿ops ← 3(↑⋈↓) BV¨ Ints ∾•FLines"../inputs/day17.txt"

# Hand optimised from input: (𝕩 & 7 ^ 1) ^ (𝕩 » (𝕩 & 7 ^ 2)) & 7
Step ← { b7∧(b1 XOR 𝕩∧b7)XOR(𝕩 Rst (b2 XOR 𝕩∧b7)) }
r ← ⟨⟩
{ r↩r∾<Step𝕩 ⋄ 𝕩 Rst 3 }•_while_(∨´) a
!"7,1,5,2,4,0,7,6,1"≡ 1↓∾∾','⊸⋈∘•Repr∘ToNum¨r

Two ← {
  { 𝕊ops‿prevs:
    ⟨¯1↓ops, ∾{𝕊p: {(Step𝕩) ≡ ¯1⊑ops}¨⊸/{(BV𝕩)∨ p Lst 3}¨↕8}¨prevs⟩
  }•_while_{ 𝕊ops‿·: ⟨⟩≢ops } 𝕩‿⟨b0⟩
}
!37222273957364≡⌊´ToNum¨1⊑Two ops
