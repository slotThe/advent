⟨Ints⟩ ← •Import "../../util/bqn_util/util.bqn"

# Using bit-vectors because we need u64's for this.
ToBV  ← { 𝕊p: p𝕊⟨⟩; 0𝕊r: r«64⥊0; p𝕊r: (⌊p÷2)𝕊(r∾˜2|p) }
XOR   ← ≠
ToNum ← 2⊸×⊸+˜´⌽
Sft   ← { (ToNum𝕩)⥊⟜0⊸»𝕨 } # Right-shift 𝕨 by the value of 𝕩
b1←ToBV 1 ⋄ b2←ToBV 2 ⋄ b3←ToBV 3 ⋄ b7←ToBV 7
⟨a,·,·⟩‿ops ← 3(↑⋈↓) ToBV¨ Ints ∾•FLines"../inputs/day17.txt"

# Hand optimised from input: (𝕩 & 7 ^ 1) ^ (𝕩 » (𝕩 & 7 ^ 2)) & 7
Step ← { b7∧(b1 XOR 𝕩∧b7)XOR(𝕩 Sft (b2 XOR 𝕩∧b7)) }
r ← ⟨⟩
{ r↩r∾<Step𝕩 ⋄ 𝕩 Sft b3 }•_while_(∨´) a
!"7,1,5,2,4,0,7,6,1"≡ 1↓∾∾','⊸⋈∘•Repr∘ToNum¨r

Two ← {
  { 𝕊ops‿prevs:
    ⟨¯1↓ops, ∾{𝕊p: {(Step𝕩) ≡ ¯1⊑ops}¨⊸/{(ToBV𝕩)∨ «⍟3 p}¨↕8}¨prevs⟩
  }•_while_{ 𝕊ops‿·: ⟨⟩≢ops } 𝕩‿⟨ToBV 0⟩
}
!37222273957364≡⌊´ToNum¨1⊑Two ops
