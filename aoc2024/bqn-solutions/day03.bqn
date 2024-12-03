⟨Split,lf⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← {
  i ← •FChars"../inputs/day03.txt"
  i ⊣ { i↩(𝕩↑i)∾"'"∾𝕩↓i }¨ixs+↕≠ixs←/i∊"'" # Escape for shelling out
}
TN ← {{⟨⟩:0;×´𝕩}•BQN"⟨"∾(¯1↓4↓𝕩)∾"⟩"}
SH ← { ⌽lf Split 1⊑•SH⟨
  "sh"
  "-c"
  "echo '"∾𝕩∾"' | perl -wnE 'say for /"∾𝕨∾"/g'"
⟩}

!189527826≡ +´TN¨ "mul\(\d+,\d+\)" SH inp
!63013756≡ 1⊑ 1‿0{
  ins𝕊e‿a: ⟨0‿1‿e⊑˜2⌊⊑"dont"‿"do"‿e⊐<ins, a+e×TN ins⟩
}´"(mul\(\d+,\d+\)|don''t|do)" SH inp
