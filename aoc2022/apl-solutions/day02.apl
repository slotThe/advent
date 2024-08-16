⎕IO ← 0
x y ← ↓⍉↑({⍸ ⍵∊¨'AX' 'BY' 'CZ'}¨ '\w'⎕S'&')¨ ⊃⎕NGET '../inputs/day2.txt' 1
part1 ← { 1+⍵+3×3|⍵-⍺-1 }   ⍝ ⍺ = op, ⍵ = me
part2 ← { 1+(3|⍵+⍺-1)+3×⍵ } ⍝ ⍺ = op, ⍵ = res
⎕ ← +/¨ (x part1 y) (x part2 y)
