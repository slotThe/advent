input ← ↑⊃⎕NGET'../inputs/day1.txt'1

⍝   (i) Enclose each rank 1 subarray in the array, so that each line is in its
⍝       own cell.
⍝  (ii) Partition according to empty lines
⍝ (iii) Parse (eval) each line into a number, and immediately sum all of the
⍝       individual calories for each elf.
parse ← {
  ⍝ (ii)      (ii)     (i)
  (+⌿⍎¨)¨  (∨/' '≠⍵)⊆  ⊂⍤1⊢⍵
}

⍝ Sort the array, take the first element for part 1, and the sum of the
⍝ highest three for part 2.
⎕ ← { (⊃,+⌿) 3↑ ⍵[⍒⍵] } parse input

⍝ Bonus: the same thing in GNU APL, because I like free software.
⍝
⍝ input ← ⎕FIO[49] '/home/slot/repos/advent-of-code/aoc2022/inputs/day1.txt'
⍝ { parse ← +⌿¨ ⍎¨¨ (∨⌿¨' '≠⍵)⊂⍵ ◊ {{(↑⍵),(+⌿⍵)}3↑parse[⍒parse]} } input
