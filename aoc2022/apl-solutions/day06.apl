⎕IO ← 0  ⍝ Important because we index with ⍳ below!
input ← ⊃⊃⎕NGET '../inputs/day6.txt' 1

⍝ A relatively straightforward solution; (1) takes care of creating
⍝ windows of size 4, (2) computes the sum of the number of pairwise
⍝ non-matching numbers in each cell, and (3) gets the index of the
⍝ first occurrence of ⍵-many non-equal numbers. To get the correct
⍝ position (since we get the index of the start of the window) we
⍝ just have to offset this with the given stride.
solve ← {
  ⍝  (3)  (2)     (1)
  ⍵+ ⍵⍳⍨ (+/≠)¨ ⍵,/input
}

⎕ ← (solve 4) , solve 14
