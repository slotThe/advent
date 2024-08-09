⍝ ⎕A gives the upper-case alphabet as a string, ⎕C makes that lower-case.
⍝ Dyadic ⍳ is index-of.
input ← (((⎕C⎕A),⎕A)∘⍳)¨ ⊃⎕NGET '../inputs/day3.txt' 1

⍝  (i) Reshape each cell into two rows of equal lengths and then ravel them
⍝      together, creating a vector of shape [2].
⍝ (ii) Look for overlapping elements of each 2-element vector, and get the
⍝      unique one(s) (in case of duplicates).
one ← +/{∪⍤∩/  ,/2(2÷⍨≢⍵)⍴⍵}¨
⍝       (ii)        (i)

⍝  (i) The line (≢⍵)⍴1 0 0 creates a vector like 1 0 0 1 0 0… until it reaches
⍝      the length of ⍵. Then, enclosed partition is used to partition the input
⍝      into chunks of 3.
⍝ (ii) Same as part one, only apply it to every chunk.
two ← +/{(∪⍤∩/)¨((≢⍵)⍴1 0 0)⊂⍵}
⍝         (ii)       (i)

⎕ ← (one input) , (two input)
