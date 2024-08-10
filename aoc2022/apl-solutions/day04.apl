⍝ 1-3,4-6 gets transformed into a matrix like [[1 3] [4 6]]
input ← (2 2⍴⍎¨ ∘ ('\d+'⎕S'&'))¨⊃⎕NGET '../inputs/day4.txt' 1
S ← { +⌿⍺⍺¨⍵ }

⍝ (1): For a given 2×2 matrix, compute the min and max along the columns
⍝ (start and end). (2): compare this to all rank 1 subarrays (= rows).
⍝ If one such subarray contains both the min start and the max end, the
⍝ other array must live completely inside of it.
one ← {
  ⍝         (1)      (2)
  ∨⌿ ((⊃⌊⌿⍵),(⊃⊖⌈⌿⍵))≡⍤1 ⊢⍵
} S input

⍝ Just brute-forcing with indices. Whoops.
two ← {~ (⍵[1;2]<⍵[2;1]) ∨ ⍵[2;2]<⍵[1;1] } S input
⎕ ← one , two

⍝ Once I already had this completed, I saw a *much* better solution
⍝ online. I suppose I still haven't internalised the true essence of
⍝ array-first programming. One day.
⍝
⍝ a b c d ← ↓⍉↑(⍎¨('\d+'⎕S'&'))¨⊃⎕NGET'../inputs/day4.txt'1
⍝ one ← +/ 0≥ (a-c)×b-d
⍝ two ← +/ 0≥ (a-d)×b-c
