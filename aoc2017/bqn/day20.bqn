P←{ p‿r←3(↑⋈↓)𝕩 ⋄ v‿a←3(↑⋈↓)r ⋄ ⟨p,v,a⟩ }
inp←{P•BQN'⟨'∾𝕩∾'⟩'}∘(∊⟜("-,"∾'0'+↕10)⊸/)¨•FLines"../inputs/day20.txt"
U←{p‿v‿a: ⟨p+v+a, v+a, a⟩}
# 1000 is a magic number.
•Show ⊑(⊢⊐⌊´){¯1⊑(+´|)∘⊑¨U⍟(↕1+1000)𝕩}¨inp             # ⇒ 376; see [Note Part 1]
•Show ≠{({𝕩-(>˝˘2↕𝕩)∾0}∘(1»≢˘⟜«)∘(⊑¨)⊸/)∨U¨𝕩}⍟1000 inp # ⇒ 574; I'm not solving quadratic equations.

# [Note Part 1]
#
# For my input, there is a unique particle with a smallest total acceleration,
# so instead of simulating everything one might also do
#
#    ⊑(⊢⊐⌊´)((+´|)¯1⊸⊑)¨inp
#
# for part 1. However, in the absence of that the solution would be a lot more
# complicated, as one would have to keep in mind velocity and direction of
# travel. I guess one could simulate the much smaller situation of all
# particles with the smallest acceleration.
