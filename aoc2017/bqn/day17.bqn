n ← •ParseFloat⊑•FLines"../inputs/day17.txt"

# Actually simulate the thing; since we're moving to the right, the list as a
# whole has to rotate to the *left*. Move one more than the input so we insert
# to the right. This also keeps the point of insertion at the very front of
# the list, so we don't need to keep track of the index.
1⊑ ⟨0⟩{𝕨∾(n+1)⌽𝕩}´⌽1+↕2017 # ⇒ 1487

# We don't actually need to simulate the whole thing to know where one number
# is inserted. The 0-based indices are determined by the formula
#
#    (stride×index  mod  round) + 1
#                               ^^^ we are inserting after the index
#
# where 'index' is the current index, 'round' is the current round, and
# 'stride' is the stride length (i.e., our input). A simple scan over the
# (long!) input list gets us a (long!) list of all indices where things are
# at. We then just need to find the biggest index where a 1 is inserted, as we
# are looking for the largest (= also latest) item after 0.
⊑ (≠-⊐⟜1∘⌽) 0{1+𝕩|n+𝕨}`1+↕50e6 # ⇒ 25674054
