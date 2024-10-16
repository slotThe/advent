# There is only one mul instruction we don't skip in the code. We start at
#   a:0 b:65 c:65 d:2 e:2 f:1 g:0 h:0
# d and e need to get up to 65 each, which they do stacked on top of one
# another (so e goes 2–65, then d goes 2→3, then e again goes 2–65, …).
# Since b and c have the same value, we don't go into the outer loop.
63×63                                                # ⇒ 3969

# Find all non-primes from 106500 to 123500 in steps of 17.
+´{1≠+´0=𝕩|˜↕⌈√𝕩}¨+⟜17⍟(↕1+17÷˜123500-106500) 106500 # ⇒ 917
