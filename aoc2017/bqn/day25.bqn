tape ← ⟨⟩•HashMap⟨⟩
G ← 0⊸tape.Get # Get
W ← tape.Set   # Write
A←B←C←D←E←F←⊢  # Forward declarations! :)
A↩{ix𝕊0: ix W 1 ⋄ ⟨ix+1,B⟩; ix𝕊1: ix W 0 ⋄ ⟨ix-1,C⟩}
B↩{ix𝕊0: ix W 1 ⋄ ⟨ix-1,A⟩; ix𝕊1: ix W 1 ⋄ ⟨ix+1,D⟩}
C↩{ix𝕊0: ix W 1 ⋄ ⟨ix+1,A⟩; ix𝕊1: ix W 0 ⋄ ⟨ix-1,E⟩}
D↩{ix𝕊0: ix W 1 ⋄ ⟨ix+1,A⟩; ix𝕊1: ix W 0 ⋄ ⟨ix+1,B⟩}
E↩{ix𝕊0: ix W 1 ⋄ ⟨ix-1,F⟩; ix𝕊1: ix W 1 ⋄ ⟨ix-1,C⟩}
F↩{ix𝕊0: ix W 1 ⋄ ⟨ix+1,D⟩; ix𝕊1: ix W 1 ⋄ ⟨ix+1,A⟩}
{𝕊i‿Sim: i Sim G i}⍟12_919_244 0‿A
+´tape.Values@ # ⇒ 4287
