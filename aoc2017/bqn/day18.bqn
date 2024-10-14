⟨Split,cr⟩←•Import"../../util/bqn_util/util.bqn"
inp ← •ParseFloat⎊⊢¨∘(' '⊸Split)¨•FLines "../inputs/day18.txt"

V ← { 1=•Type 𝕩? 𝕩; (⊑𝕩-"a")⊑𝕨 } # [V]alue, 𝕨=arr, 𝕩=el
_C ← { regs F _𝕣 x‿y: F⟜(regs V y)⌾((⊑x-"a")⊸⊑) regs } # [C]hange

# Simulate the instructions taking two arguments;
# these are the same for both parts.
SD ← { 𝕊⟨ix,regs,ins‿x‿y⟩:
  { 𝕊"set": ⟨ix+1, regs ⊢_C x‿y⟩;
    𝕊"add": ⟨ix+1, regs +_C x‿y⟩;
    𝕊"mul": ⟨ix+1, regs ×_C x‿y⟩;
    𝕊"mod": ⟨ix+1, regs {𝕩|𝕨}_C x‿y⟩;
    𝕊"jgz": (regs V x)>0? ⟨ix+(regs V y), regs⟩;
    𝕊"jgz": ⟨ix+1, regs⟩
  } ins
}

snd ← 0
One ← {
  𝕊⟨ix,regs,ins‿x⟩:
    { 𝕊"snd": snd↩regs V x ⋄ ⟨ix+1, regs⟩;
      𝕊"rcv": snd!0=regs V x ⋄ ⟨ix+1, regs⟩
    } ins;
  SD 𝕩
}
{𝕊i‿r: One⟨i,r,i⊑inp⟩}•_while_{𝕊i‿·: (i≥0)∧i<≠inp}⎊•CurrentError⟨0,16⥊0⟩ # ⇒ 9423

Two ← {
  𝕊⟨my,other,ix,regs,ins‿x⟩:
    { 𝕊"snd":
        other∾↩regs V x
        ⟨my, other, ix+1, regs⟩;
      𝕊"rcv":
        q‿qs ← {⟨my,other,ix,regs⟩!⟨⟩≢𝕩 ⋄ 1(↑⋈↓)𝕩} my
        ⟨qs, other, ix+1, regs ⊢_C x‿(⊑q)⟩
    } ins;
  𝕊⟨my,other,ix,regs,inss⟩:
    ⟨my,other⟩∾SD⟨ix,regs,inss⟩
}

⊑{ 𝕊s1‿q0‿q1‿i1‿r1‿i2‿r2‿xs: # hahahahahahahahaha
  ⟨q0,q1,i1,r1⟩ ↩ Two⎊•CurrentError⟨q0,q1,i1,r1,i1⊑xs⟩
  old0 ← ≠q0
  ⟨q1,q0,i2,r2⟩ ↩ Two⎊•CurrentError⟨q1,q0,i2,r2,i2⊑xs⟩ # q0 and q1 swap here
  ⟨s1+old0<≠q0,q0,q1,i1,r1,i2,r2,xs⟩
}•_while_{ 𝕊·‿q0‿q1‿i1‿·‿i2‿·‿xs:
  ¬∧´⟨q0≡⟨⟩, q1≡⟨⟩, "rcv"≡⊑i1⊑xs, "rcv"≡⊑i2⊑xs⟩
} ⟨0, ⟨⟩,⟨⟩, 0,(15⥊0)∾0, 0,(15⥊0)∾1, inp⟩ # ⇒ 7620
