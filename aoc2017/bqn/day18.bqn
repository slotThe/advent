⟨Split⟩←•Import"../../util/bqn_util/util.bqn"
inp ← •ParseFloat⎊⊢¨∘(' '⊸Split)¨•FLines "../inputs/day18.txt"

V ← { 1=•Type 𝕩? 𝕩; (⊑𝕩-"a")⊑𝕨 } # [V]alue, 𝕨=arr, 𝕩=el
_C ← { regs F _𝕣 x‿y: F⟜(regs V y)⌾((⊑x-"a")⊸⊑) regs } # [C]hange

# Simulate the instructions taking two arguments;
# these are the same for both parts.
SD ← { 𝕊⟨ix,regs,ins‿x‿y⟩:
  { 𝕊"set": ⟨ix+1, regs  ⊢_C x‿y⟩;
    𝕊"add": ⟨ix+1, regs  +_C x‿y⟩;
    𝕊"mul": ⟨ix+1, regs  ×_C x‿y⟩;
    𝕊"mod": ⟨ix+1, regs |˜_C x‿y⟩;
    𝕊"jgz": 0<regs V x? ⟨ix+regs V y, regs⟩;
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

Two ← { # my queue, other queue, index, registers, instruction
  𝕊⟨my,ot,ix,rs,"snd"‿x⟩: ot∾↩rs V x ⋄ ⟨my, ot, ix+1, rs⟩;
  𝕊⟨⟨⟩,ot,ix,rs,"rcv"‿x⟩: ⟨⟨⟩,ot,ix,rs⟩;
  𝕊⟨my,ot,ix,rs,"rcv"‿x⟩: q‿qs ← 1(↑⋈↓)my ⋄ ⟨qs, ot, ix+1, rs⊢_C⟨x,⊑q⟩⟩;
  𝕊⟨my,ot,ix,rs,ins⟩    : ⟨my,ot⟩∾SD⟨ix,rs,ins⟩
}

# Concurrency at home:
⊑{ 𝕊s1‿q0‿q1‿i1‿r1‿i2‿r2‿xs: # hahahahahahahahaha
  ⟨q0,q1,i1,r1⟩ ↩ Two⟨q0,q1,i1,r1,i1⊑xs⟩
  old0 ← ≠q0
  ⟨q1,q0,i2,r2⟩ ↩ Two⟨q1,q0,i2,r2,i2⊑xs⟩ # q0 and q1 swap here
  ⟨s1+old0<≠q0,q0,q1,i1,r1,i2,r2,xs⟩
}•_while_{ 𝕊·‿q0‿q1‿i1‿·‿i2‿·‿xs:
  ∨´⟨ q0≢⟨⟩, q1≢⟨⟩, "rcv"≢⊑i1⊑xs, "rcv"≢⊑i2⊑xs ⟩
} ⟨0, ⟨⟩,⟨⟩, 0,(15⥊0)∾0, 0,(15⥊0)∾1, inp⟩ # ⇒ 7620
