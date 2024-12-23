⟨Split,_merge⟩ ← •Import "../../util/bqn_util/util.bqn"

C3 ← { 𝕊hm:
  ∾{ 𝕊a:
    {𝕊a‿x: a‿x⊸∾∘<¨ {⊑(<a)∊hm.Get𝕩}¨⊸/ hm.Get x }¨ (<a)⋈¨hm.Get a
  }¨ 't'⊸=∘⊑¨⊸/ ⍷hm.Keys@
}

inp ← ⍷∘∾_merge •HashMap⟜(⋈¨⌽)¨ '-'⊸Split¨ •FLines"../inputs/day23.txt"
!926≡ ≠⍷∧¨∾C3 inp

MC ← { 𝕊hm:        # Greedy maximal clique
  ks ← ⍷hm.Keys@
  ks⊸{ ⟨⟩𝕊c: c;
       xs𝕊c: (1↓xs) 𝕊 ⊑(∧´{(<𝕩)∊hm.Get⊑xs}¨c)⊏⟨c, c∾<⊑xs⟩
 }∘⋈¨ks
}
!"az,ed,hz,it,ld,nh,pc,td,ty,ux,wc,yg,zz"≡ {
  ¯1↓ ∾ ∾⟜","¨ ∧⊑ (⊐⟜(⌈´)∘(≠¨)⊏⊢) MC inp
}
