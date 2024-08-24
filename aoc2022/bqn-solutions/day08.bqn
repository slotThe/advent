inp ← >(-⟜'0')¨ •FLines "../inputs/day8.txt"
Select ← { x‿y:        # There must be a better way, right?
  ⟨ ⟨ ⋈⟜y, (x+1)⊸↓ ⟩
    ⟨ ⋈⟜y, ⌽(x⊸↑)  ⟩
    ⟨ x⊸⋈, (y+1)⊸↓ ⟩
    ⟨ x⊸⋈, ⌽(y⊸↑)  ⟩ ⟩
}

IsVisible ← { arr 𝕊 pt:
  ∨´ { f‿g: ∧´(<⟜(pt⊑arr))∘(⊑⟜arr)∘F¨ G ↕≠arr }¨ Select pt
}
•Show +´⥊ { (𝕩⊸IsVisible)¨ ↕(≠𝕩)‿(≠𝕩) } inp

ViewingDistance ← { arr 𝕊 pt:
  Sol ← { f‿g:
    # The first 0 marks the last tree we can see
    a ← <⟜(pt⊑arr)∘⊑⟜arr∘F¨ G ↕≠arr
    # If we take n things from an integer vector of length n-1, the
    # resulting vector will have length n, padded with zero. Thus, we
    # have to make sure that we didn't take "too much". This is how one
    # differentiates the case ⟨ 1 ⟩ from the case ⟨ 1 0 ⟩.
    b ← (1+ a⊐0)↑a
    (≠b) ⌊ ≠a
  }
  ×´ Sol¨ Select pt
}
•Show ⌈´⥊ { (𝕩⊸ViewingDistance)¨ ↕(≠𝕩)‿(≠𝕩) } inp
