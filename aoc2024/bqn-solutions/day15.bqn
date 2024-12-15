⟨SplitF, Ix⟩ ← •Import "../../util/bqn_util/util.bqn"
Start ← {⊑Ix'@'=𝕩}

ins‿mvs ← {
  ins‿mvs ← ⟨⟩SplitF •FLines"../inputs/day15.txt"
  ⟨>ins, (⟨'^','>','v','<'⟩⊐∾mvs)⊏⟨¯1‿0,0‿1,1‿0,0‿¯1⟩⟩
}

Sim ← { grid‿pos‿dirs:
  { ⟨⟩‿⟨⟩‿ds:   ⟨Start grid⟩‿⟨⟩‿(1↓ds);          # Change dir after wall hit
    ⟨⟩‿seen‿ds:                                  # Change dir after moving
      {grid'.'⌾((⊑𝕩)⊸⊑)↩}¨ks←(⊣⋈⊑⟜grid)¨seen
      {𝕊p‿v: grid v⌾((p+⊑ds)⊸⊑)↩}¨ks
      ⟨Start grid⟩‿⟨⟩‿(1↓ds);
    ps‿seen‿ds: ⊑(<⊑ps)∊seen? ⟨1↓ps, seen, ds⟩;  # Already explored
    ps‿seen‿ds:                                  # Explore new position
      { '#': ⟨⟨⟩,                 ⟨⟩,      ds⟩;  # Hit wall -> abort
        '.': ⟨1↓ps,               seen∾<p, ds⟩;  # Walking on sunshine
        'O': ⟨(<np)        ∾1↓ps, seen∾<p, ds⟩;  # Box
        '[': ⟨⟨np+0‿1 , np⟩∾1↓ps, seen∾<p, ds⟩;  # BBooxx
        ']': ⟨⟨np+0‿¯1, np⟩∾1↓ps, seen∾<p, ds⟩
      }grid⊑˜ np← (⊑ds)+ p←⊑ps
  }•_while_{ ·‿·‿ds: ⟨⟩≢ds } ⟨pos⟩‿⟨⟩‿dirs
  grid                                            # return the finished grid
}
Solve ← { +´{𝕊y‿x: x+y×100}¨ Ix∊⟜'O'‿'[' Sim⟨𝕩, Start 𝕩, mvs⟩ }

!1552463≡Solve ins
!1554058≡Solve (∾{'O':"[]"; '@':"@."; 𝕩‿𝕩}¨)˘ins
