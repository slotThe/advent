⟨SplitF⟩ ← •Import "../../util/bqn_util/util.bqn"
IxOf ← /○⥊⟜↕⟜≢       # Index of all 1's in array 𝕩
Start ← {⊑IxOf'@'=𝕩}

ins‿mvs ← {
  ins‿mvs ← ⟨⟩SplitF •FLines"../inputs/day15.txt"
  ⟨>ins, (⟨'^','>','v','<'⟩⊐∾mvs)⊏⟨¯1‿0,0‿1,1‿0,0‿¯1⟩⟩
}

Sim ← { 𝕊grid‿pos‿dirs:
  { 𝕊⟨⟩‿⟨⟩‿ds:   ⟨Start grid⟩‿⟨⟩‿(1↓ds);          # hit wall
    𝕊⟨⟩‿seen‿ds:                                  # can move stuff
      {grid'.'⌾((⊑𝕩)⊸⊑)↩}¨ks←(⊣⋈⊑⟜grid)¨seen
      {𝕊p‿v: grid v⌾((p+⊑ds)⊸⊑)↩}¨ks
      ⟨Start grid⟩‿⟨⟩‿(1↓ds);
    𝕊ps‿seen‿ds: ⊑(<⊑ps)∊seen? ⟨1↓ps, seen, ds⟩;  # already collected
    𝕊ps‿seen‿ds:                                  # explore new position
      { 𝕊'#': ⟨⟨⟩,                 ⟨⟩,      ds⟩;
        𝕊'.': ⟨1↓ps,               seen∾<p, ds⟩;
        𝕊'O': ⟨(<np)        ∾1↓ps, seen∾<p, ds⟩;
        𝕊'[': ⟨⟨np+0‿1 , np⟩∾1↓ps, seen∾<p, ds⟩;
        𝕊']': ⟨⟨np+0‿¯1, np⟩∾1↓ps, seen∾<p, ds⟩
      }grid⊑˜ np← (⊑ds)+ p←⊑ps
  }•_while_{ 𝕊·‿·‿ds: ⟨⟩≢ds } ⟨pos⟩‿⟨⟩‿dirs
  grid                                            # return the finished grid
}
Solve ← { +´{𝕊y‿x: x+y×100}¨ IxOf∊⟜'O'‿'[' Sim⟨𝕩, Start 𝕩, mvs⟩ }

!1552463≡Solve ins
!1554058≡Solve (∾{𝕊'#':"##"; 𝕊'O':"[]"; 𝕊'.':".."; 𝕊'@':"@."}¨)˘ins
