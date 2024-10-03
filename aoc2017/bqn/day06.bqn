⟨Split, tab⟩ ← •Import "../../util/bqn_util/util.bqn"
inp ← •ParseFloat¨ tab Split ⊑•FLines "../inputs/day06.txt"
s‿d ← { seen‿xs:
    m ← ⌈´xs
    ⟨ seen∾<xs, xs+ +˝↑‿(≠xs)⥊(0⥊˜⊑xs⊐m)∾⟨-m⟩∾m⥊1 ⟩
    #                 reshape  before     at  after
  } •_while_ {
    seen‿xs: ⊑¬(<xs)∊seen
  } ⟨⟩‿inp
•Show ≠s         # ⇒ 7864
•Show ⊑(≠s)-s⊐<d # ⇒ 1695
