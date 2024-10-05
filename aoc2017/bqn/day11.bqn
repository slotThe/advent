⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
dirs ← "n"‿"ne"‿"se"‿"s"‿"sw"‿"nw"
movs ← ⟨ 0‿¯1, 1‿¯1, 1‿0, 0‿1, ¯1‿1, ¯1‿0 ⟩
pts ← { ⊑ movs/˜(<𝕩)⍷dirs }¨ ','Split ⊑•FLines "../inputs/day11.txt"
   +´∘|  +´pts # ⇒ 707
⌈´ +´∘|¨ +`pts # ⇒ 1490
