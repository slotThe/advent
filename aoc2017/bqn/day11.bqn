⟨Split⟩ ← •Import "../../util/bqn_util/util.bqn"
dirs ← "n"‿"ne"‿"se"‿"s"‿"sw"‿"nw"
movs ← ⟨ 0‿¯1‿1, 1‿¯1‿0, 1‿0‿¯1, 0‿1‿¯1, ¯1‿1‿0, ¯1‿0‿1 ⟩
pts ← { ⊑ movs/˜(<𝕩)⍷dirs }¨ ','Split ⊑•FLines "../inputs/day11.txt"
•Show    ⌈´|  +´pts
•Show ⌈´ ⌈´|¨ +`pts
