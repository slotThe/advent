Solve ← { # The definition of write-only code :)
  𝕩⊑ 0‿0‿0‿0‿0 { # char 𝕊 inGarbage?‿ignored?‿depth‿score‿garbageCount
    '{' 𝕊 0‿i‿d‿s‿c: 0‿0‿(d+¬i)‿s‿c;
    '}' 𝕊 0‿i‿d‿s‿c: 0‿0‿(d-¬i)‿(s+d×¬i)‿c;
    '!' 𝕊 g‿0‿d‿s‿c: g‿1‿d‿s‿c;
    '<' 𝕊 0‿0‿d‿s‿c: 1‿0‿d‿s‿c;
    '>' 𝕊 1‿i‿d‿s‿c: i‿0‿d‿s‿c;
    cr  𝕊 g‿i‿d‿s‿c: g‿0‿d‿s‿(c+g∧¬i)
  }´ ⌽•FChars"../inputs/day09.txt"
}
•Show (Solve ¯2) ⋈ Solve ¯1
