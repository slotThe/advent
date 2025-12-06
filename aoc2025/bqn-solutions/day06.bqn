⟨Ints,SplitF⟩←•Import "../../util/bqn_util/util.bqn"
ns‿op←¯1(↓⋈⊑)•FLines"../inputs/day06.txt" ⋄ op↩⟨+,×⟩⊏˜'*'=' '⊸≠⊸/op
!4580995422905 = +´(<˘⍉>Ints¨ns)             {𝕏´𝕨}¨op
!10875057285868= +´(⊑⚇1 ⟨⟩SplitF Ints¨<˘⍉>ns){𝕏´𝕨}¨op
