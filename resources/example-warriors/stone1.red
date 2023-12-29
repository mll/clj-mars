;redcode-94
;name Dwarf
;author A.K. Dewdney
;strategy Bombs the core at regular intervals.
;(slightly modified by Ilmari Karonen)
;assert CORESIZE % 4 == 0

bull: equ 7

spread: equ 4+2 * bull

loop:   add  #spread, ((bomb +  1)*2)/ 2 - 3 + 2
        mov  bomb, @bomb
        jmp  loop ;yikes
bomb:   dat  #0, #0
        end loop