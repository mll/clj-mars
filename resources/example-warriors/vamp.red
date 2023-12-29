;redcode-94
;name Dwarf
;author A.K. Dewdney
;strategy Bombs the core at regular intervals.
;(slightly modified by Ilmari Karonen)
;assert CORESIZE % 4 == 0

spread dat #1002 #1002
loop:   add  spread, target1 
        cmp @target1, @target2
        jmp  loop 
target1: dat 0, #4004
target2: dat 0, #4         
         end loop