;redcode
;name Prometheus (decoy)
;author John Metcalf
;strategy scanner
;assert CORESIZE==8000

        step   equ gap*2   ; 26
        gap    equ 13      ; 13

        bdist  equ 6199    ;6199
        first  equ scan+step ; zalezy gdzie

scan    sub    inc,        ptrs ; 12, 1 
ptrs    cmp    first+gap,  first ; -1 + 26 + 13 (38), 25 @@@@@@@@@@@@
        slt    #inc+4-ptrs,ptrs ; #10 + 4 - (-1) (#15), -1  
        djn    scan,       <-1242 ; -3, <-1242

wipe    mov    @jbmb,      @ptrs ; @6, @-3
        mov    sbmb,       <ptrs ;  3, <-4
        sub    adj,        ptrs  ; 5, -5
        jmn    ptrs,       scan ; -6, -7

sbmb    spl    0,          <clear-gap ; 0, <-12
clear   mov    adj,        <-2 ; 2, <-2
jbmb    jmp    clear,      inc+1 ; -1, 3

adj     dat    <-gap,      <-gap-1 ; <-13, <-14
inc     spl    <-step,     <-step ; <-26, <-26

boot    mov    <jbmb,      <to ; <-3, <6 
        mov    <jbmb,      <to ; <-4, <5
        mov    <jbmb,      <to ; <-5, <4
        mov    <jbmb,      <to ; <-6, <3
        mov    <jbmb,      <to ; <-7, <2
        djn    boot+1,     #3 ; -4, #3

to      djn    bdist+1,    #bdist+13 ; 6200, #6212

;a       for    MAXLENGTH-CURLINE
;        spl    a%gap,      1
;        rof

       end    boot ; -7
