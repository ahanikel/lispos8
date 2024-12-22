        .setcpu "65C02"
        .debuginfo

        TYPE_CONS := $99
        TYPE_VECT := 1
        TYPE_STRG := 2
        TYPE_SYMB := 3
        TYPE_BYTE := 4
        TYPE_WORD := 5
        TYPE_FLOT := 6
        TYPE_FUNC := 7

        .zeropage
        .org $90
PRINT_VECTOR:   .res 2          ; $90
PAREN_LEVEL: .res 1             ; $92
NIL:    .byte 0                 ; $93
HEAP_PTR:  .res 2               ; $94
OBJECT_PTR:      .res 2         ; $96
ARGUMENTS:      .res 2          ; $98

        ;; .segment "HEAP"
        ;; .res     $3000
        
        .segment "LISP"
        .org $E000
LISP:
        jsr PRINT_NEWLINE
        lda #$01                ; initialise tree level
        sta PAREN_LEVEL
        lda #$00                ; initialise tree level
        sta HEAP_PTR            ; initialise heap pointer
        sta ARGUMENTS           ; initialise argument stack
        lda #$10
        sta HEAP_PTR+1
        lda #$0F
        sta ARGUMENTS+1
        lda #$00                ; initialise the top-level list
        jsr store_and_inc_arguments
        jsr store_and_inc_arguments
read_next_char: 
        jsr wait_char
read:   
        cmp #' '
        beq read_next_char
        cmp #'('
        beq read_list
        cmp #')'
        beq end_list
        cmp #$0D
        bne read_atom
        jmp eval

read_atom:
        tax
        lda HEAP_PTR
        jsr store_and_inc_arguments
        lda HEAP_PTR+1
        jsr store_and_inc_arguments
        lda #TYPE_SYMB
        jsr store_and_inc_heap_ptr
        txa
read_atom_next_char: 
        jsr store_and_inc_heap_ptr
        jsr wait_char
        jsr is_atom_char
        bne read_atom_next_char
read_atom_done:       
        tax
        lda #$00
        jsr store_and_inc_heap_ptr
        txa
        bra read

read_list:
        inc PAREN_LEVEL
        bne read_list_1
        jmp err_list_overflow
read_list_1:    
        lda #$00
        jsr store_and_inc_arguments
        jsr store_and_inc_arguments
        bra read_next_char

end_list:
        dec PAREN_LEVEL
        bne end_list_1
        jmp err_list_underflow
end_list_1:     
        lda #$00
        jsr store_and_inc_arguments
        jsr store_and_inc_arguments
end_list_loop:  
        jsr make_cons
        bcc end_list_loop
        jmp read_next_char
        
store_and_inc_heap_ptr:
        sta (HEAP_PTR)
inc_heap_ptr:   
        clc
        lda HEAP_PTR
        adc #$01
        sta HEAP_PTR
        bcc store_and_inc_heap_ptr_done
        inc HEAP_PTR+1
        cmp #$3F
        bpl err_nomem
store_and_inc_heap_ptr_done:
        rts

store_and_inc_arguments:
        sta (ARGUMENTS)
inc_arguments:   
        inc ARGUMENTS
        bne store_and_inc_arguments_done
        jmp err_list_overflow
store_and_inc_arguments_done:
        rts
        
read_and_dec_arguments:
        dec ARGUMENTS
        lda ARGUMENTS
        cmp #$FF
        bne read_and_dec_arguments_done
        jmp WOZMON              ; internal error, wozmon
read_and_dec_arguments_done:
        lda (ARGUMENTS)
        rts

        ;; Returns with zero flag clear if A is an atom character
        ;; Returns with zero flag set if A is NUL, SPC, TAB, RET, '(' or ')'
is_atom_char:
        beq is_atom_char_done
        cmp #' '
        beq is_atom_char_done
        cmp #$09
        beq is_atom_char_done
        cmp #$0D
        beq is_atom_char_done
        cmp #'('
        beq is_atom_char_done
        cmp #')'
is_atom_char_done:
        rts
        
eval:
        jsr PRINT_NEWLINE
        jsr PRINT_NEWLINE
        lda PAREN_LEVEL
        cmp #$01
        beq print_nil
        bmi err_too_many_closed_parens
        bra err_unclosed_paren

print:
        jsr PRINT_NEWLINE
        ldy #$00
print_loop:     
        lda (PRINT_VECTOR),y
        beq print_done
        jsr CHROUT
        iny
        bra print_loop
print_done:
        jsr PRINT_NEWLINE
        jmp WOZMON
        jmp read_next_char

err_unclosed_paren:
        lda #<msg_unclosed_paren
        sta PRINT_VECTOR
        lda #>msg_unclosed_paren
        sta PRINT_VECTOR+1
        bra exception

err_too_many_closed_parens:
        lda #<msg_too_many_closed_parens
        sta PRINT_VECTOR
        lda #>msg_too_many_closed_parens
        sta PRINT_VECTOR+1
        bra exception

err_list_overflow:
        lda #<msg_list_overflow
        sta PRINT_VECTOR
        lda #>msg_list_overflow
        sta PRINT_VECTOR+1
        bra exception

err_list_underflow:
        lda #<msg_list_underflow
        sta PRINT_VECTOR
        lda #>msg_list_underflow
        sta PRINT_VECTOR+1
        bra exception

err_nomem:
        lda #<msg_nomem
        sta PRINT_VECTOR
        lda #>msg_nomem
        sta PRINT_VECTOR+1
        bra exception
        
print_nil:
        lda #<msg_nil
        sta PRINT_VECTOR
        lda #>msg_nil
        sta PRINT_VECTOR+1
        bra print
        
exception:
        bra print
        
wait_char:
        jsr CHRIN
        bcc wait_char
        rts

msg_too_many_closed_parens:     .asciiz "? Too many closed parentheses"
msg_unclosed_paren:     .asciiz "? Unclosed parentheses"
msg_nil:         .asciiz "NIL"
msg_list_overflow:      .asciiz "? List overflow"
msg_list_underflow:      .asciiz "? List underflow"
msg_nomem:      .asciiz "? Out of memory"
        
make_cons:
        lda ARGUMENTS
        sec
        sbc #$04
        sta ARGUMENTS
        lda (ARGUMENTS)         ; argptr 0/1 becomes the car
        beq make_cons_1         ; argptr 2/3 becomes the cdr
        clc
make_cons_1:    
        ldy #$01
        lda (ARGUMENTS),y
        bcc make_cons_2
        beq make_cons_2
        clc                     ; Returns C=1 if CAR is $0000
make_cons_2:    
        bcc make_cons_car_is_nonzero

        ldy #$02                ; car is zero, we're done, return last cons
        lda (ARGUMENTS),y
        jsr store_and_inc_arguments
        lda (ARGUMENTS),y
        jsr store_and_inc_arguments
        rts
        
make_cons_car_is_nonzero:       
        ;; we could use the HEAP_PTR directly and increase it at the end
        ;; but that would be unsafe
        lda HEAP_PTR            ; allocate new cons
        sta OBJECT_PTR
        lda HEAP_PTR+1
        sta OBJECT_PTR+1
        lda #TYPE_CONS
        jsr store_and_inc_heap_ptr
        jsr inc_heap_ptr
        jsr inc_heap_ptr
        jsr inc_heap_ptr
        jsr inc_heap_ptr

        bcc make_cons_car_is_nonzero_1
        inc OBJECT_PTR+1
make_cons_car_is_nonzero_1:     
        inc OBJECT_PTR

        lda (ARGUMENTS)
        sta (OBJECT_PTR)
        ldy #$01
        lda (ARGUMENTS),y
        sta (OBJECT_PTR),y
        iny
        lda (ARGUMENTS),y
        sta (OBJECT_PTR),y
        iny
        lda (ARGUMENTS),y
        sta (OBJECT_PTR),y

        bcc make_cons_car_is_nonzero_2
        dec OBJECT_PTR+1
make_cons_car_is_nonzero_2:     
        dec OBJECT_PTR

        lda OBJECT_PTR
        jsr store_and_inc_arguments
        lda OBJECT_PTR+1
        jsr store_and_inc_arguments
        rts
