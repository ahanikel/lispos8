        .setcpu "65C02"
        .debuginfo

        TYPE_LIST := 0
        TYPE_VECT := 1
        TYPE_STRG := 2
        TYPE_SYMB := 3
        TYPE_BYTE := 4
        TYPE_WORD := 5
        TYPE_FLOT := 6
        TYPE_FUNC := 7

        CHRIN := $A1DB
        CHROUT := $A1FD
        PRINT_NEWLINE := $A208

        .zeropage
        .org $90
PRINT_VECTOR:   .res 2
PAREN_LEVEL: .res 1
HEAP_PTR:  .res 2
OBJECT_PTR:      .res 2

        ;; .segment "HEAP"
        ;; .res     $3000
        
        .segment "CODE"
        .org $0400
LISP:
        jsr PRINT_NEWLINE
        lda #$00                ; initialise tree level
        sta PAREN_LEVEL
        sta HEAP_PTR            ; initialise heap pointer
        lda #$10
        sta HEAP_PTR+1
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
        beq eval
        ;; otherwise it's an atom

read_atom:
        tax
        lda #<HEAP_PTR
        pha
        lda #>HEAP_PTR
        pha
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
        ;; Add this atom to the parent list.
        ;; If there is none, continue reading
        ;; TODO...
        txa
        jmp $FF00
        bra read

read_list:
        clv
        inc PAREN_LEVEL
        bvs err_list_overflow
        jsr make_cons
        bra read

end_list:
        clv
        dec PAREN_LEVEL
        bvs err_list_overflow
        beq read                ; top level, continue reading
        pla
        sta OBJECT_PTR
        pla
        sta OBJECT_PTR+1
        ;; store this list in the parent list
        ;; if there is no parent, throw a should-not-happen error
        ;; TODO...
        bra read
        
store_and_inc_heap_ptr:
        sta (HEAP_PTR)
        clc
        inc HEAP_PTR
        bcc store_and_inc_heap_ptr_done
        lda HEAP_PTR+1
        adc #$00
        cmp #$3F
        bpl err_nomem
store_and_inc_heap_ptr_done:
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
        bmi err_too_many_closed_parens
        bne err_unclosed_paren
        bra print_nil

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
        jmp $FF00
        jmp read

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
msg_nomem:      .asciiz "? Out of memory"
        
make_cons:
        lda #<HEAP_PTR
        pha
        lda #>HEAP_PTR
        pha
        lda #TYPE_LIST
        jsr store_and_inc_heap_ptr
        pla
        jsr store_and_inc_heap_ptr
        pla
        jsr store_and_inc_heap_ptr
make_cons_done: 
        rts
