        .setcpu "65C02"
        .debuginfo

        TYPE_CONS := $99
        TYPE_VECT := 1
        TYPE_STRG := 2
        TYPE_SYMB := 3
        TYPE_BYTE := 4
        TYPE_WORD := 5
        TYPE_FLOT := 6
        TYPE_FCOM := 7          ; compiled function
        TYPE_FINT := 8          ; interpreted function

        .zeropage
        .org $90
PRINT_VECTOR: .res 2            ; $90
PAREN_LEVEL:  .res 1            ; $92
              .res 1            ; $93
HEAP_PTR:     .res 2            ; $94
OBJECT_PTR:   .res 2            ; $96
ARGUMENTS:    .res 2            ; $98
INTERNED:     .res 2            ; $9A

        .segment "LISP"
        .org $E000
LISP:
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
        lda #<interned_table    ; initialise the INTERNED pointer
        sta INTERNED
        lda #>interned_table
        sta INTERNED+1
read_next_char:
        jsr PRINT_NEWLINE
read_next_char_no_newline:
        jsr wait_char
read:
        cmp #' '
        beq read_next_char_no_newline
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
        jsr intern_atom
        txa
        bra read

intern_atom:
        jsr dup_arg
        jsr cf_assoc+1
        rts

dup_arg:
        rts

read_list:
        inc PAREN_LEVEL
        bne read_list_1
        jmp err_list_overflow
read_list_1:
        lda #$00
        jsr store_and_inc_arguments
        jsr store_and_inc_arguments
        bra read_next_char_no_newline

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
        jmp read_next_char_no_newline

        ;; Write a byte from the A register to the heap
        ;; and increase the HEAP_PTR.
        ;; Returns C=1 if a page boundary was crossed.
        ;; Signals err_nomem if HEAP_PTR >= $3F00
        ;; Modifies: flags, A
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
        jmp err_nomem
store_and_inc_heap_ptr_done:
        rts

        ;; Increase the HEAP_PTR by Y bytes.
        ;; Modifies: flags, Y
inc_heap_ptr_by:
        jsr inc_heap_ptr
        dey
        bne inc_heap_ptr_by
        rts

        ;; Write a byte from the A register to the object
        ;; and increase the OBJECT_PTR.
        ;; The object must have been allocated already.
        ;; Returns C=1 if a page boundary was crossed.
        ;; Modifies: flags, A
store_and_inc_object_ptr:
        sta (OBJECT_PTR)
inc_object_ptr:
        clc
        lda OBJECT_PTR
        adc #$01
        sta OBJECT_PTR
        bcc store_and_inc_object_ptr_done
        inc OBJECT_PTR+1
store_and_inc_object_ptr_done:
        rts

dec_object_ptr:
        dec OBJECT_PTR
        lda OBJECT_PTR
        cmp #$FF
        bne dec_object_ptr_done
        dec OBJECT_PTR+1
dec_object_ptr_done:
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
        lda PAREN_LEVEL
        cmp #$01
        beq eval_1
        bmi eval_2
        jmp err_unclosed_paren
eval_2:
        jmp err_too_many_closed_parens
eval_1:
        dec ARGUMENTS
        dec ARGUMENTS
        lda (ARGUMENTS)
        sta PRINT_VECTOR
        ldy #$01
        lda (ARGUMENTS),y
        sta PRINT_VECTOR+1
        inc ARGUMENTS
        inc ARGUMENTS
        lda (PRINT_VECTOR)
        cmp #TYPE_SYMB
        beq eval_atom           ; evaluates to itself for now
        cmp #TYPE_CONS
        beq eval_cons           ; evaluates to itself for now
        jmp print

eval_atom:
        jmp print
eval_cons:
        jmp print

print:
        dec ARGUMENTS
        dec ARGUMENTS
        lda (ARGUMENTS)
        sta PRINT_VECTOR
        ldy #$01
        lda (ARGUMENTS),y
        sta PRINT_VECTOR+1
        inc ARGUMENTS
        inc ARGUMENTS
        lda (PRINT_VECTOR)
        cmp #TYPE_SYMB
        beq print_atom
        cmp #TYPE_CONS
        beq print_cons
        ora (PRINT_VECTOR),y
        beq print_nil
        jmp print_non_printable

print_nil:
        lda #<atom_nil
        sta PRINT_VECTOR
        lda #>atom_nil
        sta PRINT_VECTOR+1
        jsr PRINT_NEWLINE
        jsr print_cstring
        jmp read_next_char

print_cons:
        bra print_non_printable

print_atom:
        inc PRINT_VECTOR
        bne print_atom_1
        inc PRINT_VECTOR+1
print_atom_1:
        jsr PRINT_NEWLINE
        jsr print_cstring
        jmp read_next_char

print_cstring:
        ldy #$00
print_cstring_loop:
        lda (PRINT_VECTOR),y
        beq print_cstring_done
        jsr CHROUT
        iny
        bra print_cstring_loop
print_cstring_done:
        rts

print_non_printable:
        lda #<msg_non_printable
        sta PRINT_VECTOR
        lda #>msg_non_printable
        sta PRINT_VECTOR+1
        jsr PRINT_NEWLINE
        jsr print_cstring
        lda OBJECT_PTR+1
        jsr PRBYTE
        lda OBJECT_PTR
        jsr PRBYTE
        lda #'>'
        jsr CHROUT
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

exception:
        jsr print_cstring
        jmp read_next_char

wait_char:
        jsr CHRIN
        bcc wait_char
        cmp #$04                ; CTRL-D
        bne wait_char_end
        ldx #$FF
        txs
        jmp WOZMON
wait_char_end:
        rts

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
        ldy #$04
        jsr inc_heap_ptr_by

        jsr inc_object_ptr
        ldy #$04
make_cons_car_is_nonzero_store_args:
        dey
        lda (ARGUMENTS),y
        sta (OBJECT_PTR),y
        cpy #$00
        bne make_cons_car_is_nonzero_store_args
        jsr dec_object_ptr

        lda OBJECT_PTR
        jsr store_and_inc_arguments
        lda OBJECT_PTR+1
        jsr store_and_inc_arguments
        rts

cf_quit:
        .byte TYPE_FCOM
        tsx
        inx
        inx
        txs
        jmp WOZMON

cf_assoc:
        .byte TYPE_FCOM
        rts

msg_too_many_closed_parens: .asciiz "? Too many closed parentheses"
msg_unclosed_paren:         .asciiz "? Unclosed parentheses"
msg_list_overflow:          .asciiz "? List overflow"
msg_list_underflow:         .asciiz "? List underflow"
msg_nomem:                  .asciiz "? Out of memory"
msg_non_printable:          .asciiz "<Non-printable object at $"

atom_nil:
        .byte TYPE_SYMB
        .asciiz "NIL"

atom_quit:
        .byte TYPE_SYMB
        .asciiz "QUIT"

function_table:
        .byte $99
        .word atom_quit
        .word function_table_1
function_table_1:
        .byte $99
        .word cf_quit
        .word $0000

interned_table:
        .byte $99
        .word atom_nil
        .word interned_table_1
interned_table_1:
        .byte $99
        .word atom_quit
        .word $0000
