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
PRINT_VECTOR:     .res 2            ; $90
PAREN_LEVEL:      .res 1            ; $92
                  .res 1            ; $93
HEAP_PTR:         .res 2            ; $94
OBJECT_PTR_ONE:   .res 2            ; $96
OBJECT_PTR_TWO:   .res 2            ; $98
ARGUMENTS:        .res 2            ; $9A
INTERNED:         .res 2            ; $9C

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
        ;; and increase the OBJECT_PTR_ONE.
        ;; The object must have been allocated already.
        ;; Returns C=1 if a page boundary was crossed.
        ;; Modifies: flags, A
store_and_inc_object_ptr:
        sta (OBJECT_PTR_ONE)
inc_object_ptr:
        clc
        lda OBJECT_PTR_ONE
        adc #$01
        sta OBJECT_PTR_ONE
        bcc store_and_inc_object_ptr_done
        inc OBJECT_PTR_ONE+1
store_and_inc_object_ptr_done:
        rts

dec_object_ptr:
        dec OBJECT_PTR_ONE
        lda OBJECT_PTR_ONE
        cmp #$FF
        bne dec_object_ptr_done
        dec OBJECT_PTR_ONE+1
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
        ora PRINT_VECTOR
        bne eval_non_nil
        ;; nil (00 00) is special: it points to addr $0000
        ;; which should contain the symbol NIL
        ;; (03 4E 49 4C 00)
        ;; but we don't want to change anything there
eval_nil:
        lda #<atom_nil
        sta PRINT_VECTOR
        lda #>atom_nil
        sta PRINT_VECTOR+1
eval_non_nil:
        lda (PRINT_VECTOR)
        cmp #TYPE_SYMB
        beq eval_atom           ; evaluates to itself for now
        cmp #TYPE_CONS
        beq eval_cons           ; evaluates to itself for now
        jmp print

eval_atom:
        jmp print
eval_cons:
        jsr dup_arg
        jsr cf_car+1

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
        ora PRINT_VECTOR
        bne print_non_nil
print_nil:
        lda #<atom_nil
        sta PRINT_VECTOR
        lda #>atom_nil
        sta PRINT_VECTOR+1
print_non_nil:
        lda (PRINT_VECTOR)
        cmp #TYPE_SYMB
        beq print_atom
        cmp #TYPE_CONS
        beq print_cons
        jmp print_non_printable

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
        lda OBJECT_PTR_ONE+1
        jsr PRBYTE
        lda OBJECT_PTR_ONE
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

msg_too_many_closed_parens: .asciiz "? Too many closed parentheses"
msg_unclosed_paren:         .asciiz "? Unclosed parentheses"
msg_list_overflow:          .asciiz "? List overflow"
msg_list_underflow:         .asciiz "? List underflow"
msg_nomem:                  .asciiz "? Out of memory"
msg_non_printable:          .asciiz "<Non-printable object at $"

wait_char:
        jsr CHRIN
        bcc wait_char
        cmp #$04                ; CTRL-D
        bne wait_char_end
        jmp cf_quit+1
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
        sta OBJECT_PTR_ONE
        lda HEAP_PTR+1
        sta OBJECT_PTR_ONE+1
        lda #TYPE_CONS
        jsr store_and_inc_heap_ptr
        ldy #$04
        jsr inc_heap_ptr_by

        jsr inc_object_ptr
        ldy #$04
make_cons_car_is_nonzero_store_args:
        dey
        lda (ARGUMENTS),y
        sta (OBJECT_PTR_ONE),y
        cpy #$00
        bne make_cons_car_is_nonzero_store_args
        jsr dec_object_ptr

        lda OBJECT_PTR_ONE
        jsr store_and_inc_arguments
        lda OBJECT_PTR_ONE+1
        jsr store_and_inc_arguments
        rts

cf_quit:
        .byte TYPE_FCOM
        ldx #$FF
        txs
        jmp WOZMON

        ;; (assoc item alist)
cf_assoc:
        .byte TYPE_FCOM
        lda ARGUMENTS
        sec
        sbc #$04
        sta ARGUMENTS
        rts

        ;; Compare two values for identity (used internally)
        ;; Expects the object pointer to point to where the
        ;; two arguments are
eq:
        ldy #$02
        lda (OBJECT_PTR_ONE)
        cmp (OBJECT_PTR_ONE),y
        bne eq_done
        ldy #$01
        lda (OBJECT_PTR_ONE),y
        ldy #$03
        cmp (OBJECT_PTR_ONE),y
eq_done:
        rts

cf_eq:
        .byte TYPE_FCOM
        lda ARGUMENTS
        sec
        sbc #$04
        sta ARGUMENTS           ; consume arguments
        sta OBJECT_PTR_ONE
        lda ARGUMENTS+1
        sta OBJECT_PTR_ONE+1
        jsr eq
        beq cf_eq_return_t
        jmp return_nil
cf_eq_return_t:
        jmp return_t

cf_equalp:
        .byte TYPE_FCOM
        lda ARGUMENTS
        sec
        sbc #$04
        sta OBJECT_PTR_ONE
        lda ARGUMENTS+1
        sta OBJECT_PTR_ONE+1
        jsr eq
        bne cf_equalp_not_eq
        jmp cf_equalp_return_t  ; objects are identical
cf_equalp_not_eq:
        lda (OBJECT_PTR_ONE)    ; OBJECT_PTR_ONE points to argument ptrs
        pha
        ldy #$01
        lda (OBJECT_PTR_ONE),y
        pha
        ldy #$02
        lda (OBJECT_PTR_ONE),y
        pha
        ldy #$03
        lda (OBJECT_PTR_ONE),y
        sta OBJECT_PTR_TWO+1
        pla
        sta OBJECT_PTR_TWO
        pla
        sta OBJECT_PTR_ONE+1
        pla
        sta OBJECT_PTR_ONE

        lda (OBJECT_PTR_ONE)    ; OBJECT_PTR_ONE points to first value
        cmp (OBJECT_PTR_TWO)
        beq cf_equalp_same_type
        jmp cf_equalp_return_nil ; different types, can't be equalp
cf_equalp_same_type:
        cmp #TYPE_SYMB
        bne compare_cons
compare_symb:
        ldy #$01
compare_symb_loop:
        lda (OBJECT_PTR_ONE),y
        cmp (OBJECT_PTR_TWO),y
        beq cf_equalp_same_char
        jmp cf_equalp_return_nil
cf_equalp_same_char:
        ora #$00
        bne cf_equalp_symb_continue
        jmp cf_equalp_return_t            ; we're done
cf_equalp_symb_continue:
        iny
        jmp compare_symb_loop
compare_cons:
        cmp #TYPE_CONS
        beq cf_equalp_is_a_cons
        jmp cf_equalp_throw
cf_equalp_is_a_cons:
        lda OBJECT_PTR_ONE
        jsr store_and_inc_arguments
        lda OBJECT_PTR_ONE+1
        jsr store_and_inc_arguments
        lda OBJECT_PTR_TWO
        jsr store_and_inc_arguments
        lda OBJECT_PTR_TWO+1
        jsr store_and_inc_arguments
compare_cons_loop:
        ldy #$01
        lda (OBJECT_PTR_ONE)
        ora (OBJECT_PTR_ONE),y    ; first arg is zero?
        bne compare_cons_loop_continue ; no
        ora (OBJECT_PTR_TWO)
        ora (OBJECT_PTR_TWO),y
        beq cf_equalp_dec4_return_t ; both args are zero, return true
        bra cf_equalp_dec4_return_nil
compare_cons_loop_continue:
        lda (OBJECT_PTR_TWO)
        ora (OBJECT_PTR_TWO),y
        beq cf_equalp_dec4_return_nil ; first arg nonzero, second is zero
        ;; both args are nonzero, push them to the argstack
        lda (OBJECT_PTR_ONE)
        jsr store_and_inc_arguments
        lda (OBJECT_PTR_ONE),y
        jsr store_and_inc_arguments
        lda (OBJECT_PTR_TWO)
        jsr store_and_inc_arguments
        lda (OBJECT_PTR_TWO),y
        jsr store_and_inc_arguments
        jsr cf_equalp+1         ; and compare them
        dec ARGUMENTS
        dec ARGUMENTS
        lda (ARGUMENTS)
        ldy #$01
        ora (ARGUMENTS),y       ; returned NIL, they're not equal
        bne cf_equalp_dec4_return_nil
        ;; increase both argument ptrs by one addr (word)
        dec ARGUMENTS
        dec ARGUMENTS
        lda (ARGUMENTS)
        inc
        inc
        sta (ARGUMENTS)
        dec ARGUMENTS
        dec ARGUMENTS
        lda (ARGUMENTS)
        inc
        inc
        sta (ARGUMENTS)
        ;; and store them in the object ptrs
        sta OBJECT_PTR_ONE
        inc ARGUMENTS
        lda (ARGUMENTS)
        sta OBJECT_PTR_ONE+1
        inc ARGUMENTS
        lda (ARGUMENTS)
        sta OBJECT_PTR_TWO
        inc ARGUMENTS
        lda (ARGUMENTS)
        sta OBJECT_PTR_TWO+1
        bra compare_cons_loop
cf_equalp_dec4_return_nil:
        lda ARGUMENTS
        sec
        sbc #$04
        sta ARGUMENTS
cf_equalp_return_nil:
        lda ARGUMENTS
        sec
        sbc #$04
        sta ARGUMENTS
        jmp return_nil
cf_equalp_dec4_return_t:
        lda ARGUMENTS
        sec
        sbc #$04
        sta ARGUMENTS
cf_equalp_return_t:
        lda ARGUMENTS
        sec
        sbc #$04
        sta ARGUMENTS
        jmp return_t
cf_equalp_throw:
        lda ARGUMENTS
        sec
        sbc #$04
        sta ARGUMENTS
        jsr PRINT_NEWLINE
        lda #<msg_non_comparable
        sta PRINT_VECTOR
        lda #>msg_non_comparable
        sta PRINT_VECTOR+1
        jsr print_cstring
        lda OBJECT_PTR_ONE+1
        jsr PRBYTE
        lda OBJECT_PTR_ONE
        jsr PRBYTE
        jmp read_next_char
msg_non_comparable: .asciiz "? Unable to compare object at $"

cf_null:
        .byte TYPE_FCOM
        dec ARGUMENTS
        lda (ARGUMENTS)
        dec ARGUMENTS
        ora (ARGUMENTS)
        beq return_t
return_nil:
        lda #$00
        sta (ARGUMENTS)
        inc ARGUMENTS
        sta (ARGUMENTS)
        inc ARGUMENTS
        rts
return_t:
        lda #<atom_t
        sta (ARGUMENTS)
        inc ARGUMENTS
        lda #>atom_t
        sta (ARGUMENTS)
        inc ARGUMENTS
        rts

cf_car:
        .byte TYPE_FCOM
        rts

atom_nil:
        .byte TYPE_SYMB
        .asciiz "NIL"

atom_t:
        .byte TYPE_SYMB
        .asciiz "T"

atom_quit:
        .byte TYPE_SYMB
        .asciiz "QUIT"

atom_eq:
        .byte TYPE_SYMB
        .asciiz "EQ"

atom_equalp:
        .byte TYPE_SYMB
        .asciiz "EQUALP"

atom_car:
        .byte TYPE_SYMB
        .asciiz "CAR"

atom_cdr:
        .byte TYPE_SYMB
        .asciiz "CDR"

atom_quote:
        .byte TYPE_SYMB
        .asciiz "QUOTE"

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
