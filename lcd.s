        ;; This file was taken from Ben Eater's videos
        .segment "BIOS"

lcd_wait:
        pha
        lda #$F0                ; LCD data is input
        sta DDRB
lcdbusy:
        lda #RW
        sta PORTB
        lda #(RW | E)
        sta PORTB
        lda PORTB               ; read high nibble
        pha                     ; and put on stack since it has the busy flag
        lda #RW
        sta PORTB
        lda #(RW | E)
        sta PORTB
        lda PORTB               ; read low nibble
        pla                     ; get high nibble off stack
        and #%00001000
        bne lcdbusy

        lda #RW
        sta PORTB
        lda #$FF                ; LCD data is output
        sta DDRB
        pla
        rts

LCDINIT:
        lda #$FF                ; set all pins on port B to output
        sta DDRB

        lda #%00000011          ; set 8-bit mode
        sta PORTB
        ora #E
        sta PORTB
        and #%00001111
        sta PORTB

        lda #%00000011          ; set 8-bit mode
        sta PORTB
        ora #E
        sta PORTB
        and #%00001111
        sta PORTB

        lda #%00000011          ; set 8-bit mode
        sta PORTB
        ora #E
        sta PORTB
        and #%00001111
        sta PORTB

        ;; Okay, now we're really in 8-bit mode.
        ;; Command to get to 4-bit mode ought to work now
        lda #%00000010          ; set 4-bit mode
        sta PORTB
        ora #E
        sta PORTB
        and #%00001111
        sta PORTB

        lda #%00101000          ; set 4-bit mode, 2-line display, 5x8 font
        jsr lcd_instruction
        lda #%00001110          ; display on, cursor on, blink off
        jsr lcd_instruction
        lda #%00000110          ; increment and shift cursor, don't shift display
        jsr lcd_instruction
        lda #%00000001          ; clear display
        jsr lcd_instruction
        rts

lcd_instruction:
        jsr lcd_wait
        pha
        lsr
        lsr
        lsr
        lsr                     ; send high 4 bits
        sta PORTB
        ora #E
        sta PORTB
        eor #E
        sta PORTB
        pla
        and #$0F                ; send low 4 bits
        sta PORTB
        ora #E
        sta PORTB
        eor #E
        sta PORTB
        rts

lcd_print_char:
        jsr lcd_wait
        pha
        lsr
        lsr
        lsr
        lsr                     ; send high 4 bits
        ora #RS
        sta PORTB
        ora #E
        sta PORTB
        eor #E
        sta PORTB
        pla
        and #$0F                ; send low 4 bits
        ora #RS
        sta PORTB
        ora #E
        sta PORTB
        eor #E
        sta PORTB
        rts
