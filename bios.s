        ;; This file was taken from Ben Eater's videos
        .setcpu "65C02"
        .debuginfo

        .zeropage
READ_PTR:       .res 1
WRITE_PTR:      .res 1

        .segment "INPUT_BUFFER"
INPUT_BUFFER:   .res $100

        .segment "BIOS"

ACIA_DATA   = $5000
ACIA_STATUS = $5001
ACIA_CMD    = $5002
ACIA_CTRL   = $5003

PORTA       = $6001
PORTB       = $6000
DDRA        = $6003
DDRB        = $6002

E   = %01000000
RW  = %00100000
RS  = %00010000
CTS = %00000001

LOAD:
        rts

SAVE:
        rts

        ;; Input a character from the serial interface.
        ;; On return, carry flag indicates whether a key was pressed.
        ;; If a key was pressed, the key value will be in the A register.
        ;;
        ;; Modifies: flags, A

MONRDKEY:
CHRIN:
        jsr     BUFFER_SIZE
        beq     @no_keypressed
        phx
        jsr     READ_BUFFER
        jsr     CHROUT
        pha
        jsr     BUFFER_SIZE
        cmp     #$B0
        bcs     @mostly_full
        lda     PORTA
        and     #<~CTS
        sta     PORTA
@mostly_full:
        pla
        plx
        sec
        rts
@no_keypressed:
        clc
        rts

MONCOUT:
CHROUT:
        pha
        sta     ACIA_DATA
        lda     #$FF
@txdelay:
        dec
        bne     @txdelay
        pla
        rts

PRINT_NEWLINE:
TERPRI: 
        LDA     #$0D
        JSR     CHROUT
        LDA     #$0A
        JMP     CHROUT

PRINT_GREETING:
        JSR     PRINT_NEWLINE
        LDX     #$00
PRINT_GREETING_LOOP:
        LDA     GREETING,X
        BEQ     PRINT_GREETING_DONE
        JSR     CHROUT
        INX
        BRA     PRINT_GREETING_LOOP
PRINT_GREETING_DONE:
        JSR     PRINT_NEWLINE
        RTS

        ;; Initialize the circular input buffer
        ;; Modifies: flags, A
INIT_BUFFER:
        lda     READ_PTR
        sta     WRITE_PTR
        lda     DDRA
        ora     #CTS            ; set CTS bit to output
        sta     DDRA
        lda     PORTA
        and     #<~CTS          ; we're clear to send
        sta     PORTA
        rts

        ;; Write a character from the A register to the circular buffer
        ;; Modifies: flags, X
WRITE_BUFFER:
        ldx     WRITE_PTR
        sta     INPUT_BUFFER,x
        inc     WRITE_PTR
        rts

        ;; Read a character from the circular buffer, put in A
        ;; Modifies: flags, A, X
READ_BUFFER:
        ldx     READ_PTR
        lda     INPUT_BUFFER,x
        inc     READ_PTR
        rts

        ;; Return in A the number of unread bytes in the circular buffer
        ;; Modifies: flags, A
BUFFER_SIZE:
        lda     WRITE_PTR
        sec
        sbc     READ_PTR
        rts

IRQ_HANDLER:
        pha
        phx
        lda     ACIA_STATUS
        ;; for now assume the only source of interrupts is incoming data
        lda     ACIA_DATA
        jsr     WRITE_BUFFER
        jsr     BUFFER_SIZE
        cmp     #$F0
        bcc     @not_full
        lda     PORTA
        ora     #CTS
        sta     PORTA
@not_full:
        plx
        pla
        rti

RESET:
        sei
        ldx     #$FF
        txs
        cld
        jsr     INIT_BUFFER
        cli
        lda     #$1F            ; 8-N-1, 19200 baud.
        sta     ACIA_CTRL
        lda     #$89            ; No parity, no echo, rx interrupts.
        sta     ACIA_CMD

        jsr     LCDINIT
        jsr     PRINT_GREETING
        jmp     WOZMON

GREETING:       .asciiz "Starting Wozmon..."

        .segment "RESETVEC"

        .word   $0F00           ; NMI vector
        .word   RESET           ; RESET vector
        .word   IRQ_HANDLER     ; IRQ vector
