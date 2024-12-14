        .setcpu "65C02"
        .segment "BIOS"

ACIA_DATA   = $5000
ACIA_STATUS = $5001
ACIA_CMD    = $5002
ACIA_CTRL   = $5003

        jmp COLD_START

LOAD:
        rts

SAVE:
        rts

ISCNTC:
        rts
        
        ;; Input a character from the serial interface.
        ;; On return, carry flag indicates whether a key was pressed.
        ;; If a key was pressed, the key value will be in the A register.
        ;;
        ;; Modifies: flags, A

MONRDKEY:       
CHRIN:
        lda     ACIA_STATUS
        and     #$08
        beq     @no_keypressed
        lda     ACIA_DATA
        jsr     CHROUT          ; echo
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
        and     #~CTS           ; we're clear to send
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
        cld
        cli
        lda     #$1F            ; 8-N-1, 19200 baud.
        sta     ACIA_CTRL
        lda     #$0B            ; No parity, no echo, no interrupts.
        sta     ACIA_CMD
        jmp     WOZMON

        .segment "RESETVEC"
        
        .word   $0F00           ; NMI vector
        .word   RESET           ; RESET vector
        .word   $0000           ; IRQ vector

        .include "wozmon.s"
