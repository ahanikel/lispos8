        .setcpu "65C02"
        .segment "BIOS"

ACIA_DATA   = $5000
ACIA_STATUS = $5001
ACIA_CMD    = $5002
ACIA_CTRL   = $5003

        ;; Input a character from the serial interface.
        ;; On return, carry flag indicates whether a key was pressed.
        ;; If a key was pressed, the key value will be in the A register.
        ;;
        ;; Modifies: flags, A

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
        
CHROUT:
        pha
        sta     ACIA_DATA
        lda     #$FF
@txdelay:
        dec
        bne     @txdelay
        pla
        rts
        
RESET:
        cld
        cli
        lda     #$1F            ; 8-N-1, 19200 baud.
        sta     ACIA_CTRL
        lda     #$0B            ; No parity, no echo, no interrupts.
        sta     ACIA_CMD
        jmp     WOZMON

        .segment "VECTORS"
        
        .word   $0F00           ; NMI vector
        .word   RESET           ; RESET vector
        .word   $0000           ; IRQ vector

        .include "wozmon.s"
