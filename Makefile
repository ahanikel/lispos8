OUTDIR = build

.PHONY: clean all

all: $(OUTDIR)/lispos8.bin $(OUTDIR)/lisp.bin

$(OUTDIR)/lispos8.bin: $(OUTDIR)/lispos8.o
	ld65 -C lispos8.cfg -o $@ -Ln $(OUTDIR)/lispos8.lbl $^

$(OUTDIR)/lisp.bin: $(OUTDIR)/lisp.o
	ld65 -C plain.cfg -o $@ $^
	sbcl --noinform --disable-debugger --userinit ~/.sbclrc.sic --load to-wozmon.lisp --eval '(to-wozmon "$@")' --quit | tail +7 > $(OUTDIR)/lisp.woz

$(OUTDIR):
	mkdir $@

$(OUTDIR)/lispos8.o: bios.s wozmon.s lcd.s lisp.s | $(OUTDIR)
	cat $^ > $(OUTDIR)/lispos8.s
	ca65 -o $@ $(OUTDIR)/lispos8.s

SYMDEFS = $(shell awk '/(CHRIN|CHROUT|WOZMON|PRINT_NEWLINE)/ { print "-D" substr($$3,2) "=\\$$" substr($$2, 3) }' < $(OUTDIR)/lispos8.lbl)

$(OUTDIR)/lisp.o: lisp.s $(OUTDIR)/lispos8.bin | $(OUTDIR)
	sed -e '/.org $$E000/s//.org $$0400/' $< > $(OUTDIR)/$<
	ca65 $(SYMDEFS) -o $@ $(OUTDIR)/$<

clean:
	rm -rf build
