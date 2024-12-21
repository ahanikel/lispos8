OUTDIR = build

$(OUTDIR)/lispos8.bin: $(OUTDIR)/bios.o $(OUTDIR)/wozmon.o $(OUTDIR)/lisp.o
	ld65 -C lispos8.cfg -o $@ $^

$(OUTDIR):
	mkdir $@

$(OUTDIR)/%.o: %.s | $(OUTDIR)
	ca65 -o $@ $<

.PHONY: clean

clean:
	rm -rf build
