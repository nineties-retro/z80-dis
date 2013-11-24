src = \
	z80-memory.el \
	z80-dis-output.el \
	z80-decoder.el \
	z80-dis-output-std.el \
	z80-dis-output-stream.el \
	z80-dis.el \
	z80-memory-buffer.el \
	z80-memory-vector.el \
	z80-ram.el \
	z80-rom.el

obj = $(src:%.el=%.elc)

.PHONY: all

all:	$(obj)

%.elc:	%.el
	emacs -q -batch -L . -f batch-byte-compile $<

clean:
	rm -f $(obj)

distclean: clean
	find . -name '*~' -print0 | xargs -0 rm -f
