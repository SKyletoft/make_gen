makegen: Makegen.hs
	ghc -O3 Makegen.hs -o makegen

install: makegen
	mkdir ${out}/bin
	mv makegen ${out}/bin

clean:
	-rm makegen
	-rm Makegen.hi
	-rm Makegen.o

.PHONY: install clean
