
.PHONY: dochi

dochi:
	ghc -fglasgow-exts --make Main -o dochi

clean:
	rm Dochi/*.hi Dochi/*.o dochi *.hi *.o
