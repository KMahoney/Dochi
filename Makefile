
.PHONY: dochi

dochi:
	ghc -fglasgow-exts --make Main -o dochi

test:
	./dochi core.chi test.chi

clean:
	rm Dochi/*.hi Dochi/*.o dochi *.hi *.o
