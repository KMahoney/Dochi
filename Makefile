
.PHONY: dochi

dochi:
	ghc --make -iDochi Main -o dochi

clean:
	rm Dochi/*.hi Dochi/*.o dochi
