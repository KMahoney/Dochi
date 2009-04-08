
dochi: Dochi/Main.hs Dochi/Parse.hs
	ghc --make -iDochi Main -o dochi

clean:
	rm Dochi/*.hi Dochi/*.o dochi
