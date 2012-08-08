all:
	ghc -fno-cse -O2 -fforce-recomp -fplugin CSE.Plugin Traced.hs
clean:
	rm -f *.o *.hi Traced
