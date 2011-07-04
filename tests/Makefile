# replace with path to your GHC with strict-ghc-plugin installed
HC=~/ghc-head/bin/ghc

all:
	$(HC) -fno-cse -O2 -fforce-recomp -fplugin CSE.Plugin Traced.hs
clean:
	rm -f *.o *.hi Traced
