# To profile:
# ./Solver  +RTS -K32000000 -p -RTS

#GHC := ghc
#GHC := ghc -fllvm -O2 -rtsopts -prof -auto-all
GHC := ghc -O2 -fllvm

Solver: Board.hs Trie.hs Solver.hs
	$(GHC) --make $^ -o Solver

install-deps:
	cabal install bytestring-trie

clean:
	rm -f *.hi *.o
	rm -f Solver

.PHONY: clean install-deps

