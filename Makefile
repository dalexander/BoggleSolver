# To profile:
# ./Solver  +RTS -K32000000 -p -RTS

#GHC := ghc -fllvm -O2 -rtsopts -prof -auto-all
#GHC := ghc -O2 
GHC := ghc

Solver: Board.hs Trie.hs Solver.hs
	$(GHC) --make $^ -o Solver

clean:
	rm -f *.hi *.o
	rm -f Solver

.PHONY: clean

