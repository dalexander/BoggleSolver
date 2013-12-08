
Board: Board.hs
	ghc -O2 Board.hs -rtsopts -prof -auto-all

Trie: Trie.hs
	ghc -O2 Trie.hs -rtsopts -prof -auto-all

BoggleSolver: Board.hs Trie.hs Main.hs
	ghc -O2 --make

clean:
	rm -f *.hi *.o
	rm -f Board Trie Main

.PHONY: clean
