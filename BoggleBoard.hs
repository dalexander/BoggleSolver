import Data.Graph.Inductive
import Data.Graph.Inductive.Graphviz
import Data.Maybe (fromJust)

type Board = Gr Char ()

--
-- Can this be made more elegant?
--
fromString :: String -> Gr Char ()
fromString s =
    insEdges [(i, i-1, ()) | i <- [1..16], (i `mod` 4) /= 1] $ -- LEFT
    insEdges [(i, i+1, ()) | i <- [1..16], (i `mod` 4) /= 0] $ -- RIGHT
    insEdges [(i, i-4, ()) | i <- [1..16], i > 4]            $ -- UP
    insEdges [(i, i+4, ()) | i <- [1..16], i < 13]           $ -- DOWN
    insNodes (zip [1..16] s) empty

--
-- Can just map this over the dictionary to do an inefficent search...
--
containsWord :: String -> Board -> Bool
containsWord word board  =
    any (containsWordFrom word board) (nodes board)
    where
      containsWordFrom ""     _     _        = True
      containsWordFrom (w:ws) board node
          | isEmpty board                    = False
          | (w /= fromJust (lab board node)) = False
          | otherwise =
              any id $ do
                node' <- neighbors board node
                let board' = delNode node board
                return $ containsWordFrom ws board' node'



--
-- Verify that we are doing the right thing by looking at graphviz
-- output in neato
--
main1 = let g      = fromString ['A' .. 'Z']
            gMinus = delNode 6 g
        in
          do writeFile "/tmp/g.dot"      (graphviz' g)
             writeFile "/tmp/gMinus.dot" (graphviz' gMinus)

board :: Board
board = fromString ("REUE" ++
                    "ALST" ++
                    "QGSE" ++
                    "TNIB")


--
-- Try finding the words in a real board
--
main2 = let board = fromString ("REUE" ++
                                "ALST" ++
                                "QGSE" ++
                                "TNIB")
        in do print ""
              print "Checking for words..."
              print $ containsWord "NIB"  board
              print $ containsWord "XEROX" board

main = main1 >> main2
