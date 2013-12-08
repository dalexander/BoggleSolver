import Data.Graph.Inductive
import Data.Graph.Inductive.Graphviz
import Data.Maybe (fromJust)
import Data.List (foldl')

type Board = Gr Char ()

type Vec2 = (Int, Int)

-- Infinity-norm distance
distInf :: Vec2 -> Vec2 -> Int
distInf (x, y) (x', y') = max (abs (x-x')) (abs (y-y'))

fromString :: String -> Board
fromString s = foldl' update start [1..n]
    where
      side = 4
      n = (side * side)
      start = (insNodes (zip [1..n] s) empty)
      pos i = (i - 1) `divMod` side
      update :: Board -> Int -> Board
      update board i =
          insEdges
          [ (i, i', ()) | i' <- [1..n],
            (pos i) `distInf` (pos i') <= 1,
            i /= i' ]
          board

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
