{-# LANGUAGE BangPatterns #-}

module Board ( Board,
               Node,
               label,
               fromString,
               containsWord,
               sampleBoard )
where


import Data.Graph.Inductive hiding (Node)
import Data.Graph.Inductive.Graphviz
import Data.Maybe (fromJust)
import Data.List (foldl')

-- Graph representation
type Board = Gr Char ()
type Node = Int

label :: Board -> Node -> Char
label board node = fromJust $ lab board node

-- Infinity-norm distance
type Vec2 = (Int, Int)

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
containsWord :: Board -> String -> Bool
containsWord board word  =
    any (containsWordFrom word board) (nodes board)

containsWordFrom (w:ws) board node
    | isEmpty board                = False
    | (w /= label board node)      = False
    | null ws                      = True
    | otherwise =
        any id $ do
          node' <- suc board node
          let !board' = delNode node board
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

sampleBoard :: Board
sampleBoard = fromString ("REUE" ++
                          "ALST" ++
                          "QGSE" ++
                          "TNIB")
--
-- Try finding the words in a real board
--
main2 = do print ""
           print "Checking for words..."
           print $ containsWord sampleBoard "NIB"
           print $ containsWord sampleBoard "NIBS"
           print $ containsWord sampleBoard "XEROX"
           print $ containsWord sampleBoard "LEAR"

main = main1 >> main2
