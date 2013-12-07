import Data.Graph.Inductive
import Data.Graph.Inductive.Graphviz

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
-- Verify that we are doing the right thing by looking at graphviz
-- output in neato
--
main = let g = fromString ['A' .. 'Z']
           gMinus = delNode 6 g
       in
         do writeFile "/tmp/g.dot"      (graphviz' g)
            writeFile "/tmp/gMinus.dot" (graphviz' gMinus)
