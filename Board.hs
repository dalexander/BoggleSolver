{-# LANGUAGE BangPatterns #-}

module Board ( Board(..),
               BGraph,
               Node,
               label,
               fromString,
               containsWord,
               sampleBoard,
               arbitrary,
               randomIO
             )
where

import Data.Graph.Inductive hiding   (Node)
import Data.Graph.Inductive.Graphviz (graphviz')
import Data.Maybe                    (fromJust)
import Data.List                     (foldl')
import Control.Applicative           ((<$>))
import Control.Monad                 (replicateM)
import Test.QuickCheck               (elements)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import System.Random                 (newStdGen, StdGen)

--
-- Board representation
--
type BGraph = Gr Char ()

data Board = Board { string :: String
                   , graph  :: BGraph
                   }

instance Show Board where
    show board = "Board " ++ (show (string board))

type Node = Int

label :: BGraph -> Node -> Char
label bg node = fromJust $ lab bg node

-- Infinity-norm distance
type Vec2 = (Int, Int)

distInf :: Vec2 -> Vec2 -> Int
distInf (x, y) (x', y') = max (abs (x-x')) (abs (y-y'))

fromString :: String -> Board
fromString s =
    Board { string=s, graph=(foldl' update start [1..n]) }
    where
      side = 4
      n = (side * side)
      start = (insNodes (zip [1..n] s) empty)
      pos i = (i - 1) `divMod` side
      update :: BGraph -> Int -> BGraph
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
    let bg = graph board in
    any (containsWordFrom word bg) (nodes bg)

containsWordFrom :: String -> BGraph -> Node -> Bool
containsWordFrom (w:ws) bg node
    | isEmpty bg           = False
    | (w /= label bg node) = False
    | null ws              = True
    | otherwise =
        any id $ do
          node' <- suc bg node
          let !bg' = delNode node bg
          return $ containsWordFrom ws bg' node'

--
-- Random boards
--
instance Arbitrary Board where
    arbitrary = fromString <$> replicateM 16 (elements ['A'..'Z'])

random :: StdGen -> Board
random gen = unGen (arbitrary :: Gen Board) gen 1

randomIO :: IO Board
randomIO = do
  gen <- newStdGen
  return $ random gen

sampleBoard :: Board
sampleBoard = fromString ("ARUG" ++
                          "ULAS" ++
                          "PINA" ++
                          "CHKA")

--
-- Verify that we are doing the right thing by looking at graphviz
-- output in neato
--
main1 = let b      = fromString ['A' .. 'Z']
            bMinus = b { graph = delNode 6 (graph b) }
        in
          do writeFile "/tmp/g.dot"      (graphviz' $ graph b)
             writeFile "/tmp/gMinus.dot" (graphviz' $ graph bMinus)

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
