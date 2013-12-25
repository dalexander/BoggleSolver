import Data.Graph.Inductive hiding (Node)
import Control.Monad               (guard, replicateM_)
import Data.List                   (nub, sort)

import Board
import Trie

solveSlow :: [String] -> Board -> [String]
solveSlow candidates board =
    filter (Board.containsWord board) candidates

solve :: Trie -> Board -> [String]
solve trie board =
    nub $ sort $
    concat [ allWordsFrom bgraph [startLetter] startNode |
             (startNode, startLetter) <- labNodes bgraph ]
    where
      bgraph = graph board
      isWord = Trie.containsWord trie
      wordsWithPrefix = Trie.wordsWithPrefix trie
      allWordsFrom :: BGraph -> String -> Node -> [String]
      allWordsFrom bgraph prefix node =
          let thisWord     = if (isWord prefix) then [prefix] else []
              furtherWords =
                  concat $ do
                    node' <- suc bgraph node
                    let letter' = label bgraph node'
                        prefix' = prefix ++ [letter']
                        bgraph' = delNode node bgraph
                    guard $ (not . null . wordsWithPrefix) prefix'
                    return $ allWordsFrom bgraph' prefix' node'
          in thisWord ++ furtherWords

main = do contents <- readFile "sowpods.txt"
          let dictionaryWords = lines contents
          let trie = Trie.fromList dictionaryWords
          --print $ solveSlow dictionaryWords sampleBoard
          --print $ solve trie sampleBoard
          replicateM_ 10 $ do
            board <- randomIO
            let soln = solve trie board
            putStrLn ((show board) ++ " --> " ++ (show $ length soln) ++ " words")
