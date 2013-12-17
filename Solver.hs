import Data.Graph.Inductive hiding (Node)
import Control.Monad (guard)
import Data.List (nub, sort)

import Board
import Trie


solveSlow :: [String] -> Board -> [String]
solveSlow candidates board =
    filter (Board.containsWord board) candidates

solve :: Trie -> Board -> [String]
solve trie board =
    nub $ sort $
    concat [ allWordsFrom board [startLetter] startNode |
             (startNode, startLetter) <- labNodes board ]
    where
      isWord = Trie.containsWord trie
      wordsWithPrefix = Trie.wordsWithPrefix trie
      allWordsFrom :: Board -> String -> Node -> [String]
      allWordsFrom board prefix node =
          let thisWord     = if (isWord prefix) then [prefix] else []
              furtherWords =
                  concat $ do
                    node' <- suc board node
                    let letter' = label board node'
                        prefix' = prefix ++ [letter']
                        board' = delNode node board
                    guard $ (not . null . wordsWithPrefix) prefix'
                    return $ allWordsFrom board' prefix' node'
          in thisWord ++ furtherWords

main = do contents <- readFile "sowpods.txt"
          let dictionaryWords = lines contents
          let trie = Trie.fromList dictionaryWords
          --print $ solveSlow dictionaryWords sampleBoard
          print $ solve trie sampleBoard
          print $ solve trie sampleBoard2
          print $ solve trie sampleBoard3
