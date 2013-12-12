{-# LANGUAGE OverloadedStrings #-}

module Trie
    ( Trie,
      fromList,
      fromFile,
      wordsWithPrefix,
      containsWord
    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Trie as T

type S = B.ByteString
type Trie = T.Trie ()

fromList :: [S] -> Trie
fromList words = T.fromList [(word, ()) | word <- words]

fromFile :: FilePath -> IO Trie
fromFile filename = do
  contents <- B.readFile filename
  let t = fromList $ B.lines contents
  return t

wordsWithPrefix :: S -> Trie -> [S]
wordsWithPrefix prefix t =
    T.toListBy const (T.submap prefix t)

containsWord :: S -> Trie -> Bool
containsWord word t = T.member word t

main = do
  t <- fromFile "sowpods.txt"
  print $ (null . wordsWithPrefix "AA") t
  print $ (length . wordsWithPrefix "AA") t
  print $ wordsWithPrefix "AA" t
  print $ wordsWithPrefix "VANQ" t
  print $ containsWord "VANQ" t
  print $ containsWord "VANQUISH" t
