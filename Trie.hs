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

type Trie = T.Trie ()

fromList :: [String] -> Trie
fromList words = T.fromList [(B.pack word, ()) | word <- words]

fromFile :: FilePath -> IO Trie
fromFile filename = do
  contents <- readFile filename
  let t = fromList $ lines contents
  return t

wordsWithPrefix :: Trie -> String -> [String]
wordsWithPrefix t prefix =
    let prefix' = B.pack prefix in
    T.toListBy (\k v -> B.unpack k) (T.submap prefix' t)

containsWord :: Trie -> String -> Bool
containsWord t word =
    let word' = (B.pack word) in
    T.member word' t

main = do
  t <- fromFile "sowpods.txt"
  print $ (null . wordsWithPrefix t) "AA"
  print $ (length . wordsWithPrefix t) "AA"
  print $ wordsWithPrefix t "AA"
  print $ wordsWithPrefix t "VANQ"
  print $ containsWord t "VANQ"
  print $ containsWord t "VANQUISH"
