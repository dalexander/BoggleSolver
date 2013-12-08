{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Trie

testTrie =
    let t = fromList [ "A", "AARDVARK", "AARDVARKS", "AARDWOLF", "AARDWOLVES",
                       "VAN", "VANQUISH", "VANQUISHED", "VANQUISHER" , "ZED"  ]
        countWithPrefix p = (length . wordsWithPrefix p)  -- why not point free?
    in
      test [ 4 ~=? countWithPrefix "AA" t,
             4 ~=? countWithPrefix "VAN" t,
             3 ~=? countWithPrefix "VANQ" t,
             False ~=? containsWord "VANQ" t,
             True  ~=? containsWord "VANQUISH" t ]
