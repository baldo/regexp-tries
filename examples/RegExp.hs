{-# LANGUAGE
        MultiParamTypeClasses,
        TemplateHaskell,
        UndecidableInstances
  #-}

module RegExp where

import Prelude hiding (lookup)

import Data.ListTrie.RegExp

import Data.Bits

import Data.KeyMap (empty, insert, toList)
import qualified Data.KeyMap as KeyMap
import Data.Derive.Trie

$(deriveTrie [''Bool])

toBits :: Int -> [Bool]
toBits n = map (testBit n) [0..7]

fromBits :: [Bool] -> Int
fromBits bs = foldr (\ b n -> let n' = shiftL n 1
                              in  if b then n' + 1 else n') 0 bs

exampleTrie :: ListTrie BoolTrie Int
exampleTrie = foldr (\ n -> insert (toBits n) n) empty [0..255]

re1 = rep (sym True)
re2 = rep (sym False)
re3 = rep (sym True `seq_` sym False)
re4 = rep anySym `seq_` sym False `seq` sym True `seq_` rep anySym
re5 = sym True `seq_` rep anySym

