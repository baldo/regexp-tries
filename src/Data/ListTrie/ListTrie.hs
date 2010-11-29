{-# LANGUAGE
        MultiParamTypeClasses,
        TemplateHaskell,
        TypeSynonymInstances,
        UndecidableInstances
  #-}
{-# OPTIONS_GHC
        -fno-warn-missing-signatures
        -fno-warn-unused-binds
        -fno-warn-unused-matches
  #-}

-- Hidden module used for deriving Trie types.

module Data.ListTrie.ListTrie
    ( StringTrie (..)
    , ListTrie (..)

    , module KeyMap
    )
where

import Data.Derive.Trie
import Data.KeyMap
import qualified Data.KeyMap as KeyMap

$(deriveTrie [''String])

