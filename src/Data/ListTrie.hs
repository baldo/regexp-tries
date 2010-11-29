{- | This module provides 'StringTrie' and 'ListTrie' as an instance of
     'KeyMap'.
-}

module Data.ListTrie
    ( -- * Trie types
      ListTrie
    , StringTrie

      -- * 'KeyMap' module providing Trie operations
    , module Data.KeyMap
    )
where

import Data.ListTrie.ListTrie
import Data.KeyMap

