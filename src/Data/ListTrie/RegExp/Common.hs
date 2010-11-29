{-# LANGUAGE
        FlexibleContexts,
        FlexibleInstances,
        FunctionalDependencies,
        MultiParamTypeClasses
  #-}

{- | This module provides functionality that all regular expression
     implementations share.
-}

module Data.ListTrie.RegExp.Common
    ( WrapClass (..)

    -- * Expression Type for Combinators
    , Expr (..)
    , ExprClass (..)

    -- * Regular Expression Combinators
    , complement
    , intersection
    , union

    -- * Trie operations
    , LookupRegExpClass (..)
    , lookup
    , delete

    -- * Optimzation of combined Regular Expressions
    , OptimizeClass (..)
    , optimize
    )
where

import Prelude hiding (lookup)
import qualified Data.KeyMap as KM
import Data.ListTrie.ListTrie hiding
    ( delete
    , intersection
    , lookup
    , union
    )
import Data.Map (Map)

-- Wrapping and unwrapping of StringTrie ---------------------------------------

{- | Allows the usage of 'StringTrie' and 'ListTrie' the same way.
-}
class (KeyMap [sym] map, KeyMap sym emap)
   => WrapClass map emap sym | map -> emap where
    wrap   :: ListTrie emap v -> map v
    unwrap :: map v -> ListTrie emap v

instance WrapClass StringTrie (Map Char) Char where
    wrap   = StringTrie
    unwrap = unStringTrie

instance (KeyMap sym emap) => WrapClass (ListTrie emap) emap sym where
    wrap   = id
    unwrap = id

-- Regular Expression Combinators ----------------------------------------------

{- | Expressions may be build by using the combinators 'complement',
     'intersection' and 'union'. They can then be used with 'lookup' and
     'delete'.
-}
data Expr regexp = REExpr regexp
                 | Complement (Expr regexp)
                 | Intersection (Expr regexp) (Expr regexp)
                 | Union (Expr regexp) (Expr regexp)

-- | Allows to also use regular expressions directly instead of 'Expr' types.
class ExprClass e regexp | e -> regexp where
    toExpr :: e -> (Expr regexp)

instance ExprClass (Expr regexp) regexp where
    toExpr = id

-- | Perform the operation with the complement of the expression.
complement
    :: ExprClass e regexp
    => e        -- ^ an expression
    -> Expr regexp -- ^ the complement
complement = Complement . toExpr

-- | Intersection of the result of two expressions.
intersection
    :: (ExprClass e1 regexp, ExprClass e2 regexp)
    => e1
    -> e2
    -> Expr regexp
intersection e1 e2 = Intersection (toExpr e1) (toExpr e2)

-- | Union of the result of two expressions.
union
    :: (ExprClass e1 regexp, ExprClass e2 regexp)
    => e1
    -> e2
    -> Expr regexp
union e1 e2 = (Union (toExpr e1) (toExpr e2))

-- Trie operations -------------------------------------------------------------

class (WrapClass map emap sym, OptimizeClass regexp)
   => LookupRegExpClass regexp map emap sym | regexp -> sym where
    lookupRegExp
        :: regexp
        -> map v
        -> map v
    lookupComplement
        :: regexp
        -> map v
        -> map v

-- | Lookup the given expression in the given Trie and return a Trie of results.
lookup
    :: (LookupRegExpClass (regexp sym) map emap sym, ExprClass e (regexp sym))
    => e     -- ^ expression to lookup
    -> map v -- ^ Trie on wich to perform the lookup
    -> map v -- ^ resulting Trie
lookup = lookupExpr . optimize

-- | Delete entries matching the expression from the given Trie.
delete
    :: (LookupRegExpClass (regexp sym) map emap sym, ExprClass e (regexp sym))
    => e     -- ^ expression
    -> map v -- ^ Trie to delete from
    -> map v -- ^ Trie missing the entries that matched the expression.
delete = lookupExpr . optimize . complement

-- | Evaluates the given expression on the given Trie.
lookupExpr
    :: LookupRegExpClass (regexp sym) map emap sym
    => Expr (regexp sym)
    -> map v
    -> map v
lookupExpr (REExpr re)                  t = lookupRegExp re t
lookupExpr (Complement (REExpr re))     t = lookupComplement re t
lookupExpr (Complement (Complement e))  t = lookupExpr e t
lookupExpr (Complement e)               t = difference t $ lookupExpr e t
lookupExpr (Intersection e1 e2)         t = lookupExpr e2 (lookupExpr e1 t)
lookupExpr (Union e1 e2)                t = KM.union t1 t2
    where
        t1 = lookupExpr e1 t
        t2 = lookupExpr e2 t

-- Optimzation of combined Regular Expressions ---------------------------------

class OptimizeClass regexp where
    -- | Optimize the given expression.
    optimizeExpr
        :: Expr regexp -- ^ expression
        -> Expr regexp -- ^ optimized expression

-- | Optimize the given expression.
optimize
    :: (ExprClass e regexp, OptimizeClass regexp)
    => e           -- ^ expression
    -> Expr regexp -- ^ optimized expression
optimize = optimizeExpr . toExpr

