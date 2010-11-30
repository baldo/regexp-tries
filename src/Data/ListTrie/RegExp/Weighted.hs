{-# LANGUAGE
        FlexibleContexts,
        FlexibleInstances,
        FunctionalDependencies,
        MultiParamTypeClasses,
        OverlappingInstances,
        UndecidableInstances
  #-}
{-# OPTIONS_GHC
        -fno-warn-orphans
  #-}

{- | This module provides 'delete' and 'lookup' functions for 'StringTrie'
     using regular expressions. The algorithm used is the one from the package
    'weighted-regexp': <http://sebfisch.github.com/haskell-regexp/>.
-}

module Data.ListTrie.RegExp.Weighted
    (
    -- * Trie types
      ListTrie
    , StringTrie

    -- * Construction of regular expressions
    , RegExp

    , eps
    , char
    , sym
    , psym

    , anySym
    , noMatch

    , alt
    , seq_

    , rep
    , rep1
    , opt
    , brep

    , perm

    -- * Expression Type for Combinators
    , Expr
    , ExprClass

    -- * Regular Expression Combinators
    , complement
    , intersection
    , union

    -- * Trie operations
    , lookup
    , delete

    , lookupW
    )
where

import Prelude hiding
    ( lookup
    , mod
    , null
    , pred
    )
import Data.ListTrie.ListTrie hiding
    ( delete
    , empty
    , intersection
    , lookup
    , union
    )
import Data.ListTrie.RegExp.Common
import Data.Maybe
import Data.Semiring
import Text.RegExp
import Text.RegExp.Internal

-- Storing weights within the Trie ---------------------------------------------

class StoreWeights v w v' | v v' -> w where
    store :: w -> v -> v'
    select :: m v -> m v' -> m v'

instance StoreWeights v Bool v where
    store _ v = v
    select m _ = m

instance StoreWeights v w (v, w) where
    store w v = (v, w)
    select _ m = m

-- Regular Expression lookup ---------------------------------------------------

instance WrapClass map emap sym
      => LookupRegExpClass (RegExp sym) map emap sym where
    lookupRegExp     = lookupReW 0 True id  empty
    lookupComplement = lookupReW 0 True not empty

lookupReW
    :: (WrapClass map emap sym, StoreWeights v w v', Weight sym sym w)
    => Int
    -> w
    -> (w -> w)
    -> (RegW w sym -> w)
    -> RegExp sym
    -> map v
    -> map v'
lookupReW n mark mod pred (RegExp re) t =
    wrap $ lookupRe n mark mod pred re $ unwrap t

lookupRe
    :: (KeyMap sym map, StoreWeights v w v', Weight sym sym w)
    => Int
    -> w
    -> (w -> w)
    -> (RegW w sym -> w)
    -> RegW w sym
    -> ListTrie map v
    -> ListTrie map v'
lookupRe _ _    _   _    _  NoListTrie       = NoListTrie
lookupRe n mark mod pred re t@(ListTrie tn tc)
    | n > 0 && not (active re) =
        if mod one' /= zero
            then NoListTrie
            else select t $ error "This should never haqppen!"
                        -- (KM.map (store one') t)
    | otherwise =
        if isJust tn' || not (null tc')
            then ListTrie tn' tc'
            else NoListTrie
    where
        one' = one
        tn'  = if w /= zero then fmap (store w) tn else Nothing
        w    = mod $ pred re
        tc'  = mapMaybeWithKey go tc

        go c ts =
            cleanup $ lookupRe (n + 1) zero mod final (shift mark (reg re) c) ts

        cleanup ts
            | null ts   = Nothing
            | otherwise = Just ts

-- Expression instances --------------------------------------------------------

instance ExprClass (RegExp sym) (RegExp sym) where
    toExpr = REExpr

instance ExprClass e (RegExp Char) => Show e where
    showsPrec _ e =
        case toExpr e of
            REExpr re ->
                showString "(RegExp<" . shows re . showString ">)"
            Complement e' ->
                showString "(Complement " . shows e' . showString ")"
            Intersection e1 e2 ->
                showString "(Intersection " . shows e1 . showChar ' '
                                            . shows e2 . showString ")"
            Union e1 e2 ->
                showString "(Union" . shows e1 . showChar ' '
                                    . shows e2 . showString ")"

-- Trie operations -------------------------------------------------------------

{- | Lookup the given regular expression in the Trie and return a Trie of
     results, containing also the calculated weights.

     Note: As weights don't make much sense with delete (would all be 'zero'),
     there is no 'deleteW'.

     Note: Since there doesn't seem to be a way to efficiently allow combining
     of weighted regular expressions, there are no combinators for this for now.
-}
lookupW
    :: (WrapClass map emap sym, StoreWeights v w (v, w), Weight sym sym w)
    => RegExp sym
    -> map v
    -> map (v, w)
lookupW (RegExp re) t = wrap $ lookupRe 0 one id empty (weighted re) $ unwrap t

-- Optimzation of combined Regular Expressions ---------------------------------

instance OptimizeClass (RegExp sym) where
    optimizeExpr e@(REExpr _) =
        e

    optimizeExpr (Complement e) =
        case optimizeExpr e of
            (Complement e') ->
                e'

            (Intersection (Complement e1') (Complement e2')) ->
                Union e1' e2'

            e' ->
                Complement e'

    optimizeExpr (Intersection e1 e2) =
        case (optimizeExpr e1, optimizeExpr e2) of
            (e1', e2') ->
                Intersection (optimizeExpr e1) (optimizeExpr e2)

    optimizeExpr (Union e1 e2) =
        case (optimizeExpr e1, optimizeExpr e2) of
            (REExpr r1, REExpr r2) ->
                REExpr $ r1 `alt` r2

            -- Complement of regular expressions is cheap.
            (e1'@(Complement (REExpr _)), e2') ->
                Union e1' e2'
            (e1', e2'@(Complement (REExpr _))) ->
                Union e1' e2'

            -- Evaluating Complement once is cheaper than doing it twice.
            (Complement e1', Complement e2') ->
                Complement $ Intersection e1' e2'

            (e1', e2') ->
                Union e1' e2'

