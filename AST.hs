{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , PatternSynonyms
  , UndecidableInstances
  #-}

module AST where

import Data.Functor.Foldable (Fix(..))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Types

type ControlPoint = Int

data AST t
  = AddF t t t
  | MovF t t
  | MovIF t Block
  | RegLabelF Int
  | SeqF ControlPoint t t
  | SeqEndF
  deriving (Functor, Foldable, Show, Traversable)

pattern RegLabel      t        = Fix (RegLabelF t)
pattern Add           t1 t2 t3 = Fix (AddF t1 t2 t3)
pattern Mov           t1 t2    = Fix (MovF t1 t2)
pattern MovI          t1 t2    = Fix (MovIF t1 t2)
pattern Seq           t1 t2 t3 = Fix (SeqF t1 t2 t3)

type Program = Fix AST
