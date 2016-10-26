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

data RegisterType
  = Stack
  | Other Int
  deriving (Show)

-- add push/pop

data AST t
  = AddF t t t
  | FetchF t t
  | JmpF ControlPoint
  | JmpZF t ControlPoint
  | MovF t t
  | MovIF t Block
  | RegLabelF RegisterType
  | SeqF ControlPoint t t
  | SeqEndF
  | WriteF t t
  deriving (Functor, Foldable, Show, Traversable)

pattern RegLabel      t1       = Fix (RegLabelF t1)
pattern Add           t1 t2 t3 = Fix (AddF t1 t2 t3)
pattern Fetch         t1 t2    = Fix (FetchF t1 t2)
pattern Jmp           t1       = Fix (JmpF t1)
pattern JmpZ          t1 t2    = Fix (JmpZF t1 t2)
pattern Mov           t1 t2    = Fix (MovF t1 t2)
pattern MovI          t1 t2    = Fix (MovIF t1 t2)
pattern Seq           t1 t2 t3 = Fix (SeqF t1 t2 t3)
pattern Write         t1 t2    = Fix (WriteF t1 t2)

type Program = Fix AST
