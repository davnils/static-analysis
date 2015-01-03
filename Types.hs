module Types where

import Algebra.Lattice (JoinSemiLattice)

type Block = Int

class JoinSemiLattice t => InterpreterLattice t
  where
  add :: t -> t -> t
  abstract :: Block -> t
