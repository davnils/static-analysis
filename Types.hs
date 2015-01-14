module Types where

import Algebra.Lattice (JoinSemiLattice)

type Block = Int

class (Eq t, JoinSemiLattice t) => InterpreterLattice t
  where
  add :: t -> t -> t
  abstract :: Block -> t
  testZero :: t -> (Bool, Bool) -- [t] for higher precision
