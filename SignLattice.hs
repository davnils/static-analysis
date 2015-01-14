module SignLattice where

import Algebra.Lattice
import Types

data SignLattice
  = None
  | Pos
  | Neg
  | Zero
  | NonPos
  | NonNeg
  | NonZero
  | Any
  deriving (Eq, Show)

instance JoinSemiLattice SignLattice
  where
  join Any _            = Any
  join None other       = other

  join Pos NonZero      = NonZero
  join Pos NonNeg       = NonNeg
  join Pos NonPos       = Any
  join Pos Zero         = NonNeg
  join Pos Neg          = NonZero
  join Pos Pos          = Pos

  join Neg NonPos       = NonPos
  join Neg NonZero      = NonZero
  join Neg NonNeg       = Any
  join Neg Zero         = NonPos
  join Neg Neg          = Neg

  join Zero NonPos      = NonPos
  join Zero NonZero     = Any
  join Zero NonNeg      = NonNeg
  join Zero Zero        = Zero

  join NonZero NonNeg   = Any
  join NonZero NonPos   = Any
  join NonZero NonZero  = NonZero

  join a1 a2            = join a2 a1

instance BoundedJoinSemiLattice SignLattice
  where
  bottom = None

evaluateBinOp
  :: (SignLattice -> SignLattice -> SignLattice)
  -> SignLattice
  -> SignLattice
  -> SignLattice

evaluateBinOp f e1 e2 = joins [f x1 x2 | x1 <- go e1, x2 <- go e2]
  where
  go NonPos  = [Neg, Zero]
  go NonNeg  = [Zero, Pos]
  go NonZero = [Neg, Pos]
  go None    = []
  go atomic  = [atomic]

instance InterpreterLattice SignLattice where
  add t1 t2 = evaluateBinOp go t1 t2
    where
    go Pos Pos   = Pos
    go Pos Zero  = Pos
    go Pos Neg   = Any
    go Zero Zero = Zero
    go Zero Neg  = Neg
    go Neg Neg   = Neg
    go a b       = go b a

  abstract n
    | n < 0  = Neg
    | n == 0  = Zero
    | n > 0  = Pos

  testZero Zero    = (True, False)
  testZero NonZero = (False, True)
  testZero Pos     = (False, True)
  testZero Neg     = (False, True)
  testZero other   = (True, True)
