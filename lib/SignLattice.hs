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
   Any \/ _            = Any
   None \/ other       = other

   Pos \/ NonZero      = NonZero
   Pos \/ NonNeg       = NonNeg
   Pos \/ NonPos       = Any
   Pos \/ Zero         = NonNeg
   Pos \/ Neg          = NonZero
   Pos \/ Pos          = Pos

   Neg \/ NonPos       = NonPos
   Neg \/ NonZero      = NonZero
   Neg \/ NonNeg       = Any
   Neg \/ Zero         = NonPos
   Neg \/ Neg          = Neg

   Zero \/ NonPos      = NonPos
   Zero \/ NonZero     = Any
   Zero \/ NonNeg      = NonNeg
   Zero \/ Zero        = Zero

   NonZero \/ NonNeg   = Any
   NonZero \/ NonPos   = Any
   NonZero \/ NonZero  = NonZero

   a1 \/ a2            = a2 \/ a1

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

  -- | Returns (should follow branch, should not follow branch)
  testZero Zero    = (True, False)
  testZero NonZero = (False, True)
  testZero Pos     = (False, True)
  testZero Neg     = (False, True)
  testZero other   = (True, True)

  concretize Zero = [0]
  concretize _    = [undefined, undefined]
