{-# LANGUAGE
    PatternSynonyms,
    TemplateHaskell
  #-}

module Interpreter where

import           Algebra.Lattice (join)
import           AST
import           Control.Applicative ((<$>))
import           Control.Monad (liftM2, when, unless)
import           Control.Monad.Trans (lift, liftIO, MonadIO)
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as S
import           Control.Lens
import           Data.Int (Int8)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.IntMap.Lazy as IML
import           Data.IntMap ((!))
import           Prelude hiding (break)
import           Types


data Machine l
  = Machine
  {
    _memory    :: IM.IntMap l,
    _registers :: (l, IM.IntMap l) -- sp and counted ([r0, ..])
  }
makeLenses ''Machine

type InstructionMap   = IML.IntMap Program
type InterpretState l = IM.IntMap (Machine l)
type InterpM l        = S.StateT (Machine l)
                                 (S.StateT (InterpretState l)
                                           (R.ReaderT InstructionMap IO)
                                 )

interpretOverLattice
  :: InterpreterLattice l
  => Program
  -> IO (InterpretState l)

interpretOverLattice prog = do
  let regs   = (concretize 0, IM.empty)
      mem    = IM.empty
      states = IM.empty

      localState = S.execStateT (interpretCodePoint 1) (Machine regs mem)
      search     = S.execStateT localState states

  R.runReaderT search (buildMapRepr prog)

buildMapRepr
  :: Program
  -> InstructionMap

buildMapRepr = go IML.empty
  where
  go points (Seq point instr prog) = go (IML.insert point instr points) prog
  go points _                      = points

renderState
  :: (InterpreterLattice v, Show v)
  => InterpretState v
  -> IO ()
renderState st = putStrLn . unlines $ map go $ IM.toList st
  where
  go (point, (reg, mem)) = unlines $
    [
     "point: " ++ show point,
     "reg: " ++ show reg,
     "mem: " ++ show mem
    ]

interpretCodePoint
  :: InterpreterLattice l
  => ControlPoint
  -> InterpM l ()

interpretCodePoint point = do
  instr <- IML.lookup point <$> lift (lift R.ask)
  case instr of
    Nothing -> return ()
    Just instr' -> do
      liftIO . putStrLn $ "[*] Evaluating code point " ++ show point ++ " with instruction " ++ show instr'
      observe <- interpret instr'
      when observe $ do
        changed <- liftM2 (merge point) S.get (lift S.get) >>= id
        when changed $ interpretCodePoint (point + 1)

continue, break
  :: Monad m
  => m a
  -> m Bool

continue act = act >> return True
break    act = act >> return False

-------------------------------------------------------------

interpret
  :: InterpreterLattice l
  => Program
  -> InterpM l Bool

interpret (Add src1 src2 dst) = continue $ do
  val1 <- readR src1
  val2 <- readR src2
  writeR dst (add val1 val2)

interpret (Fetch src dst) = continue $ readM src >>= writeR dst

interpret (Jmp point) = break $ interpretCodePoint point

-- IMPROVEMENT
-- the textZero call can be replaced
-- with the corresponding states in order to reach higher precision.
-- this may however result in more configurations being explored.
interpret (JmpZ t1 point) = do
  (_, cond) <- readR t1
  let (b1, b2) = testZero cond
  when b1 $ interpretCodePoint point
  if b2 then return True else return False

interpret (Mov t1 t2) = continue $ readR t1 >>= writeR t2

interpret (MovI t1 n) = continue $ writeR t1 (SymbolicWord, abstract n)

interpret (Write val dst) = continue $ writeM val dst

interpret _ = return True

merge
  :: InterpreterLattice l
  => ControlPoint
  -> Machine l
  -> InterpretState l
  -> InterpM l Bool

merge point updated entries = case membership of
  Nothing       -> putState updated >> return True
  Just existing -> do
    let newState = join' existing updated
    if newState == existing
      then return False
      else putState newState >> return True
  where
  membership = IM.lookup point entries
  putState v = lift . S.put $ IM.insert point v entries
  join' (Machine r1 m1) (Machine r2 m2) = (joinMaps r1 r2, joinMaps m1 m2)
  joinMaps = IM.unionWith unionOp
  unionOp val1 val2 = join val1 val2

-------------------------------------------------------------

readR
  :: Program
  -> InterpM l l

readR (RegLabel reg) = fmap ((! reg) . view registers) S.get

writeR
  :: Program
  -> l
  -> InterpM l ()

writeR (RegLabel n) val = S.modify $ over registers (IM.insert n val)

{-readM
  :: InterpreterLattice l
  => Program
  -> InterpM l l

readM = fetchFromStackAddress

writeM
  :: InterpreterLattice l
  => Program
  -> Program
  -> InterpM l ()

writeM valReg dstReg = do
  addr <- fetchFromAddress dstReg
  val <- readR valReg
  (regs, stack) <- S.get

  case concretize addr of
    [n] -> S.put $ (regs, IML.insert n val stack)
    _ -> error "TBD: implement when fetchMemoryAddress is extended"

fetchFromAddress
  :: InterpreterLattice l
  => Program
  -> InterpM l l
fetchFromAddress addrReg = do
  addr <- readR addrReg 
  process addr

  where
  process addr = case concretize addr of
    [n] -> do
      stack <- fmap snd S.get
      case IML.lookup n stack of
            Just val -> return val
            _        -> return $ error "Out of range memory access detected TODO"
    _   -> return $ error "Insufficient precision in memory operation"
    -- TODO: this can be refined by taking the union of all memory locations in range
    --       i.e. optionally returning a list of addresses
-}
