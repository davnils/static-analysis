---
--
-- How should stack allocations be handled?
--
-- the value of sp is clearly symbolic.
--
-- is it enough to simply have a dedicated sp in regsters?
-- should be. but stack-relative access can't be validated (in sign).
--
-- In interval domain it's better. can validate that the index
-- is a subinterval of the available stack (i.e. higher than the stack).
---

{-# LANGUAGE
    PatternSynonyms
  #-}

module Interpreter where

import           Algebra.Lattice (join)
import           AST
import           Control.Applicative ((<$>))
import           Control.Monad (liftM2, when, unless)
import           Control.Monad.Trans (lift, liftIO, MonadIO)
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as S
import           Data.Int (Int8)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.IntMap.Lazy as IML
import           Data.IntMap ((!))
import           Prelude hiding (break)
import           Types

data ValueType
  = SymbolicWord
  | StackAddress
  -- | HeapAddress
  deriving (Eq, Ord, Show)

type Value l      = (ValueType, l)
type Registers l  = (l, IM.IntMap (Value l)) -- sp and counted ([r0, ..])
type Stack l      = IM.IntMap (Value l)
type ConfState l  = (Registers l, Stack l)
type InstrMap     = IML.IntMap Program

type InterpretState l = IM.IntMap (ConfState l)
type InterpM l        = S.StateT (ConfState l)
                                 (S.StateT (InterpretState l)
                                           (R.ReaderT InstrMap IO)
                                 )

interpretOverLattice
  :: InterpreterLattice l
  => Program
  -> IO (InterpretState l)

interpretOverLattice prog = do
  let reg = (concretize 0, IM.empty)
      mem = IM.empty
      states = IM.empty

      localState = S.execStateT (interpretCodePoint 1) (reg, mem)
      search     = S.execStateT localState states

  R.runReaderT search (buildMapRepr prog)

buildMapRepr
  :: Program
  -> InstrMap

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

-- nan kunch-an-na (fine)
-- pi gå nä yo (tired)

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

-- TODO: should implement Add
--
-- TODO: add stack allocation.
--       i.e. add esp, 2 results in allocS 2; add esp 2
--       where the initial value in esp is of type esp

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
  -> ConfState l
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
  join' (r1, m1) (r2, m2) = (joinMaps r1 r2, joinMaps m1 m2)
  joinMaps = IM.unionWith unionOp
  unionOp (kind1, val1) (kind2, val2) = (refineTypes kind1 kind2, join val1 val2)
  refineTypes t1 t2 = if t1 == t2 then t1 else (error "Invalid type refinement")
  -- note that this assumes same set of registers and memory range for this ctrl point

-------------------------------------------------------------

readR
  :: Program
  -> InterpM l (Value l)

readR (RegLabel reg) = fmap ((! reg) . fst) S.get

writeR
  :: Program
  -> (Value l)
  -> InterpM l ()

writeR (RegLabel n) val = S.modify $ \(reg, mem) -> (IM.insert n val reg, mem)

assertStackAddress
  :: Monad m
  => ValueType
  -> m ()
assertStackAddress (StackAddress) = return ()
assertStackAddress  _             = return $ error "Memory reference to non-memory value"

readM
  :: InterpreterLattice l
  => Program
  -> InterpM l (Value l)

readM = fetchMemoryAddress

writeM
  :: InterpreterLattice l
  => Program
  -> Program
  -> InterpM l ()

writeM valReg dstReg = do
  (kind, addr) <- fetchMemoryAddress dstReg
  val <- readR valReg
  (regs, stack) <- S.get

  assertStackAddress kind

  case concretize addr of
    [n] -> S.put $ (regs, IML.insert n val stack)
    _ -> error "TBD: implement when fetchMemoryAddress is extended"

fetchMemoryAddress
  :: InterpreterLattice l
  => Program
  -> InterpM l (Value l)
fetchMemoryAddress addrReg = do
  (kind, addr) <- readR addrReg 
  assertStackAddress kind
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

-------------------------------------------------------------

evalType
  :: ValueType
  -> ValueType
  -> ValueType

evalType StackAddress StackAddress = SymbolicWord
evalType StackAddress SymbolicWord = StackAddress
evalType SymbolicWord StackAddress = StackAddress
evalType SymbolicWord SymbolicWord = SymbolicWord 
