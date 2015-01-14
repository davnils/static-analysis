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

type Register v        = IM.IntMap v
-- TOOD: memory should be modeled as stack and heap
type Memory v          = IM.IntMap v
type ConfState v       = (Register v, Memory v)
type InstrMap          = IML.IntMap Program

type InterpretState v = IM.IntMap (ConfState v)
type InterpM v = S.StateT (ConfState v) (S.StateT (InterpretState v) (R.ReaderT InstrMap IO))

interpretOverLattice
  :: InterpreterLattice v
  => v
  -> Program
  -> IO (InterpretState v)

interpretOverLattice defValue prog = do
  -- TODO: integrate defValue for uninit reads
  let reg = IM.empty
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

-------------------------------------------------------------
-- åtto ke tje ne yo
-- nan kunch-an-na
-- pi gå nä yo
--
interpretCodePoint
  :: InterpreterLattice v
  => ControlPoint
  -> InterpM v ()

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
  :: InterpreterLattice v
  => Program
  -> InterpM v Bool

interpret (Add t1 t2 t3) = continue $ liftM2 add (readR t1) (readR t2) >>= writeR t3

interpret (Jmp point) = break $ interpretCodePoint point

-- IMPROVEMENT
-- the textZero call can be replaced
-- with the corresponding states in order to reach higher precision.
-- this may however result in more configurations being explored.
interpret (JmpZ t1 point) = do
  cond <- readR t1
  let (b1, b2) = testZero cond
  when b1 $ interpretCodePoint point
  if b2 then return True else return False

interpret (Mov t1 t2) = continue $ readR t1 >>= writeR t2

interpret (MovI t1 n) = continue $ writeR t1 (abstract n)

interpret _ = return True

merge
  :: InterpreterLattice v
  => ControlPoint
  -> ConfState v
  -> InterpretState v
  -> InterpM v Bool

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
  joinMaps = IM.unionWith join
  -- note that this assumes same set of registers and memory range for this ctrl point

-------------------------------------------------------------

readR
  :: Program
  -> InterpM v v

readR (RegLabel reg) = fmap ((! reg) . fst) S.get

writeR
  :: Program
  -> v
  -> InterpM v ()

writeR (RegLabel n) val = S.modify $ \(reg, mem) -> (IM.insert n val reg, mem)
