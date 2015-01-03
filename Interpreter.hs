{-# LANGUAGE
    PatternSynonyms
  #-}

module Interpreter where

import           AST
import           Control.Monad (liftM2)
import           Control.Monad.Trans (lift, MonadIO)
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as S
import           Data.Int (Int8)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import           Data.IntMap ((!))
import           Types

type Register v = IM.IntMap v
-- TOOD: memory should be modeled as stack and heap
type Memory v = IM.IntMap v
type ConfState v = (Register v, Memory v)

type InterpretState v = IM.IntMap (ConfState v)
type InterpM v = S.StateT (ConfState v) (S.StateT (InterpretState v) IO)

interpretOverLattice
  :: InterpreterLattice v
  => v
  -> Program
  -> IO (InterpretState v)

interpretOverLattice defValue prog = do
  -- TODO: integrate defValue for uninit reads
  let reg = IM.empty
      mem = IM.empty
      states = IM.singleton 0 (reg, mem)
      inner = S.execStateT (interpret prog) (reg, mem)
  S.execStateT inner states

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

interpret
  :: InterpreterLattice v
  => Program
  -> InterpM v ()

interpret (Seq point instr tail) = do
  interpret instr
  newConf <- S.get
  lift . S.modify $ \confs -> IM.insert point newConf confs
  interpret tail

interpret (Add t1 t2 t3) = liftM2 add (readR t1) (readR t2) >>= writeR t3

interpret (Mov t1 t2) = readR t1 >>= writeR t2

interpret (MovI t1 n) = writeR t1 (abstract n)

interpret _ = return ()

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
