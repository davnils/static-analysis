module Main where

import AST
import Data.Functor.Foldable (Fix(..))
import Interpreter
import SignLattice

parse :: String -> Program
parse = fix 1 . map go . map words . lines
  where
  go ["add", r1, r2, r3] = AddF (fixReg r1) (fixReg r2) (fixReg r3)
  go ["fetch", r1, r2]   = FetchF (fixReg r1) (fixReg r2)
  go ["jmp", point]      = JmpF (read point)
  go ["jmpz", r1, point] = JmpZF (fixReg r1) (read point)
  go ["mov", r1, r2]     = MovF (fixReg r1) (fixReg r2)
  go ["movi", r1, n]     = MovIF (fixReg r1) (read n)
  go ["write", r1, r2]   = WriteF (fixReg r1) (fixReg r2)

  fixReg reg = Fix (RegLabelF . read $ tail reg)

  fix count (x:xs) = Fix $ SeqF count (Fix x) (fix (count + 1) xs)
  fix _ [] = Fix SeqEndF

run :: String -> IO ()
run file = readFile file >>= interpretOverLattice . parse >>= renderState . overSignLattice
  where
  overSignLattice :: InterpretState (Value SignLattice) -> InterpretState (Value SignLattice)
  overSignLattice = id

main :: IO ()
main = return () {- do
  prog <- fmap parse getContents
  interpretOverLattice prog >>= print -}
