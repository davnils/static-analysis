module Main where

import AST
import Data.Functor.Foldable (Fix(..))
import Interpreter
import SignLattice

parse :: String -> Program
parse = fix 1 . map go . map words . lines
  where
  go ["add", r1, r2, r3] = AddF (fixReg r1) (fixReg r2) (fixReg r3)
  go ["mov", r1, r2]     = MovF (fixReg r1) (fixReg r2)
  go ["movi", r1, n]     = MovIF (fixReg r1) (read n)

  fixReg reg = Fix (RegLabelF . read $ tail reg)

  fix count (x:xs) = Fix $ SeqF count (Fix x) (fix (count + 1) xs)
  fix _ [] = Fix SeqEndF

run :: String -> IO ()
run file =  readFile file >>= interpretOverLattice Any . parse >>= renderState

main :: IO ()
main = do
  prog <- fmap parse getContents
  interpretOverLattice Any prog >>= print
