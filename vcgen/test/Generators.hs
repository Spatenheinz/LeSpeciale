module Generators where

import Data.Int (Int64)
import qualified Imp as I
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Test.QuickCheck
import Protocol
import Eval (configEval)
import Data.List (inits, tails, subsequences)

imm32bMax = (2^31) - 1
imm64bMax = maxBound :: Int64

genDestinationAndSource :: Int64 -> Gen (I.RegName, I.Prim)
genDestinationAndSource maxImm = oneof
  [
    do
      -- destination is always a register
      -- TODO: We might not want to mess with R0, which should contain a pointer to a map
      destination <- elements regs
      -- Immediates are between 0 and 2^32
      -- TODO: Double check if we can use 64-bit immediates in general
      imm <- choose (0, maxImm)
      return $ (destination, I.Const imm)
  ,
    do
      (dest, source) <- elements [(x,y) | (x:ys) <- tails regs, y <- ys]
      return $ (dest, I.Reg source)
    -- TODO: Memloc generation
  ]
  where regs = [6..7]

genSize :: Gen I.BSize
genSize = elements [I.B32, I.B64]

genStraightAlu :: Int64 -> Gen I.Instr
genStraightAlu maxImm =
  I.cmdsToInstr <$> (listOf1 $ genAlu maxImm)

genAlu maxImm = frequency
  [ (1, do
      (rd,source) <- genDestinationAndSource maxImm
      size <- genSize
      return $ I.Mov size rd source
    )
    -- ALU instructions
  , (10, do
      (rd,rs) <- genDestinationAndSource maxImm
      binOp <- elements [I.Add, I.Sub, I.Mul, I.Div, I.Or, I.And, I.Mod, I.Xor]
      size <- genSize
      return $ I.SetBin size rd (I.Reg rd) binOp rs
    )
   -- ALU with shifts
  , (10, do
      (rd,rs) <- genDestinationAndSource 63
      binOp <- elements [I.Lsh, I.Rsh ] --, I.Arsh]
      size <- genSize
      return $ I.SetBin size rd (I.Reg rd) binOp rs
    )
    -- unary ops
  , (5, do
      (size, opr) <- elements [ (sz, alu)
                              | alu <- [ I.Neg ], --, I.Le, I.Be],
                                sz <- case alu of
                                        I.Neg -> [I.B32, I.B64]
                                        _ -> [I.B16, I.B32, I.B64] ]
      reg <- dstReg
      return $ I.SetUn size reg opr (I.Reg reg)
    )
  ]
  where
    dstReg = elements [6,7]

newtype InBound = MkIB I.Instr
  deriving(Show)

instance Arbitrary InBound where
  arbitrary = do
    instr <- genStraightAlu 10
    let (_, regs) = configEval (I.Prog I.initial instr)
    return $ MkIB $ replace instr mov_r0
    where
      r0 = I.cmdsToInstr [I.SetBin I.B64 0 (I.Reg 0) I.Add (I.Reg 6)]
      mov_r0 = I.cmdsToInstr [I.Mov I.B64 6 (I.Const 0),
                              I.SetBin I.B64 0 (I.Reg 0) I.Add (I.Reg 6)]
