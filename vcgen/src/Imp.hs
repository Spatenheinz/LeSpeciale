{-# LANGUAGE DeriveGeneric #-}
module Imp where

import GHC.Generics
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Int (Int64)
import Data.Word (Word8)

data BinAlu = Add | Sub | Mul | Div | Or | And | Lsh | Rsh | Mod | Xor | Arsh
  -- | MovTmp
  deriving (Eq, Show, Ord, Enum, Generic)

data UnAlu = Neg | Le | Be
  deriving (Eq, Show, Ord, Enum, Generic)

data RelOp = Eq | Gt | Ge | Lt | Leq | Set | Ne | Sgt | Sge | Slt | Sle
  deriving (Eq, Show, Ord, Enum, Generic)

data BSize = B8 | B16 | B32 | B64
  deriving (Eq, Show, Ord, Enum, Generic)

type Data = Int64
type RegName = Int
data Loc = Stack Word8
  deriving(Eq, Show, Ord, Generic)

data Prim = Const Data
          | Reg RegName
  deriving (Eq, Show, Ord, Generic)

data Cmd = SetBin BSize RegName Prim BinAlu Prim
         | SetUn BSize RegName UnAlu Prim
         | Mov BSize RegName Prim
         | MemRead BSize RegName Loc
         | MemWrite BSize Loc Prim
  deriving (Eq, Show, Generic)

data Instr = Seq Cmd Instr
           | Cond Prim RelOp Prim Instr Instr
           | Exit
  deriving (Eq, Show, Generic)

cmdsToInstr cmds = foldr Seq Exit cmds


type Config = (Map Loc Data, IntMap Data)

initial :: Config
initial = (M.empty, IM.fromList $ zip [0..9] [0,0..])

data Prog = Prog Config Instr
  deriving(Eq, Show)

initProg :: Instr -> Prog
initProg = Prog initial
