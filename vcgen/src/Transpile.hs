{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
-- | This module is used to convert the ADT of IMP to eBPF
-- | NOTE: THIS IS VERY FAULTY SIMPLY FROM THE SIZE OF INT
module Transpile where

import qualified Ebpf.Asm as A
import Imp as I
import Data.Maybe (fromJust)
import Ebpf.Helpers
import Data.Monoid

transpile :: Instr -> A.Program
transpile instr = appEndo (trans instr) [A.Exit]

trans = \case
  Seq cmd rest -> cmd2eBPF cmd <> trans rest
  Exit -> mempty
  Cond _ _ _ _ _ -> error "Transpiler does not handle conditionals, yet"

binI2A :: I.BinAlu -> A.BinAlu
binI2A = \case
  I.Add -> A.Add ; I.Sub -> A.Sub; I.Mul -> A.Mul; I.Div -> A.Div
  I.Or -> A.Or; I.And -> A.And;
  I.Lsh -> A.Lsh; I.Rsh -> A.Rsh; I.Mod -> A.Mod; I.Xor -> A.Xor; I.Arsh -> A.Arsh

unI2A :: I.UnAlu -> A.UnAlu
unI2A = \case
  I.Neg -> A.Neg;  I.Le -> A.Le; I.Be -> A.Be

sizeI2A :: I.BSize -> A.BSize
sizeI2A = \case
  I.B8 -> A.B8; I.B16 -> A.B16; I.B32 -> A.B32; I.B64 -> A.B64

cond2Jmp :: RelOp -> A.Jcmp
cond2Jmp = \case
  Eq ->  A.Jeq ; Ne ->  A.Jne;
  Gt ->  A.Jgt ; Ge ->  A.Jge;
  Lt ->  A.Jlt ; Leq -> A.Jle;
  Sgt -> A.Jsgt; Sge -> A.Jsge;
  Slt -> A.Jslt; Sle -> A.Jsle;
  Set -> A.Jset

-- NOTE: THIS IS JUST A PLACEHOLDER FOR NOW.
-- AT THE MOMENT STUFF WONT WORK
-- WITHOUT DIFFERENT NAMES
-- INSTANCE: x = *m⮾*n
rAlloc :: RegName
rAlloc = 9

rAlloc2 :: A.Reg
rAlloc2 = A.Reg 8

stack :: A.Reg
stack = A.Reg 10

fromPrim = \case
  Reg src -> Left $ A.Reg src
  Const c -> Right $ fromIntegral c

cmd2eBPF :: I.Cmd -> Endo A.Program
cmd2eBPF cmd = toDList $
  case cmd of
    SetBin sz dst lhs op src | Reg dst == lhs ->
      [ A.Binary (sizeI2A sz) (binI2A op) (A.Reg dst) (fromPrim src) ]
    SetUn sz dst op src ->
      [ A.Binary size A.Mov tdst tsrc | Left tdst /= tsrc ] ++
      [ A.Unary size (unI2A op) tdst ]
      where
        size = sizeI2A sz
        tdst = A.Reg dst
        tsrc = fromPrim src
    Mov sz dst src ->
      [ A.Binary (sizeI2A sz) A.Mov (A.Reg dst) (fromPrim src) ]
    MemRead sz dst (Stack loc) ->
      [ A.Load (sizeI2A sz) (A.Reg dst) stack (Just . negate $ fromIntegral loc) ]

toDList :: [a] -> Endo [a]
toDList xs = Endo (xs <>)

-- newtype Transpile a = Transpile
--   { runTranspile :: StateT (Endo [A.Instruction]) (Reader A.BSize) a }
--   deriving(Functor, Applicative, Monad,
--            MonadReader A.BSize,
--            MonadState (Endo [A.Instruction])) -- use state because of conditionals

-- transpile :: Instr -> A.Program
-- transpile inst =
--   flip appEndo [] $ snd $ runReader (flip runStateT mempty $ runTranspile $ instr2BPF inst) defaultSize
--   where defaultSize = A.B64

-- tellD :: [A.Instruction] -> Transpile ()
-- tellD xs = modify (<> toDList xs)

-- load :: RegName -> Prim -> Transpile ()
-- load dst (Const d) = tellD [ A.LoadImm (A.Reg dst) d ]
-- load dst (MemRead l) = mRead dst l
-- load dst (I.Reg r)
--   | dst == r  = return ()
--   | otherwise =
--       do size <- ask
--          tellD [ A.Binary size A.Mov (A.Reg dst) (Left $ A.Reg r) ]

-- mRead :: RegName -> Loc -> Transpile ()
-- mRead  dst (Stack loc) = do
--   size <- ask
--   tellD []

-- mStore :: Loc -> RegName -> Transpile ()
-- mStore (Stack loc) src = do
--   size <- ask
--   tellD [A.Store size stack (Just . negate $ fromIntegral loc) (Left $ A.Reg src)]

-- binop :: I.BinAlu -> RegName -> Prim -> Transpile ()
-- binop op dst (I.Reg r) = do
--   size <- ask
--   tellD [ A.Binary size (binI2A op) (A.Reg dst) (Left $ A.Reg r)]
-- binop op dst (Const d) = do
--   size <- ask
--   tellD [ A.Binary size (binI2A op) (A.Reg dst) (Right $ fromIntegral d)]
-- binop op dst m@(MemRead l) = do
--   size <- ask
--   let reg' = rAlloc
--   load reg' m
--   tellD [ A.Binary size (binI2A op) (A.Reg dst) (Left $ A.Reg reg') ]

-- relop :: RelOp -> RegName -> Prim -> A.Offset -> Transpile ()
-- relop op dst (I.Reg r) offset =
--   tellD [A.JCond (cond2Jmp op) (A.Reg dst) (Left $ A.Reg r) offset]
-- relop op dst (Const d) offset =
--   tellD [A.JCond (cond2Jmp op) (A.Reg dst) (Right $ fromIntegral d) offset]
-- relop op dst m@(MemRead l) offset = do
--   let reg' = rAlloc
--   load reg' m
--   tellD [A.JCond (cond2Jmp op) (A.Reg dst) (Left $ A.Reg reg') offset]

-- setBin :: RegName -> Prim -> I.BinAlu -> Prim -> Transpile ()
-- setBin dst lhs op rhs
--   | (I.Reg dst) == rhs =
--     case lhs of
--       I.Reg y -> instr y
--       _       -> do
--                  let reg' = rAlloc
--                  load reg' lhs >> instr reg'
--   | otherwise = let reg' = rAlloc in load dst lhs >> binop op dst rhs
--   where instr x = do
--           size <- ask
--           binop op x rhs
--           tellD [A.Binary size A.Mov (A.Reg dst) (Left $ A.Reg x)]

-- setUn :: RegName -> I.UnAlu -> Prim -> Transpile ()
-- setUn dst op src = do
--   size <- ask
--   load dst src
--   tellD [ A.Unary size (unI2A op) (A.Reg dst) ]

-- memWrite :: Loc -> Prim -> I.BinAlu -> Prim -> Transpile ()
-- memWrite loc lhs op rhs =
--   case lhs of
--     (I.Reg r) -> binop op r rhs >> mStore loc r
--     _         -> do
--                  let reg' = rAlloc
--                  load reg' lhs
--                  binop op reg' rhs
--                  mStore loc reg'

-- cond :: Prim -> RelOp -> Prim -> Instr -> Instr -> Transpile ()
-- cond lhs op rhs ifc elc = do
--       st <- gets (flip appEndo [])
--       let stlen = length st
--       instr2BPF ifc
--       st' <- gets (flip appEndo [])
--       let stlen' = length st'
--       let offset = stlen' - stlen
--       instr2BPF elc -- For good measure
--       st'' <- gets (flip appEndo [])
--       let rel x = relop op x rhs (fromIntegral offset)
--       put $ Endo (st <>)
--       case lhs of
--         (I.Reg r) -> rel r
--         _         -> let reg' = rAlloc in load reg' lhs >> rel reg'
--       modify (\s -> s <> Endo ((drop (stlen) st') <>))
--       modify (\s -> s <> Endo ((drop (stlen') st'') <>))

-- mov :: RegName -> Prim -> Transpile ()
-- mov dst (I.Reg src) =
--   ask >>= \size -> tellD [ A.Binary size A.Mov (A.Reg dst) (Left $ A.Reg src) ]
-- mov dst (I.Const d) =
--   ask >>= \size -> tellD [ A.Binary size A.Mov (A.Reg dst) (Right $ fromIntegral d) ]
-- mov dst (I.MemRead l) = do
--   mRead dst l
--   size <- ask
--   tellD [ A.Binary size A.Mov (A.Reg dst) (Left $ A.Reg rAlloc) ]

-- --TODO: sizes everywhere
-- instr2BPF :: I.Instr -> Transpile ()
-- instr2BPF (I.SetBin size dst lhs op rhs next)   = local (const . sizeI2A $ size) (setBin dst lhs op rhs) >> instr2BPF next
-- instr2BPF (I.SetUn size dst op src next)        = local (const . sizeI2A $ size) (setUn dst op src) >> instr2BPF next
-- instr2BPF (I.Mov size dst src next)             = local (const . sizeI2A $ size) (mov dst src) >> instr2BPF next
-- instr2BPF (I.MemWrite size dst lhs op rhs next) = local (const . sizeI2A $ size) (memWrite dst lhs op rhs) >> instr2BPF next
-- instr2BPF (I.Cond lhs op rhs ifc elc)      = cond lhs op rhs ifc elc
-- instr2BPF (I.Exit)                         = tellD [A.Exit]
