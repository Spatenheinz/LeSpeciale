{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase, Strict #-}
-- | Evaluator for a Imp Prog, will give back a an updated Config
module Eval where

import Imp
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Bits
import Data.Int
import Data.Word
import Data.Function
import Debug.Trace

newtype Eval a = Eval {runEval :: ReaderT BSize (State Config) a }
  deriving (Functor, Applicative, Monad, MonadState Config, MonadReader BSize)

eval :: Prog -> Prog
eval (Prog config instr) =
  case flip execState config . flip runReaderT B64 $ runEval (evalInstr instr) of
    c -> Prog c instr

configEval :: Prog -> Config
configEval = fetchConfig . eval

stepwiseEval :: Prog -> Prog
stepwiseEval (Prog config (Seq cmd next)) =
  let (Prog config' _) = eval (Prog config (Seq cmd Exit))
  in (Prog config' next)
stepwiseEval (Prog config Exit) = (Prog config Exit)
stepwiseEval (Prog config instr@(Cond _ _ _ _ _)) = error "not defined yet"

fetchConfig (Prog c i) = c


setReg :: RegName -> Data -> Eval ()
setReg dst dat =
  modify' (\(stack, regs) -> (stack, IM.insert dst dat regs))

setLoc :: Loc -> Data -> Eval ()
setLoc dst dat = modify (\(stack, regs) -> (M.insert dst dat stack, regs))

evalCmd :: Cmd -> Eval ()
evalCmd (SetBin size dst lhs op rhs) =
  setReg dst =<< local (const size) (evalBinop lhs op rhs)
evalCmd (SetUn _size dst op src) =
  setReg dst =<< evalUnop op src
evalCmd (Mov size dst src) =
  case size of
    B64 -> do src' <- fromIntegral <$> evalPrim src :: Eval Word64
              setReg dst $ fromIntegral src'
    B32 -> do src' <- fromIntegral <$> evalPrim src :: Eval Word32
              setReg dst $ fromIntegral src'
evalCmd (MemRead sz dst l) =
  do stack <- gets fst
     let r = maybe 0 id $ (l `M.lookup` stack)
     setReg dst r
evalCmd (MemWrite size dst src) =
  case size of
    B64 -> do src' <- fromIntegral <$> evalPrim src :: Eval Word64
              setLoc dst $ fromIntegral src'
    B32 -> do src' <- fromIntegral <$> evalPrim src :: Eval Word32
              setLoc dst $ fromIntegral src'



evalInstr :: Instr -> Eval ()
evalInstr (Seq cmd rest) =
  evalCmd cmd >>
  evalInstr rest
evalInstr (Cond lhs op rhs ifc elc) = do
  cond <- evalRelOp lhs op rhs
  evalInstr (if cond then ifc else elc)
evalInstr Exit = return ()

data E = E

evalUnop :: UnAlu -> Prim -> Eval Data
evalUnop Neg src = negate <$> evalPrim src
evalUnop Le src = undefined -- i dont know
evalUnop Be src = undefined -- not good

retData :: (Monad m, Integral a, Num b) => a -> m b
retData = return . fromIntegral

-- Does this handle overflows??
evalBinop :: Prim -> BinAlu -> Prim -> Eval Data
evalBinop lhs Add rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> retData (fromIntegral $ l + r :: Word64)
    B32 -> retData (fromIntegral $ l + r :: Word32)
evalBinop lhs Sub rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> retData (fromIntegral $ l - r :: Word64)
    B32 -> retData (fromIntegral $ l - r :: Word32)
evalBinop lhs Mul rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> retData (fromIntegral $ l * r :: Word64)
    B32 -> retData (fromIntegral $ l * r :: Word32)
evalBinop lhs Div rhs = do
  ask >>= \case
    B64 -> (fromIntegral <$> evalPrim rhs :: Eval Word64) >>= \case
                0 -> return 0
                r -> do l <- fromIntegral <$> evalPrim lhs :: Eval Word64
                        retData $ l `div` r
    B32 -> (fromIntegral <$> evalPrim rhs :: Eval Word32) >>= \case
                0 -> return 0
                r -> do l <- fromIntegral <$> evalPrim lhs :: Eval Word32
                        retData $ l `div` r
evalBinop lhs Or rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> retData (fromIntegral $ l .|. r :: Word64)
    B32 -> retData (fromIntegral $ l .|. r :: Word32)
evalBinop lhs And rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> retData (fromIntegral $ l .&. r :: Word64)
    B32 -> retData (fromIntegral $ l .&. r :: Word32)
evalBinop lhs Lsh (Const imm) | imm < 0 || imm > 63 = error $ "Left shift not allowed with imm " <> show imm
evalBinop lhs Lsh rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    -- shiftL and shiftR is weird because we can only shift by an Int. IDK if this is correct.
    B64 -> retData $ (fromIntegral $ l :: Word64) `shiftL` (fromIntegral r .&. 63)   -- RHS needs be an Int
    B32 -> retData $ (fromIntegral $ l :: Word32) `shiftL` (fromIntegral r .&. 31)
evalBinop lhs Rsh (Const imm) | imm < 0 || imm > 63 = error $ "Right shift not allowed with imm " <> show imm
evalBinop lhs Rsh rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> retData $ (fromIntegral $ l :: Word64) `shiftR` (fromIntegral r .&. 63)   -- RHS needs be an Int
    B32 -> retData $ (fromIntegral $ l :: Word32) `shiftR` (fromIntegral r .&. 31)
evalBinop lhs Mod rhs =
  ask >>= \case
    B64 -> (fromIntegral <$> evalPrim rhs :: Eval Word64) >>= \case
                0 -> evalPrim lhs
                r -> do l <- fromIntegral <$> evalPrim lhs :: Eval Word64
                        retData $ l `mod` r
    B32 -> (fromIntegral <$> evalPrim rhs :: Eval Word32) >>= \case
                0 -> (fromIntegral <$> evalPrim lhs :: Eval Word32) >>= retData
                r -> do l <- fromIntegral <$> evalPrim lhs :: Eval Word32
                        retData $ l `mod` r
evalBinop lhs Xor rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> retData (fromIntegral $ l `xor` r :: Word64)
    B32 -> retData (fromIntegral $ l `xor` r :: Word32)
  -- For Arsh it should fill with ones. shiftR does so if lhs is negative
evalBinop lhs Arsh (Const imm) | imm < 0 || imm > 63 = error $ "Right shift not allowed with imm " <> show imm
evalBinop lhs Arsh rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> return . fromIntegral $ (fromIntegral l :: Int64) `shiftR` (fromIntegral r .&. 63)
    B32 -> return . fromIntegral $ (fromIntegral l :: Int32) `shiftR` (fromIntegral r .&. 31)

evalRelOp :: Prim -> RelOp -> Prim -> Eval Bool
evalRelOp lhs Eq   rhs = do (l,r) <- evalPrims lhs rhs; return $ l == r
evalRelOp lhs Gt   rhs = do (l,r) <- evalPrims lhs rhs; return $ l >  r
evalRelOp lhs Ge   rhs = do (l,r) <- evalPrims lhs rhs; return $ l >= r
evalRelOp lhs Lt   rhs = do (l,r) <- evalPrims lhs rhs; return $ l <  r
evalRelOp lhs Leq  rhs = do (l,r) <- evalPrims lhs rhs; return $ l <= r
-- This was the weird function
-- evalRelOp lhs Set  rhs = do (l,r) <- evalPrims lhs rhs; return $ l == r
evalRelOp lhs Ne   rhs = do (l,r) <- evalPrims lhs rhs; return $ l /= r
evalRelOp lhs Sgt  rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> return $ (fromIntegral l :: Word64) > (fromIntegral r :: Word64)
    B32 -> return $ (fromIntegral l :: Word32) > (fromIntegral r :: Word32)
evalRelOp lhs Sge  rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> return $ (fromIntegral l :: Word64) >= (fromIntegral r :: Word64)
    B32 -> return $ (fromIntegral l :: Word32) >= (fromIntegral r :: Word32)
evalRelOp lhs Slt  rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> return $ (fromIntegral l :: Word64) < (fromIntegral r :: Word64)
    B32 -> return $ (fromIntegral l :: Word32) < (fromIntegral r :: Word32)
evalRelOp lhs Sle  rhs = do
  (l,r) <- evalPrims lhs rhs
  ask >>= \case
    B64 -> return $ (fromIntegral l :: Word64) <= (fromIntegral r :: Word64)
    B32 -> return $ (fromIntegral l :: Word32) <= (fromIntegral r :: Word32)


evalPrims :: Prim -> Prim -> Eval (Data, Data)
evalPrims a b = evalPrim a >>= \a' -> (,) a' <$> evalPrim b

-- evalSrel :: (Integral a, Num b) => (b -> b -> Bool) -> a -> a -> Eval Bool
-- evalSrel op a b =
--   ask >>= \case
--     B64 -> return $ (fromIntegral a :: Word64) `op` (fromIntegral b :: Word64)
--     B32 -> return $ (fromIntegral a :: Word32) `op` (fromIntegral b :: Word32)

-- NOTE: this assumes that registers and stack are 0 initialized
evalPrim :: Prim -> Eval Data
evalPrim (Const d) = return d
--evalPrim (Const d) = trace (show d) $ return d
evalPrim (Reg r) = do regSt <- gets snd
                      maybe (return 0) return $ r `IM.lookup` regSt
