module Main (main) where

import Data.Int (Int64)
import qualified Imp as I
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Test.QuickCheck
import Test.QuickCheck.Gen
import Protocol
import Eval (configEval)
import Data.List (inits, tails, subsequences)
import VCgen
import Pretty (prettyProg)

import System.Process
import System.Exit
import System.Directory
import Control.Monad (guard, forM_, when)

import Criterion.Main

import qualified Data.ByteString as B
import Ebpf.Encode
import Transpile
import EbpfFFI


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
genStraightAlu maxImm = sized $ \n -> do
  I.cmdsToInstr <$> (vectorOf n $ genAlu maxImm)

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
      (rd,rs) <- genDestinationAndSource 20
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
    instr <- genStraightAlu 512
    let (_, regs) = configEval (I.Prog I.initial instr)
    let r6 = regs IM.! 6
    if r6 >= 0 && r6 < 8192 then return $ MkIB $ replace instr r0
    else return $ MkIB $ replace instr mov_r0
    where
      r0 = I.cmdsToInstr [I.SetBin I.B64 0 (I.Reg 0) I.Add (I.Reg 6)]
      mov_r0 = I.cmdsToInstr [I.Mov I.B64 6 (I.Const 0),
                              I.SetBin I.B64 0 (I.Reg 0) I.Add (I.Reg 6)]

-- benchmarking
cvc5Args = ["--dump-proofs"
           ,"--proof-format-mode=lfsc"
           ,"--simplification=none"
           ,"--dag-thresh=0"
           ,"--proof-granularity=theory-rewrite"
           ]

signatures = [ "core_defs.plf"
             , "util_defs.plf"
             , "theory_def.plf"
             , "nary_programs.plf"
             , "boolean_programs.plf"
             , "boolean_rules.plf"
             , "cnf_rules.plf"
             , "equality_rules.plf"
             , "arith_programs.plf"
             , "arith_rules.plf"
             , "strings_programs.plf"
             , "strings_rules.plf"
             , "quantifiers_rules.plf"
             ]

sigDir = "../deps/share/lfsc/signatures/"
lfscc = "../deps/bin/lfscc"
lfsc_rust = "../lfsc-lexer/target/release/lfsc-lexer"

sigs = unwords $ map (sigDir <>) signatures

hyperfineArgs prog = ["--warmup", "5", "--min-runs", "10"]
  -- <> [lfscc <> " " <> sigs <> " " <> prog]
  <> [unwords [lfscc, sigs, prog, " >& /dev/null"], unwords [lfsc_rust, prog]]

hyperfine = "hyperfine"

tens = 1 : [10 * x | x <- tens]

test :: Int -> (String, B.ByteString) -> IO ()
test n (prog, ebpf) = do
  putStrLn $ "bench for " <> show n
  defaultMainWith defaultConfig $ [bench "ebpf" $ whnfIO $ (do p <- cLoadProg ebpf; if p < 0 then putStrLn "false" >> return False else cCloseFd p >> return True)]
  -- putStrLn $ "hyperfine for " <> show n
  -- let filename = "benchmark_" <> show n
  -- let smt2 = filename <> ".smt2"
  -- writeFile smt2 prog
  -- (exitcode, stdout, stderr) <-
  --   readProcessWithExitCode "cvc5" (cvc5Args ++ [smt2]) ""
  -- case exitcode of
  --   ExitSuccess -> do
  --     let (sat:proof) = lines stdout
  --     when (sat /= "unsat") (putStrLn ("cvc5: Failure at " <> show n) >> mkprog n >>= test n)
  --     if proof == [] then putStrLn "cvc5: No proof"
  --     else do
  --       writeFile (filename ++ ".plf") $ unlines $ tail proof
  --       (_exitcode, stdout, stderr) <-
  --         readProcessWithExitCode hyperfine (hyperfineArgs (filename <> ".plf") <> ["--show-output"]) ""
  --       putStrLn stdout
  --       -- removeFile $ filename <> ".smt2"
  --       -- hyperfine --runs 10 --warmup 5 'target/release/lfsc-lexer' --export-markdown /tmp/hf.md && cat /tmp/hf.md"
  --   ExitFailure _ -> do
  --     putStr "cvc5: Failure\n\t" >> putStr stdout >> putStr stderr >> exitFailure

r0InRange = Rel (RLE (RPrim $ C 0) (RPrim $ V "0")) ./\. Rel (RLT (RPrim $ V "0") (RPrim $ C 8192)) .=>. Rel RTrue

initRegisters = foldr (\e a -> I.Seq (I.Mov I.B64 e (I.Const 0)) a)

mkprog :: Int -> IO (String, B.ByteString)
mkprog n = do
  (MkIB prog) <- generate (resize n (arbitrary :: Gen InBound))
  let prog' = initRegisters prog [0,6,7]
  let wp' = wp prog' r0InRange
  let tran = encodeProgram $ transpile prog'
  return $ (prettyProg wp', tran)

main :: IO ()
main = do
  (_, _, _, ph) <-
    createProcess (proc "cargo" ["build", "--release", "--quiet"])
    { cwd = Just "../lfsc-lexer/"}
  exitCode <- waitForProcess ph
  forM_ (take 4 tens) $ \n -> do
    putStrLn "=========================="
    prog <- mkprog n
    test n prog
