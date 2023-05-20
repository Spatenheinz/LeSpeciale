module Pretty where

import VCgen
import Imp
import Data.Int (Int64)
import Text.Printf

prettyProg :: Pred -> String
prettyProg = with_smt_lib_wrapping . pp_smt

with_smt_lib_wrapping :: String -> String
with_smt_lib_wrapping s =
  "(set-logic BV)\n(set-option :produce-proofs true)\n(set-option :proof-format-mode lfsc)\n(set-option :dump-proofs true)\n\n" ++
  "(assert (not\n" ++
  "(forall ((r0 (_ BitVec 64)))\n" ++
  s ++
  "\n)))\n(check-sat)\n(exit)\n"

toHex :: Int64 -> String
toHex x = printf "#x%016x" x

ppIndent :: Int -> String
ppIndent d = replicate (d*2) ' '

ppOp_smt :: Operation -> String
ppOp_smt (V x) = 'r':x
ppOp_smt (C c) = toHex c
ppOp_smt (UnOp Neg a) = "(bvneg "  ++ ppOp_smt a ++ ")"
ppOp_smt (BinOp a Add b) = "(bvadd "  ++ ppOp_smt a ++ " " ++ ppOp_smt b ++ ")"
ppOp_smt (BinOp a Sub b) = "(bvsub "  ++ ppOp_smt a ++ " " ++ ppOp_smt b ++ ")"
ppOp_smt (BinOp a Mul b) = "(bvmul "  ++ ppOp_smt a ++ " " ++ ppOp_smt b ++ ")"
ppOp_smt (BinOp a Div b) = "(bvudiv " ++ ppOp_smt a ++ " " ++ ppOp_smt b ++ ")"
ppOp_smt (BinOp a Xor b) = "(bvxor "  ++ ppOp_smt a ++ " " ++ ppOp_smt b ++ ")"
ppOp_smt (BinOp a Mod b) = "(bvurem "  ++ ppOp_smt a ++ " " ++ ppOp_smt b ++ ")"
ppOp_smt (BinOp a And b) = "(bvand "  ++ ppOp_smt a ++ " " ++ ppOp_smt b ++ ")"
ppOp_smt (BinOp a Or  b) = "(bvor "   ++ ppOp_smt a ++ " " ++ ppOp_smt b ++ ")"
ppOp_smt (BinOp a Lsh b) = "(bvshl "  ++ ppOp_smt a ++ " " ++ ppOp_smt b ++ ")"
ppOp_smt (BinOp a Rsh b) = "(bvlshr " ++ ppOp_smt a ++ " " ++ ppOp_smt b ++ ")"

ppRelation_smt :: Relation -> String
ppRelation_smt RTrue = "true"
ppRelation_smt RFalse = "false"
ppRelation_smt (RPrim op) = ppOp_smt op
ppRelation_smt (REQ  p1 e2) = "(= "     ++ ppRelation_smt p1 ++ " " ++ ppRelation_smt e2 ++ ")"
ppRelation_smt (RNE p1 e2) = "(not "   ++ ppRelation_smt (REQ p1 e2) ++ ")"
ppRelation_smt (RGE p1 e2) = "(bvuge " ++ ppRelation_smt p1 ++ " " ++ ppRelation_smt e2 ++ ")"
ppRelation_smt (RGT p1 e2) = "(bvugt " ++ ppRelation_smt p1 ++ " " ++ ppRelation_smt e2 ++ ")"
ppRelation_smt (RLE p1 e2) = "(bvule " ++ ppRelation_smt p1 ++ " " ++ ppRelation_smt e2 ++ ")"
ppRelation_smt (RLT p1 e2) = "(bvult " ++ ppRelation_smt p1 ++ " " ++ ppRelation_smt e2 ++ ")"

ppP_smt :: Int -> Pred -> String
ppP_smt d (Forall v p) = ppIndent d ++ "(forall ((" ++ ('r':v) ++ " (_ BitVec 64)))\n" ++ ppP_smt (d+1) p ++ "\n" ++ ppIndent d ++ ")"
ppP_smt d (Negation p) = ppIndent d ++ "(not " ++ ppP_smt d p ++ ")"
ppP_smt d (Conj p1 p2) = ppIndent d ++ "(and \n" ++ ppP_smt (d+1) p1 ++ "\n" ++ ppP_smt (d+1) p2 ++ "\n" ++ ppIndent d ++ ")"
ppP_smt d (Disj p1 p2) = ppIndent d ++ "(and \n" ++ ppP_smt (d+1) p1 ++ "\n" ++ ppP_smt (d+1) p2 ++ "\n" ++ ppIndent d ++ ")"
ppP_smt d (Imp p1 p2) = ppIndent d ++ "(=> \n" ++ ppP_smt (d+1) p1 ++ "\n" ++ ppP_smt (d+1) p2 ++ "\n" ++ ppIndent d ++ ")"
ppP_smt d (Rel ep) = ppIndent d ++ ppRelation_smt ep

pp_smt :: Pred -> String
pp_smt predicate = ppP_smt 1 predicate
