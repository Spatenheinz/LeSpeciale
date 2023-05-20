module VCgen where

import Imp

data Pred =
    Forall VName Pred
  | Negation Pred
  | Conj Pred Pred
  | Disj Pred Pred
  | Imp Pred Pred
  | Rel Relation
  deriving (Show, Eq)


(./\.) :: Pred -> Pred -> Pred
(./\.) (Rel RTrue) a = a
(./\.) a b = Conj a b
infixr 3 ./\.
(.\/.) :: Pred -> Pred -> Pred
(.\/.) = Disj
infixr 2 .\/.
(.=>.) :: Pred -> Pred -> Pred
(.=>.) = Imp
infixr 1 .=>.

data Relation =
    RTrue
  | RFalse
  | RPrim Operation
  | REQ Relation Relation
  | RNE Relation Relation
  | RLT Relation Relation
  | RLE Relation Relation
  | RGT Relation Relation
  | RGE Relation Relation
  deriving (Show, Eq)

eq a b = Rel $ REQ a b
true = Rel RTrue

type VName = String
data Operation =
    V VName
  | C Data
  | BinOp Operation BinAlu Operation
  | UnOp UnAlu Operation
  deriving (Show, Eq)

var = RPrim . V
con = RPrim . C

prim2Op (Reg x) = V $ show x
prim2Op (Const x) = C x

freshVar x xs = if x `elem` xs then freshVar (x ++ "_") xs else x

forall' :: RegName -> Pred -> Operation -> Pred -> Pred
forall' dst guard rel q =
  Forall x' $ guard ./\. ((var x') `eq` (RPrim rel) .=>. q')
  where
    x' = freshVar (show dst) (bv q)
    q' = subst (show dst) x' q

class WPAble a where
  wp :: a -> Pred -> Pred

instance WPAble Cmd where
  wp (SetBin _sz dst (Reg src1) alu src2) q
    | dst == src1 = forall' dst guard' rel q
      where
        src' = prim2Op src2
        rel = BinOp (V $ show dst) alu src'
        guard' = Rel $ if alu `elem` [Div , Mod] then RNE (RPrim src') (con 0)
                       else RTrue
  wp (SetUn _sz dst unalu src) q =
    forall' dst true (UnOp unalu $ prim2Op src) q
  wp (Mov _sz dst src) q = forall' dst true (prim2Op src) q
  wp _ _ = undefined

instance WPAble Instr where
  wp (Seq cmd instr) q = wp cmd (wp instr q)
  wp Exit q = q
  wp _ _ = undefined

class BVars a where
  bv :: a -> [VName]

instance BVars Pred where
  bv (Forall x p) = x : bv p
  bv (Negation p) = bv p
  bv (Conj p1 p2) = bv p1 <> bv p2
  bv (Disj p1 p2) = bv p1 <> bv p2
  bv (Imp p1 p2) = bv p1 <> bv p2
  bv (Rel r) = bv r

instance BVars Relation where
  bv RTrue = []
  bv RFalse = []
  bv (RPrim op) = bv op
  bv (REQ a b) = bv a <> bv b
  bv (RNE a b) = bv a <> bv b
  bv (RLT a b) = bv a <> bv b
  bv (RLE a b) = bv a <> bv b
  bv (RGT a b) = bv a <> bv b
  bv (RGE a b) = bv a <> bv b

instance BVars Operation where
  bv (V x) = [x]
  bv (C _) = []
  bv (BinOp a _ b) = bv a <> bv b
  bv (UnOp _ a) = bv a

class Subst a where
  subst :: VName -> VName -> a -> a

instance Subst Pred where
  subst x x' (Forall y p) = Forall y $ subst x x' p
  subst x x' (Negation p) = Negation $ subst x x' p
  subst x x' (Conj p1 p2) = Conj (subst x x' p1) (subst x x' p2)
  subst x x' (Disj p1 p2) = Disj (subst x x' p1) (subst x x' p2)
  subst x x' (Imp p1 p2) = Imp (subst x x' p1) (subst x x' p2)
  subst x x' (Rel r) = Rel $ subst x x' r

instance Subst Relation where
  subst _ _ RTrue = RTrue
  subst _ _ RFalse = RFalse
  subst x x' (RPrim a) = RPrim $ subst x x' a
  subst x x' (REQ a b) = REQ (subst x x' a) (subst x x' b)
  subst x x' (RNE a b) = RNE (subst x x' a) (subst x x' b)
  subst x x' (RLT a b) = RLT (subst x x' a) (subst x x' b)
  subst x x' (RLE a b) = RLE (subst x x' a) (subst x x' b)
  subst x x' (RGT a b) = RGT (subst x x' a) (subst x x' b)
  subst x x' (RGE a b) = RGE (subst x x' a) (subst x x' b)

instance Subst Operation where
  subst x x' (V y) = V $ if x == y then x' else y
  subst _ _ (C c) = C c
  subst x x' (BinOp a alu b) = BinOp (subst x x' a) alu (subst x x' b)
  subst x x' (UnOp unalu a) = UnOp unalu (subst x x' a)
