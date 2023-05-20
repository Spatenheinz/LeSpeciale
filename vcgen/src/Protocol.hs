{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | This module is used for defining the common protocol which we use.

module Protocol where

import Foreign.C.Types
import Ebpf.Asm as A
import qualified Imp as I


header :: CInt -> A.Program
header mapFd =
  [
    -- Load the file descriptor
    LoadMapFd  (Reg 1) (fromIntegral mapFd)
    -- Store the key on the stack
  , Store B32 (Reg 10) (Just (-4)) (Right 0)
    -- Load pointer to key into R2
  , Binary B64 Mov (Reg 2) (Left (Reg 10))
  , Binary B64 Add (Reg 2) (Right (-4))
  -- Call Lookup element for first value, getting a pointer in R0 in return
  , Call 1
  -- Check if pointer is NULL
  , JCond Jne (Reg 0) (Right 0) 2
  -- Exit with 1 if pointer was NULL
  , Binary B64 Mov (Reg 0) (Right 1)
  , Exit
  -- Initialize All other registers , so we don't discard most programs because
  -- of uninitialised reads of those registers
  , Binary B64 Mov (Reg 1) (Right 0)
  , Binary B64 Mov (Reg 2) (Right 0)
  , Binary B64 Mov (Reg 3) (Right 0)
  , Binary B64 Mov (Reg 4) (Right 0)
  , Binary B64 Mov (Reg 5) (Right 0)
  , Binary B64 Mov (Reg 6) (Right 0)
  , Binary B64 Mov (Reg 7) (Right 0)
  , Binary B64 Mov (Reg 8) (Right 0)
  , Binary B64 Mov (Reg 9) (Right 0)
  ]

footer :: CInt -> A.Program
footer mapFd =
  [
    -- Do a read with R0
    Load B64 (Reg 7) (Reg 0) (Nothing)
    -- And store that value in the map
  , LoadMapFd (Reg 1) (fromIntegral mapFd)
  -- Put key on stack
  , Store B32 (Reg 10) (Just (-4)) (Right 0)
  , Binary B64 Mov (Reg 2) (Left (Reg 10))
  , Binary B64 Add (Reg 2) (Right (-4))
  , Call 1 -- lookup_map to get a pointer
  , JCond Jne (Reg 0) (Right 0) 2
  , Binary B64 Mov (Reg 0) (Right 1)
  , Exit
  -- Then store the value in the map
  , Store B64 (Reg 0) Nothing (Left (Reg 7))
  , Store B64 (Reg 0) (Just 8) (Left (Reg 6))
  -- And exit with 0
  , Binary B64 Mov (Reg 0) (Right 0)
  , Exit
  ]

-- Transformations on programs

smush :: CInt -> A.Program -> A.Program
smush mapFd prog = header mapFd ++ replace prog (footer mapFd)


class TransformableAST a where
  replace :: a -> a -> a


instance TransformableAST I.Instr where
  replace (I.Seq cmd rest) e = I.Seq cmd (replace rest e)
    -- is this a good approach???
  replace (I.Cond l op r ifc elc) e = I.Cond l op r (replace ifc e) (replace elc e)
  replace I.Exit e = e


instance TransformableAST A.Program where
  replace [] _ = []
  replace (JCond cmp r rim offset : prog) snip =
    JCond cmp r rim (offset + fromIntegral (length snip - 1)) : replace prog snip
  replace (Exit: prog) snip = snip ++ replace prog snip
  replace (i:prog) snip = i : replace prog snip
