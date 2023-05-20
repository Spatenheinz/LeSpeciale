{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic
import EbpfFFI
--import UbpfFFI
import Foreign.C.Types
import Ebpf.Asm
import Ebpf.Encode
import qualified Imp as I
import Transpile
import Eval
import Data.Word
import Data.Int (Int64)
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import System.IO.Unsafe
import Protocol
import Generators

main :: IO ()
main = defaultMain testsuite

testsuite =
  testGroup "Tests"
  [
  ]
