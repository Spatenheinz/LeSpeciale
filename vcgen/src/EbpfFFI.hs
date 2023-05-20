{-# LANGUAGE ForeignFunctionInterface #-}

module EbpfFFI where
import Foreign
import Foreign.C.Types
import qualified Data.ByteString as B
import qualified Ebpf.Asm as A
import qualified Ebpf.Encode as E
import System.IO.Unsafe
-- import Protocol

-- Imports of C-functions
-- --------------------------------
foreign import ccall unsafe "lib_ebpf_qc.h create_array_map"
    c_create_array_map :: CUInt -> CUInt -> CUInt -> IO CInt

foreign import ccall unsafe "lib_ebpf_qc.h create_array_map_with_constant32"
    c_create_array_map_with_constant32 :: CUInt -> CUInt -> CUInt -> CUInt  -> IO CInt

foreign import ccall unsafe "lib_ebpf_qc.h create_array_map_with_constant64"
    c_create_array_map_with_constant64 :: CUInt -> CUInt -> CUInt -> CULong  -> IO CInt

foreign import ccall unsafe "lib_ebpf_qc.h create_array_map_with_constant_arb"
    c_create_array_map_with_constant_arb :: CUInt -> CUInt -> CUInt -> CUInt  -> IO CInt

foreign import ccall unsafe "lib_ebpf_qc.h update_array_map32"
    c_update_array_map32 :: CInt -> CUInt -> CUInt -> IO CInt

foreign import ccall unsafe "lib_ebpf_qc.h update_array_map64"
    c_update_array_map64 :: CInt -> CUInt -> CULong -> IO CInt

foreign import ccall unsafe "lib_ebpf_qc.h read_array_map32"
    c_read_array_map32 :: CInt -> CUInt -> IO CUInt

foreign import ccall unsafe "lib_ebpf_qc.h read_array_map64"
    c_read_array_map64 :: CInt -> CUInt -> IO CULong

foreign import ccall unsafe "lib_ebpf_qc.h read_array_map_arb"
    c_read_array_map_arb :: CInt -> CUInt -> CUInt -> CUInt -> IO CULong

foreign import ccall unsafe "lib_ebpf_qc.h read_array_map_arb32"
    c_read_array_map_arb32 :: CInt -> CUInt -> CUInt -> CUInt -> IO CUInt

foreign import ccall unsafe "lib_ebpf_qc.h write_array_map_arb"
    c_write_array_map_arb :: CInt -> CUInt -> CUInt -> CUInt -> CULong -> IO CULong

foreign import ccall unsafe "lib_ebpf_qc.h create_stack_map"
    c_create_stack_map :: CUInt -> CUInt -> IO CInt

foreign import ccall unsafe "lib_ebpf_qc.h stack_map_push64"
    c_stack_map_push64 :: CInt -> CULong -> IO CInt

foreign import ccall unsafe "lib_ebpf_qc.h stack_map_push32"
    c_stack_map_push32 :: CInt -> CUInt -> IO CInt

foreign import ccall unsafe "lib_ebpf_qc.h stack_map_pop64"
    c_stack_map_pop64 :: CInt -> IO CULong

foreign import ccall unsafe "lib_ebpf_qc.h stack_map_pop32"
    c_stack_map_pop32 :: CInt -> IO CUInt


foreign import ccall unsafe "lib_ebpf_qc.h load_prog"
    c_load_prog :: Ptr CChar -> CSize -> CBool -> IO CInt

foreign import ccall unsafe "lib_ebpf_qc.h trigger_prog"
    c_trigger_prog :: CInt -> IO CInt

foreign import ccall unsafe "lib_ebpf_qc.h run_bpf"
    c_run_bpf :: CInt -> Ptr CChar -> Int -> CBool -> IO CUInt

foreign import ccall unsafe "lib_ebpf_qc.h run_bpf64"
    c_run_bpf64 :: CInt -> Ptr CChar -> Int -> CBool -> IO CULong

foreign import ccall unsafe "lib_ebpf_qc.h close_fd"
    c_close_fd :: CInt -> IO CInt

-- Function definitions for the imported C-functions
-- --------------------------------
cCreateMap :: CUInt -> CUInt -> CUInt -> IO CInt
cCreateMap valueSize keySize maxEntries = c_create_array_map valueSize keySize maxEntries

cCreateMapWithConstant32 :: CUInt -> CUInt -> IO CInt
cCreateMapWithConstant32 maxEntries constant = c_create_array_map_with_constant32 4 4 maxEntries constant

cCreateMapWithConstant64 :: CUInt -> CULong -> IO CInt
cCreateMapWithConstant64 maxEntries constant = c_create_array_map_with_constant64 8 4 maxEntries constant

cCreateMapWithConstantArb :: CUInt -> CUInt -> CUInt -> IO CInt
cCreateMapWithConstantArb valueSize maxEntries constant = c_create_array_map_with_constant_arb valueSize 4 maxEntries constant


cUpdateArrayMap32 :: CInt -> CUInt -> CUInt -> IO CInt
cUpdateArrayMap32 mapFd key value = c_update_array_map32 mapFd key value

cUpdateArrayMap64 :: CInt -> CUInt -> CULong -> IO CInt
cUpdateArrayMap64 mapFd key value = c_update_array_map64 mapFd key value

cReadArrayMap32 :: CInt -> CUInt -> IO CUInt
cReadArrayMap32 mapFd key = c_read_array_map32 mapFd key

cReadArrayMap64 :: CInt -> CUInt -> IO CULong
cReadArrayMap64 mapFd key = c_read_array_map64 mapFd key

cReadArrayMapArb :: CInt -> CUInt -> CUInt -> CUInt -> IO CULong
cReadArrayMapArb mapFd key offset size = c_read_array_map_arb mapFd key offset size

cReadArrayMapArb32 :: CInt -> CUInt -> CUInt -> CUInt -> IO CUInt
cReadArrayMapArb32 mapFd key offset size = c_read_array_map_arb32 mapFd key offset size

cWriteArrayMapArb :: CInt -> CUInt -> CUInt -> CUInt -> CULong -> IO CULong
cWriteArrayMapArb mapFd key offset size value = c_write_array_map_arb mapFd key offset size value

cCreateStackMap :: CUInt -> CUInt -> IO CInt
cCreateStackMap valueSize maxEntries = c_create_stack_map valueSize maxEntries


cStackMapPush64 :: CInt -> CULong -> IO CInt
cStackMapPush64 mapFd value = c_stack_map_push64 mapFd value

cStackMapPush32 :: CInt -> CUInt -> IO CInt
cStackMapPush32 mapFd value = c_stack_map_push32 mapFd value

cStackMapPop64 :: CInt -> IO CULong
cStackMapPop64 mapFd = c_stack_map_pop64 mapFd

cStackMapPop32 :: CInt -> IO CUInt
cStackMapPop32 mapFd = c_stack_map_pop32 mapFd


cLoadProg :: B.ByteString -> IO CInt
cLoadProg prog =
  B.useAsCStringLen prog (\(ptr, blen) -> c_load_prog ptr (fromIntegral (blen `div` 8) :: CSize) 0)

cLoadProgVerbose :: B.ByteString -> IO CInt
cLoadProgVerbose prog =
  B.useAsCStringLen prog (\(ptr, blen) -> c_load_prog ptr (fromIntegral (blen `div` 8) :: CSize) 1)

cTriggerProg :: CInt -> IO CInt
cTriggerProg progFd = c_trigger_prog progFd

cRunBPF :: CInt -> B.ByteString -> IO CUInt
cRunBPF mapFd prog =
  B.useAsCStringLen prog (\(ptr, blen) -> c_run_bpf mapFd ptr (blen `div` 8) 0)

cRunBPF64 :: CInt -> B.ByteString -> IO CULong
cRunBPF64 mapFd prog =
  B.useAsCStringLen prog (\(ptr, blen) -> c_run_bpf64 mapFd ptr (blen `div` 8) 0)

cCloseFd :: CInt -> IO CInt
cCloseFd fd = c_close_fd fd

verify_barebone :: A.Program -> IO Bool
verify_barebone prog = do
    fd <- cLoadProgVerbose $ E.encodeProgram prog
    if fd <= 0 then return False else cCloseFd fd >> return True


comparisonSetup :: CInt -> CInt -> A.Program -> A.Program
comparisonSetup sizemap ctxmap prog =
  [
    A.LoadMapFd (A.Reg 1) (fromIntegral sizemap)
  , A.Store A.B32 (A.Reg 10) (Just (-4)) (Right 0)
    -- Store the key on the stack
  , A.Store A.B32 (A.Reg 10) (Just (-4)) (Right 0)
    -- Load pointer to key into R2
  , A.Binary A.B64 A.Mov (A.Reg 2) (Left (A.Reg 10))
  , A.Binary A.B64 A.Add (A.Reg 2) (Right (-4))
  -- Call Lookup element for first value, getting a pointer in R0 in return
  , A.Call 1
  -- Check if pointer is NULL
  , A.JCond A.Jne (A.Reg 0) (Right 0) 2
  -- Exit with 1 if pointer was NULL
  , A.Binary A.B64 A.Mov (A.Reg 0) (Right 1)
  , A.Exit
  -- Else, load the value into R9 temporarily
  , A.Load A.B64 (A.Reg 9) (A.Reg 0) (Nothing)
  -- Now setup the ctxmap st. r1 is a pointer to the beginning of memory
  , A.LoadMapFd (A.Reg 1) (fromIntegral ctxmap)
  , A.Store A.B32 (A.Reg 10) (Just (-4)) (Right 0)
    -- Store the key on the stack
  , A.Store A.B32 (A.Reg 10) (Just (-4)) (Right 0)
    -- Load pointer to key into R2
  , A.Binary A.B64 A.Mov (A.Reg 2) (Left (A.Reg 10))
  , A.Binary A.B64 A.Add (A.Reg 2) (Right (-4))
  -- Call Lookup element for first value, getting a pointer in R0 in return
  , A.Call 1
  -- Check if pointer is NULL
  , A.JCond A.Jne (A.Reg 0) (Right 0) 2
  -- Exit with 1 if pointer was NULL
  , A.Binary A.B64 A.Mov (A.Reg 0) (Right 1)
  , A.Exit
  , A.Binary A.B64 A.Mov (A.Reg 1) (Left (A.Reg 0))
  , A.Binary A.B64 A.Mov (A.Reg 2) (Left (A.Reg 9))
  ] ++ prog
