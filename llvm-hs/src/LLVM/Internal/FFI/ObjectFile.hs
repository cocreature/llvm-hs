{-# LANGUAGE ForeignFunctionInterface #-}

-- | <https://llvm.org/doxygen/classllvm_1_1object_1_1ObjectFile.html>
module LLVM.Internal.FFI.ObjectFile where

import Foreign.Ptr

import LLVM.Prelude
import LLVM.Internal.FFI.MemoryBuffer

data ObjectFile

foreign import ccall unsafe "LLVMCreateObjectFile" createObjectFile ::
  Ptr MemoryBuffer -> IO (Ptr ObjectFile)

foreign import ccall unsafe "LLVMDisposeObjectFile" disposeObjectFile ::
  Ptr ObjectFile -> IO ()
