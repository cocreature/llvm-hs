module LLVM.Internal.FFI.NewPassManager where

import LLVM.Prelude

import Foreign.C.Types
import Foreign.Ptr

import LLVM.Internal.FFI.LLVMCTypes

data ModulePassManager

foreign import ccall unsafe "createModulePassManager" createModulePassManager ::
  LLVMBool -> Ptr ModulePassManager

foreign import ccall unsafe "disposeModulePassManager" disposeModulePassManager ::
  Ptr ModulePassManager -> IO ()
