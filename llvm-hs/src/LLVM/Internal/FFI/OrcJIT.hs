{-# LANGUAGE MultiParamTypeClasses, ForeignFunctionInterface #-}

module LLVM.Internal.FFI.OrcJIT where

import LLVM.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.Internal.FFI.DataLayout
import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.ObjectFile

data JITSymbol
data LambdaResolver

data LinkingLayer

data ObjectLinkingLayer

instance ChildOf LinkingLayer ObjectLinkingLayer

newtype TargetAddress = TargetAddress Word64

type SymbolResolverFn = CString -> Ptr JITSymbol -> IO ()

newtype ObjectHandle = ObjectHandle Word

foreign import ccall "wrapper" wrapSymbolResolverFn ::
  SymbolResolverFn -> IO (FunPtr SymbolResolverFn)

foreign import ccall safe "LLVM_Hs_disposeJITSymbol" disposeSymbol ::
  Ptr JITSymbol -> IO ()

foreign import ccall safe "LLVM_Hs_createLambdaResolver" createLambdaResolver ::
  FunPtr SymbolResolverFn ->
  FunPtr SymbolResolverFn ->
  IO (Ptr LambdaResolver)

foreign import ccall safe "LLVM_Hs_disposeLambdaResolver" disposeLambdaResolver ::
  Ptr LambdaResolver -> IO ()

foreign import ccall safe "LLVM_Hs_createObjectLinkingLayer" createObjectLinkingLayer ::
  IO (Ptr ObjectLinkingLayer)

foreign import ccall safe "LLVM_Hs_LinkingLayer_dispose" disposeLinkingLayer ::
  Ptr LinkingLayer -> IO ()

foreign import ccall safe "LLVM_Hs_LinkingLayer_addObject" addObjectFile ::
  Ptr LinkingLayer ->
  Ptr ObjectFile ->
  Ptr LambdaResolver ->
  Ptr (OwnerTransfered CString) ->
  IO ObjectHandle

foreign import ccall safe "LLVM_Hs_JITSymbol_getAddress" getAddress ::
  Ptr JITSymbol -> Ptr (OwnerTransfered CString) -> IO TargetAddress

foreign import ccall safe "LLVM_Hs_JITSymbol_getFlags" getFlags ::
  Ptr JITSymbol -> IO JITSymbolFlags

foreign import ccall safe "LLVM_Hs_setJITSymbol" setJITSymbol ::
  Ptr JITSymbol -> TargetAddress -> JITSymbolFlags -> IO ()

foreign import ccall safe "LLVM_Hs_getMangledSymbol" getMangledSymbol ::
  Ptr CString -> CString -> Ptr DataLayout -> IO ()

foreign import ccall safe "LLVM_Hs_disposeMangledSymbol" disposeMangledSymbol ::
  CString -> IO ()
