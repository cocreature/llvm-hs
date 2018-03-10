{-# LANGUAGE MultiParamTypeClasses #-}
module LLVM.Internal.OrcJIT where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (packCString, useAsCString)
import Data.IORef
import Foreign.C.String
import Foreign.Ptr

import LLVM.Internal.Coding
import LLVM.Internal.Target
import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.Internal.FFI.OrcJIT as FFI
import qualified LLVM.Internal.FFI.Target as FFI

-- | A mangled symbol which can be used in 'findSymbol'. This can be
-- created using 'mangleSymbol'.
newtype MangledSymbol = MangledSymbol ByteString
  deriving (Show, Eq, Ord)

instance EncodeM (AnyContT IO) MangledSymbol CString where
  encodeM (MangledSymbol bs) = anyContToM $ useAsCString bs

instance MonadIO m => DecodeM m MangledSymbol CString where
  decodeM str = liftIO $ MangledSymbol <$> packCString str

data JITSymbolFlags =
  JITSymbolFlags {
    jitSymbolWeak :: !Bool, -- ^ Is this a weak symbol?
    jitSymbolExported :: !Bool -- ^ Is this symbol exported?
  }
  deriving (Show, Eq, Ord)

data JITSymbol =
  JITSymbol {
    jitSymbolAddress :: !WordPtr, -- ^ The address of the symbol. If
                                  -- you’ve looked up a function, you
                                  -- need to cast this to a 'FunPtr'.
    jitSymbolFlags :: !JITSymbolFlags -- ^ The flags of this symbol.
  }
  deriving (Show, Eq, Ord)

type SymbolResolverFn = MangledSymbol -> IO JITSymbol

-- | Specifies how external symbols in a module added to a
-- 'CompielLayer' should be resolved.
data SymbolResolver =
  SymbolResolver {
    -- | This is used to find symbols in the same logical dynamic
    -- library as the module referencing them.
    dylibResolver :: !SymbolResolverFn,
    -- | When 'dylibResolver' fails to resolve a symbol,
    -- 'externalResolver' is used as a fallback to find external symbols.
    externalResolver :: !SymbolResolverFn
  }

instance Monad m => EncodeM m JITSymbolFlags FFI.JITSymbolFlags where
  encodeM f = return $ foldr1 (.|.) [
      if a f
         then b
         else 0
    | (a,b) <- [
          (jitSymbolWeak, FFI.jitSymbolFlagsWeak),
          (jitSymbolExported, FFI.jitSymbolFlagsExported)
        ]
    ]

instance Monad m => DecodeM m JITSymbolFlags FFI.JITSymbolFlags where
  decodeM f =
    return $ JITSymbolFlags {
      jitSymbolWeak = FFI.jitSymbolFlagsWeak .&. f /= 0,
      jitSymbolExported = FFI.jitSymbolFlagsExported .&. f /= 0
    }

instance MonadIO m => EncodeM m JITSymbol (Ptr FFI.JITSymbol -> IO ()) where
  encodeM (JITSymbol addr flags) = return $ \jitSymbol -> do
    flags' <- encodeM flags
    FFI.setJITSymbol jitSymbol (FFI.TargetAddress (fromIntegral addr)) flags'

instance (MonadIO m, MonadAnyCont IO m) => DecodeM m JITSymbol (Ptr FFI.JITSymbol) where
  decodeM jitSymbol = do
    errMsg <- alloca
    FFI.TargetAddress addr <- liftIO $ FFI.getAddress jitSymbol errMsg
    -- TODO read error message and throw exception
    flags <- liftIO $ decodeM =<< FFI.getFlags jitSymbol
    return (JITSymbol (fromIntegral addr) flags)

instance MonadIO m =>
  EncodeM m SymbolResolver (IORef [IO ()] -> IO (Ptr FFI.LambdaResolver)) where
  encodeM (SymbolResolver dylib external) = return $ \cleanups -> do
    dylib' <- allocFunPtr cleanups (encodeM dylib)
    external' <- allocFunPtr cleanups (encodeM external)
    allocWithCleanup cleanups (FFI.createLambdaResolver dylib' external') FFI.disposeLambdaResolver

instance MonadIO m => EncodeM m SymbolResolverFn (FunPtr FFI.SymbolResolverFn) where
  encodeM callback =
    liftIO $ FFI.wrapSymbolResolverFn
      (\symbol result -> do
         setSymbol <- encodeM =<< callback =<< decodeM symbol
         setSymbol result)

-- | Allocate the resource and register it for cleanup.
allocWithCleanup :: IORef [IO ()] -> IO a -> (a -> IO ()) -> IO a
allocWithCleanup cleanups alloc free = mask $ \restore -> do
  a <- restore alloc
  modifyIORef cleanups (free a :)
  pure a

-- | allocate a function pointer and register it for cleanup.
allocFunPtr :: IORef [IO ()] -> IO (FunPtr a) -> IO (FunPtr a)
allocFunPtr cleanups alloc = allocWithCleanup cleanups alloc freeHaskellFunPtr

createRegisteredDataLayout :: (MonadAnyCont IO m) => TargetMachine -> IORef [IO ()] -> m (Ptr FFI.DataLayout)
createRegisteredDataLayout (TargetMachine tm) cleanups =
  let createDataLayout = do
        dl <- FFI.createTargetDataLayout tm
        modifyIORef' cleanups (FFI.disposeDataLayout dl :)
        pure dl
  in anyContToM $ bracketOnError createDataLayout FFI.disposeDataLayout
