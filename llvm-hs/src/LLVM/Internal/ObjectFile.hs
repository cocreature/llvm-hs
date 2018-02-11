{-# LANGUAGE MultiParamTypeClasses #-}

module LLVM.Internal.ObjectFile (
    ObjectFile,
    createObjectFile,
    disposeObjectFile
  ) where

import Control.Monad.IO.Class
import Control.Monad.AnyCont
import Foreign.Ptr

import LLVM.Prelude
import LLVM.Internal.Coding
import LLVM.Internal.MemoryBuffer
import qualified LLVM.Internal.FFI.ObjectFile as FFI

data ObjectFile = ObjectFile (Ptr FFI.ObjectFile)

instance MonadIO m => EncodeM m ObjectFile (Ptr FFI.ObjectFile) where
  encodeM (ObjectFile ref) = return ref

instance MonadIO m => DecodeM m ObjectFile (Ptr FFI.ObjectFile) where
  decodeM = return . ObjectFile

-- | Dispose of an 'ObjectFile'.
disposeObjectFile :: ObjectFile -> IO ()
disposeObjectFile obj = FFI.disposeObjectFile =<< (encodeM obj)

-- | Create a object file which can later be linked with the
-- 'LLVM.OrcJIT.LinkingLayer'.
--
-- Note that the file at `path` should already be a compiled object file i.e a
-- `.o` file. This does *not* compile source files.
createObjectFile :: FilePath -> IO ObjectFile
createObjectFile path = flip runAnyContT return $ do
  buf <- encodeM (File path)
  obj <- liftIO $ FFI.createObjectFile buf
  when (obj == nullPtr) $ error "LLVMCreateObjectFile returned a null pointer."
  decodeM obj
