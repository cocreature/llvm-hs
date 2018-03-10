module LLVM.Internal.OrcJIT.IRCompileLayer where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.IORef
import Foreign.Ptr

import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.OrcJIT.CompileLayer as FFI
import qualified LLVM.Internal.FFI.OrcJIT.IRCompileLayer as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import LLVM.Internal.OrcJIT
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Internal.Target
import LLVM.Internal.OrcJIT.LinkingLayer (LinkingLayer(..))

-- | 'IRCompileLayer' compiles modules immediately when they are
-- added. It parametrized by a 'LinkingLayer' which handles linking of
-- the generated object files.
data IRCompileLayer linkingLayer =
  IRCompileLayer {
    compileLayer :: !(Ptr FFI.IRCompileLayer),
    dataLayout :: !(Ptr FFI.DataLayout),
    cleanupActions :: !(IORef [IO ()])
  }
  deriving Eq

instance CompileLayer (IRCompileLayer l) where
  getCompileLayer = FFI.upCast . compileLayer
  getDataLayout = dataLayout
  getCleanups = cleanupActions

-- | Create a new 'IRCompileLayer'.
--
-- When the layer is no longer needed, it should be disposed using 'disposeCompileLayer.
newIRCompileLayer :: LinkingLayer l => l -> TargetMachine -> IO (IRCompileLayer l)
newIRCompileLayer linkingLayer (TargetMachine tm) = flip runAnyContT return $ do
  cleanups <- liftIO (newIORef [])
  dl <- createRegisteredDataLayout (TargetMachine tm) cleanups
  cl <- anyContToM $
    bracketOnError
      (FFI.createIRCompileLayer (getLinkingLayer linkingLayer) tm)
      (FFI.disposeCompileLayer . FFI.upCast)
  return (IRCompileLayer cl dl cleanups)

-- | 'bracket'-style wrapper around 'newIRCompileLayer' and 'disposeCompileLayer'.
withIRCompileLayer :: LinkingLayer l => l -> TargetMachine -> (IRCompileLayer l -> IO a) -> IO a
withIRCompileLayer linkingLayer tm =
  bracket (newIRCompileLayer linkingLayer tm) disposeCompileLayer
