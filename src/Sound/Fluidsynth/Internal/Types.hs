{-# LANGUAGE TypeFamilies #-}
module Sound.Fluidsynth.Internal.Types( MonadFluid(..)
                                      , assign
                                      , MonadSettings(..)
                                      , MonadSynth(..)
                                      ) where

import Control.Applicative
import Control.Monad

import Control.Lens(LensLike')
import qualified Control.Lens as Lens
import Control.Monad.Trans(MonadIO)
import Foreign.Ptr(Ptr)

import Sound.Fluidsynth.Internal.FFI.Types

class (Applicative m, MonadIO m) => MonadFluid m where
   fluidDataPtr :: m (Ptr ())
   runFluidMonad :: Ptr () -> m a -> IO a

class (MonadFluid m) => MonadSettings m where
   settingsPtr :: m (Ptr C'fluid_settings_t)

-- Copying a lens interface for MonadState
class (MonadSettings m) => MonadSynth m where
   type LensSource m
   use :: LensLike' m (LensSource m) a -> m a
   uses :: LensLike' m (LensSource m) a -> (a -> r) -> m r
   (.=) :: LensLike' m (LensSource m) a -> a -> m ()
   (<.=) :: LensLike' m (LensSource m) a -> a -> m b
   (<<.=) :: LensLike' m (LensSource m) a -> a -> m b
   (%=) :: LensLike' m (LensSource m) a -> (a -> a) -> m ()
   (<%=) :: LensLike' m (LensSource m) a -> (a -> a) -> m a
   (<<%=) :: LensLike' m (LensSource m) a -> (a -> a) -> m a
   (%%=) :: LensLike' m (LensSource m) a -> (a -> (r,a)) -> m r
   synthPtr :: m (Ptr C'fluid_synth_t)
   audioDriverPtr :: LensSynth m (Maybe (Ptr C'fluid_audio_driver_t))

type LensSynth m a = LensLike' m (LensSource m) a

assign :: (MonadSynth m) => LensLike' m (LensSource m) a -> a -> m ()
assign = (.=)
