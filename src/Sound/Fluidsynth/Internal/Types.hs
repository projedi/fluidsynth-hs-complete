{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell #-}
module Sound.Fluidsynth.Internal.Types
   ( AudioDriver(..)
   , FluidState
   , FluidSynth(..)
   , MonadSettings(..)
   , Settings
   , withSettingsRunFluid
   -- Lenses from FluidState
   , synthPtr
   , audioDriver
   ) where

import Control.Applicative
import Control.Monad

import Control.Exception(bracket)
import Control.Lens(makeLenses)
import Control.Monad.Reader(MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State(MonadState, get, put)
import Control.Monad.Trans(MonadIO, liftIO)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Foreign
   ( Ptr
   , castPtr
   , StablePtr
   , newStablePtr
   , deRefStablePtr
   , freeStablePtr
   , castStablePtrToPtr
   , castPtrToStablePtr
   )

import Sound.Fluidsynth.Internal.FFI.Types
import Sound.Fluidsynth.Internal.FFI.Settings
   ( c'new_fluid_settings
   , c'delete_fluid_settings
   )
import Sound.Fluidsynth.Internal.FFI.Synth
   ( c'new_fluid_synth
   , c'delete_fluid_synth
   )

-- MonadSettings exists because Settings and FluidSynth both
-- should be able to use Sound.Fluidsynth.Settings interface.
-- Settings is distinguished from FluidSynth because some
-- properties must be set before creating C'fluid_synth_t.

-- fluidDataPtr and runFluidMonad are used in C callbacks.
class (Applicative m, MonadIO m) => MonadSettings m where
   -- Contains data needed to reconstruct a monad in runFluidMonad.
   -- Must be alive for the entire life of the monad.
   fluidDataPtr :: m (Ptr ())
   -- Reconstruct and run the monad.
   runFluidMonad :: m a -> Ptr () -> IO a
   settingsPtr :: m (Ptr C'fluid_settings_t)

instance MonadSettings (ReaderT (Ptr C'fluid_settings_t) IO) where
   fluidDataPtr = castPtr <$> ask
   runFluidMonad m ptr = runReaderT m (castPtr ptr)
   settingsPtr = ask

newtype Settings a = Settings (ReaderT (Ptr C'fluid_settings_t) IO a)
   deriving (Functor, Applicative, Monad, MonadIO, MonadSettings)

data AudioDriver = AudioDriver (Ptr C'fluid_audio_driver_t)
                 | FileRenderer (Ptr C'fluid_file_renderer_t)

data FluidState = FluidState
   { fsSettingsPtr :: Ptr C'fluid_settings_t -- we have settingsPtr in MonadSettings
   , _synthPtr :: Ptr C'fluid_synth_t
   , _audioDriver :: Maybe AudioDriver
   }

makeLenses ''FluidState

-- First argument in the environment is a StablePtr to the second
newtype ReaderFluidState a = ReaderFluidState (ReaderT (Ptr (), IORef FluidState) IO a)
   deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Ptr (), IORef FluidState))

-- This is the instance for which I had to wrap ReaderT into ReaderFluidState
instance MonadState FluidState ReaderFluidState where
   get = do
      ref <- snd <$> ask
      liftIO $ readIORef ref
   put val = do
      ref <- snd <$> ask
      liftIO $ writeIORef ref val

instance MonadSettings ReaderFluidState where
   fluidDataPtr = fst <$> ask
   runFluidMonad (ReaderFluidState m) ptr = do
      ref <- liftIO $ deRefStablePtr (castPtrToStablePtr ptr)
      runReaderT m (ptr, ref)
   settingsPtr = fsSettingsPtr <$> get

newtype FluidSynth a = FluidSynth (ReaderFluidState a)
   deriving (Functor, Applicative, Monad, MonadIO, MonadSettings)

withSettingsRunFluid :: Settings () -> FluidSynth a -> IO a
withSettingsRunFluid (Settings set) (FluidSynth (ReaderFluidState syn)) = do
   bracket c'new_fluid_settings c'delete_fluid_settings $ \setptr -> do
      runReaderT set setptr
      bracket (c'new_fluid_synth setptr) c'delete_fluid_synth $ \synptr -> do
         let flstate = FluidState
              { fsSettingsPtr = setptr
              , _synthPtr = synptr
              , _audioDriver = Nothing
              }
         ref <- newIORef flstate
         bracket (newStablePtr ref) freeStablePtr $ \ptr ->
            runReaderT syn (castStablePtrToPtr ptr, ref)
         -- TODO: This is the point when we need to look at FluidState and deallocate
         -- TODO: all the pointers(e.g. audio driver)
