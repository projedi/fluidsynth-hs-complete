{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell #-}
module Sound.Fluidsynth.Internal.Types
   ( AudioDriver(..)
   , FluidState
   , MidiPlayer(..)
   , FluidSynth(..)
   , MidiDriver(..)
   , MidiRouter(..)
   , MonadSettings(..)
   , ReleaseResources(..)
   , Settings
   , Synth(..)
   , withSettingsRunFluid
   -- Lenses from FluidState
   , audioDriver
   , midiPlayers
   , midiDriver
   , midiRouter
   , synth
   ) where

import Control.Applicative

import Control.Exception(bracket)
import Control.Lens(makeLenses)
import Control.Monad.Reader(MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State(MonadState, get, put)
import Control.Monad.Trans(MonadIO, liftIO)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Foreign
   ( Ptr
   , castPtr
   , newStablePtr
   , deRefStablePtr
   , freeStablePtr
   , castStablePtrToPtr
   , castPtrToStablePtr
   )

import Sound.Fluidsynth.Internal.FFI.Audio
   ( c'delete_fluid_audio_driver
   , c'delete_fluid_file_renderer
   )
import Sound.Fluidsynth.Internal.FFI.Midi
   ( c'delete_fluid_midi_router
   , c'delete_fluid_midi_driver
   , c'delete_fluid_player
   )
import Sound.Fluidsynth.Internal.FFI.Settings
   ( c'new_fluid_settings
   , c'delete_fluid_settings
   )
import Sound.Fluidsynth.Internal.FFI.Synth
   ( c'new_fluid_synth
   , c'delete_fluid_synth
   )
import Sound.Fluidsynth.Internal.FFI.Types

-- | MonadSettings exists because 'Settings' and 'FluidSynth' both
--   should be able to use "Sound.Fluidsynth.Settings" interface.
class (Applicative m, MonadIO m) => MonadSettings m where
   -- fluidDataPtr and runFluidMonad are used in C callbacks.
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

-- | 'Settings' is distinguished from 'FluidSynth' because some
--   properties must be set before creating synthesizer.
newtype Settings a = Settings (ReaderT (Ptr C'fluid_settings_t) IO a)
   deriving (Functor, Applicative, Monad, MonadIO, MonadSettings)

class ReleaseResources a where
   releaseResources :: a -> IO ()

instance ReleaseResources a => ReleaseResources (Maybe a) where
   releaseResources Nothing = return ()
   releaseResources (Just x) = releaseResources x

instance ReleaseResources a => ReleaseResources [a] where
   releaseResources = mapM_ releaseResources

newtype Synth = Synth (Ptr C'fluid_synth_t)

instance ReleaseResources Synth where
   releaseResources (Synth ptr) = c'delete_fluid_synth ptr

data AudioDriver = AudioDriver (Ptr C'fluid_audio_driver_t)
                 | FileRenderer (Ptr C'fluid_file_renderer_t)

instance ReleaseResources AudioDriver where
   releaseResources (AudioDriver aptr) =
      c'delete_fluid_audio_driver aptr
   releaseResources (FileRenderer fptr) =
      c'delete_fluid_file_renderer fptr

newtype MidiRouter = MidiRouter (Ptr C'fluid_midi_router_t)

instance ReleaseResources MidiRouter where
   releaseResources (MidiRouter ptr) = c'delete_fluid_midi_router ptr

newtype MidiDriver = MidiDriver (Ptr C'fluid_midi_driver_t)

instance ReleaseResources MidiDriver where
   releaseResources (MidiDriver ptr) = c'delete_fluid_midi_driver ptr

newtype MidiPlayer = MidiPlayer (Ptr C'fluid_player_t)

instance ReleaseResources MidiPlayer where
   releaseResources (MidiPlayer ptr) = c'delete_fluid_player ptr

data FluidState = FluidState
   { fsSettingsPtr :: Ptr C'fluid_settings_t -- we have settingsPtr in MonadSettings
   , _synth :: Synth
   , _audioDriver :: Maybe AudioDriver
   , _midiRouter :: Maybe MidiRouter
   , _midiDriver :: Maybe MidiDriver
   , _midiPlayers :: [MidiPlayer]
   }

instance ReleaseResources FluidState where
   -- The first one is handled separately in the withSettingsRunFluid
   releaseResources (FluidState _ a1 a2 a3 a4 a5) = do
      releaseResources a1
      releaseResources a2
      releaseResources a3
      releaseResources a4
      releaseResources a5

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

-- | The main monad in which everything happens.
newtype FluidSynth a = FluidSynth (ReaderFluidState a)
   deriving (Functor, Applicative, Monad, MonadIO, MonadSettings)

-- | Run the 'Settings' monad and then allocate a synthesizer and
--   run 'FluidSynth' monad. Releases all the resources at the end.
withSettingsRunFluid :: Settings () -> FluidSynth a -> IO a
withSettingsRunFluid (Settings set) (FluidSynth (ReaderFluidState syn)) =
   bracket c'new_fluid_settings c'delete_fluid_settings $ \setptr -> do
      runReaderT set setptr
      bracket (initState setptr) releaseState $ \(ref, ptr) ->
         runReaderT syn (castStablePtrToPtr ptr, ref)
 where initState setptr = do
          synptr <- c'new_fluid_synth setptr
          let flstate = FluidState
               { fsSettingsPtr = setptr
               , _synth = Synth synptr
               , _audioDriver = Nothing
               , _midiRouter = Nothing
               , _midiDriver = Nothing
               , _midiPlayers = []
               }
          ref <- newIORef flstate
          ptr <- newStablePtr ref
          return (ref, ptr)
       releaseState (ref, ptr) = do
          curstate <- readIORef ref
          freeStablePtr ptr
          releaseResources curstate
