-- |
-- Audio drivers and file renderers.
--
-- Some notes:
--
-- * As of fluidsynth 1.1.6 several instances of audio drivers at once do not work.
--
-- * I do not know yet when I have to enforce the presence of audio driver.
--
-- * There is a \"file\" audio driver which is implemented in terms of lower level
--   file renderer.
--
-- TODO: What about low level file renderer
module Sound.Fluidsynth.Audio
   ( -- * Audio drivers
     replaceAudioDriver
   , AudioCallback
   , replaceAudioDriverWithCallback
   , deleteAudioDriver
   ) where

import Control.Applicative
import Control.Lens
import Control.Monad

import Control.Monad.Trans(liftIO)
import Data.Maybe(isNothing)
import Foreign.Marshal.Array(peekArray, pokeArray)
import Foreign.Ptr(nullPtr)

import Sound.Fluidsynth.Internal.FFI.Audio
import Sound.Fluidsynth.Internal.FFI.Types
import Sound.Fluidsynth.Internal.Types

-- TODO: What about error handling.

-- | Will read current "Sound.Fluidsynth.Settings", create an according audio driver
-- and replace an existing one(if any). Returns False on error.
replaceAudioDriver :: FluidSynth Bool
replaceAudioDriver = FluidSynth $ do
   sePtr <- settingsPtr
   syPtr <- use synthPtr
   aptr <- liftIO $ c'new_fluid_audio_driver sePtr syPtr
   when (aptr /= nullPtr) $
      audioDriverPtr .= Just aptr
   return $ aptr /= nullPtr

type AudioCallback
   = Int -- ^ Length of audio in frames
   -> [[Float]] -- ^ Input (unused as of fluidsynth 1.1.6)
   -> Int -- ^ Output channel count
   -> FluidSynth [[Float]] -- ^ Output buffers(outer list is of channels)

-- | Like 'replaceAudioDriver' but supply your own callback to generate sound samples.
replaceAudioDriverWithCallback :: AudioCallback -> FluidSynth Bool
replaceAudioDriverWithCallback f = FluidSynth $ do
   sePtr <- settingsPtr
   cbPtr <- liftIO $ mk'fluid_audio_func_t callback
   dataPtr <- fluidDataPtr
   aptr <- liftIO $ c'new_fluid_audio_driver2 sePtr cbPtr dataPtr
   when (aptr /= nullPtr) $
      audioDriverPtr .= Just aptr
   return $ aptr /= nullPtr
 where callback ptr len nin cin nout cout = do
          -- TODO: Error handling: wrong lengths and such
          inchans <- peekArray (fromIntegral nin) cin
          inbuf <- mapM (peekArray (fromIntegral len)) inchans
          let inbuf' = map (map realToFrac) inbuf
          outbuf' <- runFluidMonad (f (fromIntegral len) inbuf' (fromIntegral nout)) ptr
          let outbuf = map (map realToFrac) outbuf'
          outchans <- peekArray (fromIntegral nout) cout
          mapM_ (uncurry pokeArray) $
             zip outchans (map (take (fromIntegral len)) outbuf)
          return 0

-- | Will remove an audio driver. Returns False when no driver were attached
deleteAudioDriver :: FluidSynth Bool
deleteAudioDriver = FluidSynth $ do
   aptrPrev <- audioDriverPtr <<.= Nothing
   case aptrPrev of
    Nothing -> return False
    Just aptr -> do
      liftIO $ c'delete_fluid_audio_driver aptr
      return True
