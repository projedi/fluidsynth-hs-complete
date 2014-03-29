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
-- * File renderer also cannot be used simultaneously with another audio driver:
--   its core function, process_block rolls synth forward.
module Sound.Fluidsynth.Audio
   ( replaceAudioDriver
   , AudioCallback
   , replaceAudioDriverWithCallback
   , replaceFileRenderer
   , fileRendererProcessBlock
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
import Sound.Fluidsynth.Internal.FFI.Misc
import Sound.Fluidsynth.Internal.FFI.Types
import Sound.Fluidsynth.Internal.Types

-- TODO: What about error handling.

-- | Will read current "Sound.Fluidsynth.Settings", create an according audio driver
--   and replace an existing audio driver(if any) or file renderer.
--   Returns False on error.
replaceAudioDriver :: FluidSynth Bool
replaceAudioDriver = FluidSynth $ do
   sePtr <- settingsPtr
   syPtr <- use synthPtr
   aptr <- liftIO $ c'new_fluid_audio_driver sePtr syPtr
   when (aptr /= nullPtr) $
      audioDriver .= Just (AudioDriver aptr)
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
      audioDriver .= Just (AudioDriver aptr)
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

-- | Will read current "Sound.Fluidsynth.Settings", create an according file renderer
--   and replace an existing file renderer(if any) or audio driver.
--   Returns False on error.
replaceFileRenderer :: FluidSynth Bool
replaceFileRenderer = FluidSynth $ do
   syPtr <- use synthPtr
   fptr <- liftIO $ c'new_fluid_file_renderer syPtr
   when (fptr /= nullPtr) $
      audioDriver .= Just (FileRenderer fptr)
   return $ fptr /= nullPtr

-- TODO: Maybe throw an exception when no renderer is attached.
-- | Will get audio frames from synthesizer and write them to the file.
--   Returns False on error(failure to write or when no file renderer is attached).
fileRendererProcessBlock :: FluidSynth Bool
fileRendererProcessBlock = FluidSynth $ do
   aptr <- use audioDriver
   case aptr of
    Just (FileRenderer fptr) -> do
       res <- liftIO $ c'fluid_file_renderer_process_block fptr
       return $ res == c'FLUID_OK
    _ -> return False

-- | Will remove an audio driver or file renderer(if any).
deleteAudioDriver :: FluidSynth ()
deleteAudioDriver = FluidSynth $ do
   aptrPrev <- audioDriver <<.= Nothing
   liftIO $ releaseResources aptrPrev
