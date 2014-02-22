{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
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
module Sound.Fluidsynth.Audio( -- * Audio drivers
                               replaceAudioDriver
                             , AudioCallback
                             , replaceAudioDriverWithCallback
                             , deleteAudioDriver
                             ) where

import Control.Applicative
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
replaceAudioDriver :: (MonadSynth m) => m Bool
replaceAudioDriver = do
   sePtr <- settingsPtr
   syPtr <- synthPtr
   aptr <- liftIO $ c'new_fluid_audio_driver sePtr syPtr
   when (aptr /= nullPtr) $
      audioDriverPtr .= Just aptr
   return $ aptr /= nullPtr

type AudioCallback m = Int -- ^ Length of audio in frames
                     -> [[Float]] -- ^ Input (unused as of fluidsynth 1.1.6)
                     -> Int -- ^ Output channel count
                     -> m [[Float]] -- ^ Output buffers(outer is a list of channels)

-- | Like 'replaceAudioDriver' but supply your own callback to generate sound samples.
replaceAudioDriverWithCallback :: (MonadSynth m) => AudioCallback m -> m Bool
replaceAudioDriverWithCallback f = do
   sePtr <- settingsPtr
   cbPtr <- liftIO $ mk'fluid_audio_func_t callback
   dataPtr <- fluidDataPtr
   aptr <- liftIO $ c'new_fluid_audio_driver2 sePtr cbPtr dataPtr
   when (aptr /= nullPtr) $
      audioDriverPtr .= Just aptr
   return $ aptr /= nullPtr
 where callback ptr len nin cin nout cout = do
          -- TODO: Error handling
          inchans <- peekArray (fromIntegral nin) cin
          inbuf <- mapM (peekArray (fromIntegral len)) inchans
          let inbuf' = map (map realToFrac) inbuf
          outbuf' <- runFluidMonad ptr $ f (fromIntegral len) inbuf' (fromIntegral nout)
          let outbuf = map (map realToFrac) outbuf'
          outchans <- peekArray (fromIntegral nout) cout
          mapM_ (uncurry pokeArray) $
             zip outchans (map (take (fromIntegral len)) outbuf)
          return 0

-- | Will remove an audio driver. Returns False when no driver were attached
deleteAudioDriver :: (MonadSynth m) => m Bool
deleteAudioDriver = do
   aptrPrev <- audioDriverPtr <<.= Nothing
   case aptrPrev of
    Nothing -> return False
    Just aptr -> do
      liftIO $ c'delete_fluid_audio_driver aptr
      return True
