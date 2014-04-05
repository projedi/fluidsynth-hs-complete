module Sound.Fluidsynth.Misc
   ( isSoundFont
   , isMidiFile
   ) where

import Control.Applicative

import Foreign.C.Types(CInt)
import Foreign.C.String(withCString)

import Sound.Fluidsynth.Internal.FFI.Misc

toBool :: CInt -> Bool
toBool 0 = False
toBool _ = True

isSoundFont :: String -> IO Bool
isSoundFont str =
   toBool <$> withCString str c'fluid_is_soundfont

isMidiFile :: String -> IO Bool
isMidiFile str =
   toBool <$> withCString str c'fluid_is_midifile
