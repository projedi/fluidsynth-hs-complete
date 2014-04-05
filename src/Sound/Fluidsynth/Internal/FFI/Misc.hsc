{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Misc where
#strict_import

#num FLUID_OK
#num FLUID_FAILED

#ccall fluid_is_soundfont , CString -> IO CInt
#ccall fluid_is_midifile , CString -> IO CInt
