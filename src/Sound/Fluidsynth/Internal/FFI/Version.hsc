#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Version where
#strict_import

-- Bindings were made for version 1.1.6
#num FLUIDSYNTH_VERSION
#num FLUIDSYNTH_VERSION_MAJOR
#num FLUIDSYNTH_VERSION_MINOR
#num FLUIDSYNTH_VERSION_MICRO

-- major, minor, micro
#ccall fluid_version , Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
#ccall fluid_version_str , IO CString
