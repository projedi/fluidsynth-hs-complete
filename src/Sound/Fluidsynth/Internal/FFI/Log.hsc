#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Log where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#integral_t enum fluid_log_level
#num FLUID_PANIC
#num FLUID_ERR
#num FLUID_WARN
#num FLUID_INFO
#num FLUID_DBG
#num LAST_LOG_LEVEL

#callback_t fluid_log_function_t , <fluid_log_level> -> CString -> Ptr () -> IO ()

#ccall fluid_set_log_function , <fluid_log_level> -> <fluid_log_function_t> -> Ptr () -> IO <fluid_log_function_t>

#ccall fluid_default_log_function , <fluid_log_level> -> CString -> Ptr () -> IO ()
