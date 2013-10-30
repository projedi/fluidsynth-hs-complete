#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Audio where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#ccall new_fluid_audio_driver , Ptr <fluid_settings_t> -> Ptr <fluid_synth_t> -> IO (Ptr <fluid_audio_driver_t>)

#callback_t fluid_audio_func_t , Ptr () -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt

#ccall new_fluid_audio_driver2 , Ptr <fluid_settings_t> -> <fluid_audio_func_t> -> Ptr () -> IO (Ptr <fluid_audio_driver_t>)

#ccall delete_fluid_audio_driver , Ptr <fluid_audio_driver_t> -> IO ()

#ccall new_fluid_file_renderer , Ptr <fluid_synth_t> -> IO (Ptr <fluid_file_renderer_t>)
#ccall fluid_file_renderer_process_block , Ptr <fluid_file_renderer_t> -> IO CInt
#ccall delete_fluid_file_renderer , Ptr <fluid_file_renderer_t> -> IO ()
