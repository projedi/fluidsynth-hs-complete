#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Voice where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types
import Sound.Fluidsynth.Internal.FFI.Gen

#ccall fluid_voice_update_param , Ptr <fluid_voice_t> -> <fluid_gen_type> -> IO ()

#integral_t enum fluid_voice_add_mod
#num FLUID_VOICE_OVERWRITE
#num FLUID_VOICE_ADD
#num FLUID_VOICE_DEFAULT

#ccall fluid_voice_add_mod , Ptr <fluid_voice_t> -> Ptr <fluid_mod_t> -> <fluid_voice_add_mod> -> IO ()

#ccall fluid_voice_gen_set , Ptr <fluid_voice_t> -> <fluid_gen_type> -> CFloat -> IO ()
#ccall fluid_voice_gen_get , Ptr <fluid_voice_t> -> <fluid_gen_type> -> IO CFloat
#ccall fluid_voice_gen_incr , Ptr <fluid_voice_t> -> <fluid_gen_type> -> CFloat -> IO ()

#ccall fluid_voice_get_id , Ptr <fluid_voice_t> -> IO CUInt
#ccall fluid_voice_is_playing , Ptr <fluid_voice_t> -> IO CInt
#ccall fluid_voice_optimize_sample , Ptr <fluid_sample_t> -> IO CInt
