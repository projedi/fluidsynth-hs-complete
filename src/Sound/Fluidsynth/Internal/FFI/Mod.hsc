{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Mod where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#num FLUID_NUM_MOD

#starttype struct _fluid_mod_t
#field dest , CUChar
#field src1 , CUChar
#field flags1 , CUChar
#field src2 , CUChar
#field flags2 , CUChar
-- Multiplier amount
#field amount , CDouble
{- The 'next' field allows to link modulators into a list.  It is
   not used in fluid_voice.c, there each voice allocates memory for a
   fixed number of modulators.  Since there may be a huge number of
   different zones, this is more efficient.
-}
#field next , Ptr <fluid_mod_t>
#stoptype

#integral_t enum fluid_mod_flags
#num FLUID_MOD_POSITIVE
#num FLUID_MOD_NEGATIVE
#num FLUID_MOD_UNIPOLAR
#num FLUID_MOD_BIPOLAR
#num FLUID_MOD_LINEAR
#num FLUID_MOD_CONCAVE
#num FLUID_MOD_CONVEX
#num FLUID_MOD_SWITCH
#num FLUID_MOD_GC
#num FLUID_MOD_CC

#integral_t enum fluid_mod_src
#num FLUID_MOD_NONE
#num FLUID_MOD_VELOCITY
#num FLUID_MOD_KEY
#num FLUID_MOD_KEYPRESSURE
#num FLUID_MOD_CHANNELPRESSURE
#num FLUID_MOD_PITCHWHEEL
#num FLUID_MOD_PITCHWHEELSENS

#ccall fluid_mod_new , IO (Ptr <fluid_mod_t>)
#ccall fluid_mod_delete , Ptr <fluid_mod_t> -> IO ()

-- src , flags
#ccall fluid_mod_set_source1 , Ptr <fluid_mod_t> -> CInt -> CInt -> IO ()
-- src , flags
#ccall fluid_mod_set_source2 , Ptr <fluid_mod_t> -> CInt -> CInt -> IO ()
#ccall fluid_mod_set_dest , Ptr <fluid_mod_t> -> CInt -> IO ()
#ccall fluid_mod_set_amount , Ptr <fluid_mod_t> -> CDouble -> IO ()

#ccall fluid_mod_get_source1 , Ptr <fluid_mod_t> -> IO CInt
#ccall fluid_mod_get_flags1 , Ptr <fluid_mod_t> -> IO CInt
#ccall fluid_mod_get_source2 , Ptr <fluid_mod_t> -> IO CInt
#ccall fluid_mod_get_flags2 , Ptr <fluid_mod_t> -> IO CInt
#ccall fluid_mod_get_dest , Ptr <fluid_mod_t> -> IO CInt
#ccall fluid_mod_get_amount , Ptr <fluid_mod_t> -> IO CDouble

#ccall fluid_mod_test_identity , Ptr <fluid_mod_t> -> Ptr <fluid_mod_t> -> IO CInt
