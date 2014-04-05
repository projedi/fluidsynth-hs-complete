{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Sfont where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#num FLUID_PRESET_SELECTED
#num FLUID_PRESET_UNSELECTED
#num FLUID_SAMPLE_DONE

#starttype struct _fluid_sfloader_t
#field data , Ptr ()
#field free , FunPtr (Ptr <fluid_sfloader_t> -> IO CInt)
#field load , FunPtr (Ptr <fluid_sfloader_t> -> CString -> IO (Ptr <fluid_sfont_t>))
#stoptype

#starttype struct _fluid_sfont_t
#field data , Ptr ()
#field id , CUInt
#field free , FunPtr (Ptr <fluid_sfont_t> -> IO CInt)
#field get_name , FunPtr (Ptr <fluid_sfont_t> -> IO CString)
#field get_preset , FunPtr (Ptr <fluid_sfont_t> -> CUInt -> CUInt -> IO (Ptr <fluid_preset_t>))
#field iteration_start , FunPtr (Ptr <fluid_sfont_t> -> IO ())
#field iteration_next , FunPtr (Ptr <fluid_sfont_t> -> Ptr <fluid_preset_t> -> IO CInt)
#stoptype

-- TODO: There were two #defines of a function in a header

#starttype struct _fluid_preset_t
#field data , Ptr ()
#field sfont , Ptr <fluid_sfont_t>
#field free , FunPtr (Ptr <fluid_preset_t> -> IO CInt)
#field get_name , FunPtr (Ptr <fluid_preset_t> -> IO CString)
#field get_banknum , FunPtr (Ptr <fluid_preset_t> -> IO CInt)
#field get_num , FunPtr (Ptr <fluid_preset_t> -> IO CInt)
#field noteon , FunPtr (Ptr <fluid_preset_t> -> Ptr <fluid_synth_t> -> CInt -> CInt -> CInt -> IO CInt)
#field notify , FunPtr (Ptr <fluid_preset_t> -> CInt -> CInt)
#stoptype

-- TODO: struct _fluid_sample_t

#num FLUID_SAMPLETYPE_MONO
#num FLUID_SAMPLETYPE_RIGHT
#num FLUID_SAMPLETYPE_LEFT
#num FLUID_SAMPLETYPE_LINKED
#num FLUID_SAMPLETYPE_ROM
