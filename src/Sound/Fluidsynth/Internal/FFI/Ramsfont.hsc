#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Ramsfont where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#ccall fluid_ramsfont_create_sfont , IO (Ptr <fluid_sfont_t>)
#ccall fluid_ramsfont_set_name , Ptr <fluid_ramsfont_t> -> CString -> IO CInt
-- bank number, preset number, sample, lower key range, higher key range
#ccall fluid_ramsfont_add_izone , Ptr <fluid_ramsfont_t> -> CUInt -> CUInt -> Ptr <fluid_sample_t> -> CInt -> CInt -> IO CInt
-- bank number, preset number, sample
#ccall fluid_ramsfont_remove_izone , Ptr <fluid_ramsfont_t> -> CUInt -> CUInt -> Ptr <fluid_sample_t> -> IO CInt
-- bank number, preset number, sample, generator id, generator value
#ccall fluid_ramsfont_izone_set_gen , Ptr <fluid_ramsfont_t> -> CUInt -> CUInt -> Ptr <fluid_sample_t> -> CInt -> CFloat -> IO CInt
-- bank number, preset number, sample, enable loop?, loopstart, loopend
#ccall fluid_ramsfont_izone_set_loop , Ptr <fluid_ramsfont_t> -> CUInt -> CUInt -> Ptr <fluid_sample_t> -> CInt -> CFloat -> CFloat -> IO CInt

#ccall new_fluid_ramsample , IO (Ptr <fluid_sample_t>)
#ccall delete_fluid_ramsample , Ptr <fluid_sample_t> -> IO CInt
#ccall fluid_sample_set_name , Ptr <fluid_sample_t> -> CString -> IO CInt
-- data containing 16-bit audio sample, number of samples, copy data?, root midi note
#ccall fluid_sample_set_sound_data , Ptr <fluid_sample_t> -> Ptr CShort -> CUInt -> CShort -> IO CInt
