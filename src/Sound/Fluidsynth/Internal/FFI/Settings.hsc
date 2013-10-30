#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Settings where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#num FLUID_HINT_BOUNDED_BELOW
#num FLUID_HINT_BOUNDED_ABOVE
#num FLUID_HINT_TOGGLED
#num FLUID_HINT_SAMPLE_RATE
#num FLUID_HINT_LOGARITHMIC
#num FLUID_HINT_FILENAME
#num FLUID_HINT_OPTIONLIST

#integral_t enum fluid_types_enum
#num FLUID_NO_TYPE
#num FLUID_NUM_TYPE
#num FLUID_INT_TYPE
#num FLUID_STR_TYPE
#num FLUID_SET_TYPE

#ccall new_fluid_settings , IO (Ptr <fluid_settings_t>)
#ccall delete_fluid_settings , Ptr <fluid_settings_t> -> IO ()

#ccall fluid_settings_get_type , Ptr <fluid_settings_t> -> CString -> IO CInt
#ccall fluid_settings_get_hints , Ptr <fluid_settings_t> -> CString -> IO CInt
#ccall fluid_settings_is_realtime , Ptr <fluid_settings_t> -> CString -> IO CInt
-- name, value
#ccall fluid_settings_setstr , Ptr <fluid_settings_t> -> CString -> CString -> IO CInt
-- name, buf, buflength
#ccall fluid_settings_copystr , Ptr <fluid_settings_t> -> CString -> CString -> CInt -> IO CInt
#ccall fluid_settings_dupstr , Ptr <fluid_settings_t> -> CString -> Ptr CString -> IO CInt
#ccall fluid_settings_getstr , Ptr <fluid_settings_t> -> CString -> Ptr CString -> IO CInt
#ccall fluid_settings_getstr_default , Ptr <fluid_settings_t> -> CString -> IO CString
#ccall fluid_settings_str_equal , Ptr <fluid_settings_t> -> CString -> CString -> IO CInt
#ccall fluid_settings_setnum , Ptr <fluid_settings_t> -> CString -> CDouble -> IO CInt
#ccall fluid_settings_getnum , Ptr <fluid_settings_t> -> CString -> Ptr CDouble -> IO CInt
#ccall fluid_settings_getnum_default , Ptr <fluid_settings_t> -> CString -> IO CDouble
#ccall fluid_settings_getnum_range , Ptr <fluid_settings_t> -> CString -> Ptr CDouble -> Ptr CDouble -> IO ()
#ccall fluid_settings_setint , Ptr <fluid_settings_t> -> CString -> CInt -> IO CInt
#ccall fluid_settings_getint , Ptr <fluid_settings_t> -> CString -> Ptr CInt -> IO CInt
#ccall fluid_settings_getint_default , Ptr <fluid_settings_t> -> CString -> IO CInt
#ccall fluid_settings_getint_range , Ptr <fluid_settings_t> -> CString -> Ptr CInt -> Ptr CInt -> IO ()

#callback_t fluid_settings_foreach_option_t , Ptr () -> CString -> CString -> IO ()

#ccall fluid_settings_foreach_option , Ptr <fluid_settings_t> -> CString -> Ptr () -> <fluid_settings_foreach_option_t> -> IO ()
#ccall fluid_settings_option_count , Ptr <fluid_settings_t> -> CString -> IO CInt
#ccall fluid_settings_option_concat , Ptr <fluid_settings_t> -> CString -> CString -> IO CString

#callback_t fluid_settings_foreach_t , Ptr () -> CString -> <fluid_types_enum> -> IO ()

#ccall fluid_settings_foreach , Ptr <fluid_settings_t> -> Ptr () -> <fluid_settings_foreach_t> -> IO ()
