#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Synth where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#num FLUID_SYNTH_CHANNEL_INFO_NAME_SIZE

#starttype struct _fluid_synth_channel_info_t
-- This one is actually one bit + 31 reserved flag bits
-- TODO: Use this bitfield
-- #field assigned , CInt
#field sfont_id , CInt
#field bank , CInt
#field program , CInt
#array_field name , CChar
-- This is another reserved data spot
#array_field reserved , CChar
#stoptype

#ccall new_fluid_synth , Ptr <fluid_settings_t> -> IO (Ptr <fluid_synth_t>)
-- Originally returned CInt
#ccall delete_fluid_synth , Ptr <fluid_synth_t> -> IO ()
#ccall fluid_synth_get_settings , Ptr <fluid_synth_t> -> IO (Ptr <fluid_settings_t>)

-- MIDI channel messages

-- chan, key, vel
#ccall fluid_synth_noteon , Ptr <fluid_synth_t> -> CInt -> CInt -> CInt -> IO CInt
-- chan, key
#ccall fluid_synth_noteoff , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt
-- chan, ctrl, val
#ccall fluid_synth_cc , Ptr <fluid_synth_t> -> CInt -> CInt -> CInt -> IO CInt
-- chan, ctrl, val
#ccall fluid_synth_get_cc , Ptr <fluid_synth_t> -> CInt -> CInt -> Ptr CInt -> IO CInt
-- sysex data, length of data, response, response length(in/out), handled?(out), dryrun?
#ccall fluid_synth_sysex , Ptr <fluid_synth_t> -> CString -> CInt -> CString -> Ptr CInt -> Ptr CInt -> CInt -> IO CInt
-- chan, val
#ccall fluid_synth_pitch_bend , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt
-- chan, val
#ccall fluid_synth_get_pitch_bend , Ptr <fluid_synth_t> -> CInt -> Ptr CInt -> IO CInt
-- chan, val
#ccall fluid_synth_pitch_whell_sens , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt
-- chan, val
#ccall fluid_synth_get_pitch_wheel_sens , Ptr <fluid_synth_t> -> CInt -> Ptr CInt -> IO CInt
-- chan, program
#ccall fluid_synth_program_change , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt
-- chan, val
#ccall fluid_synth_channel_pressure , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt
-- chan, bank
#ccall fluid_synth_bank_select , Ptr <fluid_synth_t> -> CInt -> CUInt -> IO CInt
-- chan, sfont_id
#ccall fluid_synth_sfont_select , Ptr <fluid_synth_t> -> CInt -> CUInt -> IO CInt
-- chan, sfont_id, bank_num, preset_num
#ccall fluid_synth_program_select , Ptr <fluid_synth_t> -> CInt -> CUInt -> CUInt -> CUInt -> IO CInt
-- chan, sfont_name, bank_num, preset_num
#ccall fluid_synth_program_select_by_sfont_name , Ptr <fluid_synth_t> -> CInt -> CString -> CUInt -> CUInt -> IO CInt
-- chan, sfont_id, bank_num, preset_num
#ccall fluid_synth_get_program , Ptr <fluid_synth_t> -> CInt -> Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> IO CInt
-- chan
#ccall fluid_synth_unset_program , Ptr <fluid_synth_t> -> CInt -> IO CInt
-- chan, info
#ccall fluid_synth_get_channel_info , Ptr <fluid_synth_t> -> CInt -> Ptr <fluid_synth_channel_info_t> -> IO CInt

#ccall fluid_synth_program_reset , Ptr <fluid_synth_t> -> IO CInt
#ccall fluid_synth_system_reset , Ptr <fluid_synth_t> -> IO CInt
#ccall fluid_synth_all_notes_off , Ptr <fluid_synth_t> -> CInt -> IO CInt
#ccall fluid_synth_all_sounds_off , Ptr <fluid_synth_t> -> CInt -> IO CInt

#integral_t enum fluid_midi_channel_type
#num CHANNEL_TYPE_MELODIC
#num CHANNEL_TYPE_DRUM

#ccall fluid_synth_set_channel_type , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt

-- Low level access
#ccall fluid_synth_get_channel_preset , Ptr <fluid_synth_t> -> CInt -> IO (Ptr <fluid_preset_t>)
#ccall fluid_synth_start , Ptr <fluid_synth_t> -> CUInt -> Ptr <fluid_preset_t> -> CInt -> CInt -> CInt -> CInt -> IO CInt
#ccall fluid_synth_stop , Ptr <fluid_synth_t> -> CUInt -> IO CInt

-- SoundFont management
#ccall fluid_synth_sfload , Ptr <fluid_synth_t> -> CString -> CInt -> IO CInt
#ccall fluid_synth_sfreload , Ptr <fluid_synth_t> -> CString -> CUInt -> IO CInt
#ccall fluid_synth_sfunload , Ptr <fluid_synth_t> -> CString -> CUInt -> CInt -> IO CInt
#ccall fluid_synth_add_sfont , Ptr <fluid_synth_t> -> Ptr <fluid_sfont_t> -> IO CInt
#ccall fluid_synth_remove_sfont , Ptr <fluid_synth_t> -> Ptr <fluid_sfont_t> -> IO ()
#ccall fluid_synth_sfcount , Ptr <fluid_synth_t> -> IO CInt
#ccall fluid_synth_get_sfont , Ptr <fluid_synth_t> -> CUInt -> IO (Ptr <fluid_sfont_t>)
#ccall fluid_synth_get_sfont_by_id , Ptr <fluid_synth_t> -> CUInt -> IO (Ptr <fluid_sfont_t>)
#ccall fluid_synth_get_sfont_by_name , Ptr <fluid_synth_t> -> CString -> IO (Ptr <fluid_sfont_t>)
#ccall fluid_synth_set_bank_offset , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt
#ccall fluid_synth_get_bank_offset , Ptr <fluid_synth_t> -> CInt -> IO CInt

-- Reverb

#ccall fluid_synth_set_reverb , Ptr <fluid_synth_t> -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
#ccall fluid_synth_set_reverb_on , Ptr <fluid_synth_t> -> CInt -> IO ()
#ccall fluid_synth_get_reverb_roomsize , Ptr <fluid_synth_t> -> IO Double
#ccall fluid_synth_get_reverb_damp , Ptr <fluid_synth_t> -> IO Double
#ccall fluid_synth_get_reverb_level , Ptr <fluid_synth_t> -> IO Double
#ccall fluid_synth_get_reverb_width , Ptr <fluid_synth_t> -> IO Double

#num FLUID_REVERB_DEFAULT_ROOMSIZE
#num FLUID_REVERB_DEFAULT_DAMP
#num FLUID_REVERB_DEFAULT_WIDTH
#num FLUID_REVERB_DEFAULT_LEVEL

-- Chorus

#integral_t enum fluid_chorus_mod
#num FLUID_CHORUS_MOD_SINE
#num FLUID_CHORUS_MOD_TRIANGLE

#ccall fluid_synth_set_chorus , Ptr <fluid_synth_t> -> CInt -> CDouble -> CDouble -> CDouble -> <fluid_chorus_mod> -> IO ()
#ccall fluid_synth_set_chorus_on , Ptr <fluid_synth_t> -> CInt -> IO ()
#ccall fluid_synth_get_chorus_nr , Ptr <fluid_synth_t> -> IO CInt
#ccall fluid_synth_get_chorus_level , Ptr <fluid_synth_t> -> IO Double
#ccall fluid_synth_get_chorus_speed_Hz , Ptr <fluid_synth_t> -> IO Double
#ccall fluid_synth_get_chorus_depth_ms , Ptr <fluid_synth_t> -> IO Double
#ccall fluid_synth_get_chorus_type , Ptr <fluid_synth_t> -> IO <fluid_chorus_mod>

#num FLUID_CHORUS_DEFAULT_N
#num FLUID_CHORUS_DEFAULT_LEVEL
#num FLUID_CHORUS_DEFAULT_SPEED
#num FLUID_CHORUS_DEFAULT_DEPTH
#num FLUID_CHORUS_DEFAULT_TYPE

-- Audio and MIDI channels

#ccall fluid_synth_count_midi_channels , Ptr <fluid_synth_t> -> IO CInt
#ccall fluid_synth_count_audio_channels , Ptr <fluid_synth_t> -> IO CInt
#ccall fluid_synth_count_audio_groups , Ptr <fluid_synth_t> -> IO CInt
#ccall fluid_synth_count_effects_channels , Ptr <fluid_synth_t> -> IO CInt

-- Synthesis parameters */

#ccall fluid_synth_set_sample_rate , Ptr <fluid_synth_t> -> CFloat -> IO ()
#ccall fluid_synth_set_gain , Ptr <fluid_synth_t> -> CFloat -> IO ()
#ccall fluid_synth_get_gain , Ptr <fluid_synth_t> -> IO CFloat
#ccall fluid_synth_set_polyphony , Ptr <fluid_synth_t> -> CInt -> IO CInt
#ccall fluid_synth_get_polyphony , Ptr <fluid_synth_t> -> IO CInt
#ccall fluid_synth_get_active_voice_count , Ptr <fluid_synth_t> -> IO CInt
#ccall fluid_synth_get_internal_bufsize , Ptr <fluid_synth_t> -> IO CInt
#ccall fluid_synth_set_interp_method , Ptr <fluid_synth_t> -> CInt -> <fluid_interp> -> IO CInt

#integral_t enum fluid_interp
#num FLUID_INTERP_NONE
#num FLUID_INTERP_LINEAR
#num FLUID_INTERP_4THORDER
#num FLUID_INTERP_7THORDER

#num FLUID_INTERP_DEFAULT
#num FLUID_INTERP_HIGHEST

-- Generator interface

#ccall fluid_synth_set_gen , Ptr <fluid_synth_t> -> CInt -> CInt -> CFloat -> IO CInt
#ccall fluid_synth_set_gen2 , Ptr <fluid_synth_t> -> CInt -> CInt -> CFloat -> CInt -> CInt -> IO CInt
#ccall fluid_synth_get_gen , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CFloat

-- Tuning

#ccall fluid_synth_create_key_tuning , Ptr <fluid_synth_t> -> CInt -> CInt -> CString -> Ptr CDouble -> IO CInt
#ccall fluid_synth_activate_key_tuning , Ptr <fluid_synth_t> -> CInt -> CInt -> CString -> Ptr CDouble -> CInt -> IO CInt
#ccall fluid_synth_create_octave_tuning , Ptr <fluid_synth_t> -> CInt -> CInt -> CString -> Ptr CDouble -> IO CInt
#ccall fluid_synth_activate_octave_tuning , Ptr <fluid_synth_t> -> CInt -> CInt -> CString -> Ptr CDouble -> CInt -> IO CInt
#ccall fluid_synth_tune_notes , Ptr <fluid_synth_t> -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CDouble -> CInt -> IO CInt
#ccall fluid_synth_select_tuning , Ptr <fluid_synth_t> -> CInt -> CInt -> CInt -> IO CInt
#ccall fluid_synth_activate_tuning , Ptr <fluid_synth_t> -> CInt -> CInt -> CInt -> CInt -> IO CInt
#ccall fluid_synth_reset_tuning , Ptr <fluid_synth_t> -> CInt -> IO CInt
#ccall fluid_synth_deactivate_tuning , Ptr <fluid_synth_t> -> CInt -> CInt -> IO CInt
#ccall fluid_synth_tuning_iteration_start , Ptr <fluid_synth_t> -> IO ()
#ccall fluid_synth_tuning_iteration_next , Ptr <fluid_synth_t> -> Ptr CInt -> Ptr CInt -> IO CInt
#ccall fluid_synth_tuning_dump , Ptr <fluid_synth_t> -> CInt -> CInt -> CString -> CInt -> Ptr CDouble -> IO CInt

-- Misc

#ccall fluid_synth_get_cpu_load , Ptr <fluid_synth_t> -> IO CDouble
#ccall fluid_synth_get_synth_error , Ptr <fluid_synth_t> -> IO CString

-- Synthesizer plugin

#ccall fluid_synth_write_s16 , Ptr <fluid_synth_t> -> CInt -> Ptr () -> CInt -> CInt -> Ptr () -> CInt -> CInt -> IO CInt
#ccall fluid_synth_write_float , Ptr <fluid_synth_t> -> CInt -> Ptr () -> CInt -> CInt -> Ptr () -> CInt -> CInt -> IO CInt
#ccall fluid_synth_nwrite_float , Ptr <fluid_synth_t> -> CInt -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> Ptr (Ptr CFloat) -> IO CInt
#ccall fluid_synth_process , Ptr <fluid_synth_t> -> CInt -> CInt -> Ptr (Ptr CFloat) -> CInt -> Ptr (Ptr CFloat) -> IO CInt

#callback_t fluid_audio_callback_t , Ptr <fluid_synth_t> -> CInt -> Ptr () -> CInt -> CInt -> Ptr () -> CInt -> CInt -> IO CInt

-- Synthesizer's interface to handle SoundFont loaders
#ccall fluid_synth_add_sfloader , Ptr <fluid_synth_t> -> Ptr <fluid_sfloader_t> -> IO ()
#ccall fluid_synth_alloc_voice , Ptr <fluid_synth_t> -> Ptr <fluid_sample_t> -> CInt -> CInt -> CInt -> IO (Ptr <fluid_voice_t>)
#ccall fluid_synth_start_voice , Ptr <fluid_synth_t> -> Ptr <fluid_voice_t> -> IO ()
#ccall fluid_synth_get_voicelist , Ptr <fluid_synth_t> -> Ptr (Ptr <fluid_voice_t>) -> CInt -> CInt -> IO ()
#ccall fluid_synth_handle_midi_event , Ptr <fluid_synth_t> -> Ptr <fluid_midi_event_t> -> IO CInt
#ccall fluid_synth_set_midi_router , Ptr <fluid_synth_t> -> Ptr <fluid_midi_router_t> -> IO ()
