{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Midi where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#ccall new_fluid_midi_event , IO (Ptr <fluid_midi_event_t>)
-- Returned CInt originally.
#ccall delete_fluid_midi_event , Ptr <fluid_midi_event_t> -> IO ()

#ccall fluid_midi_event_set_type , Ptr <fluid_midi_event_t> -> CInt -> IO CInt
#ccall fluid_midi_event_get_type , Ptr <fluid_midi_event_t> -> IO CInt
#ccall fluid_midi_event_set_channel , Ptr <fluid_midi_event_t> -> CInt -> IO CInt
#ccall fluid_midi_event_get_channel , Ptr <fluid_midi_event_t> -> IO CInt
#ccall fluid_midi_event_get_key , Ptr <fluid_midi_event_t> -> IO CInt
#ccall fluid_midi_event_set_key , Ptr <fluid_midi_event_t> -> CInt -> IO CInt
#ccall fluid_midi_event_get_velocity , Ptr <fluid_midi_event_t> -> IO CInt
#ccall fluid_midi_event_set_velocity , Ptr <fluid_midi_event_t> -> CInt -> IO CInt
#ccall fluid_midi_event_get_control , Ptr <fluid_midi_event_t> -> IO CInt
#ccall fluid_midi_event_set_control , Ptr <fluid_midi_event_t> -> CInt -> IO CInt
#ccall fluid_midi_event_get_value , Ptr <fluid_midi_event_t> -> IO CInt
#ccall fluid_midi_event_set_value , Ptr <fluid_midi_event_t> -> CInt -> IO CInt
#ccall fluid_midi_event_get_program , Ptr <fluid_midi_event_t> -> IO CInt
#ccall fluid_midi_event_set_program , Ptr <fluid_midi_event_t> -> CInt -> IO CInt
#ccall fluid_midi_event_get_pitch , Ptr <fluid_midi_event_t> -> IO CInt
#ccall fluid_midi_event_set_pitch , Ptr <fluid_midi_event_t> -> CInt -> IO CInt

-- pointer to SYSEX data, size of SYSEX data, true if the SYSEX data should be freed
#ccall fluid_midi_event_set_sysex , Ptr <fluid_midi_event_t> -> Ptr () -> CInt -> CInt -> IO CInt

#integral_t fluid_midi_router_rule_type
#num FLUID_MIDI_ROUTER_RULE_NOTE
#num FLUID_MIDI_ROUTER_RULE_CC
#num FLUID_MIDI_ROUTER_RULE_PROG_CHANGE
#num FLUID_MIDI_ROUTER_RULE_PITCH_BEND
#num FLUID_MIDI_ROUTER_RULE_CHANNEL_PRESSURE
#num FLUID_MIDI_ROUTER_RULE_KEY_PRESSURE
#num FLUID_MIDI_ROUTER_RULE_COUNT

#callback_t handle_midi_event_func_t , Ptr () -> Ptr <fluid_midi_event_t> -> IO CInt

#ccall new_fluid_midi_router , Ptr <fluid_settings_t> -> <handle_midi_event_func_t> -> Ptr a -> IO (Ptr <fluid_midi_router_t>)
-- Originally returned CInt
#ccall delete_fluid_midi_router , Ptr <fluid_midi_router_t> -> IO ()
#ccall fluid_midi_router_set_default_rules , Ptr <fluid_midi_router_t> -> IO CInt
#ccall fluid_midi_router_clear_rules , Ptr <fluid_midi_router_t> -> IO CInt
#ccall fluid_midi_router_add_rule , Ptr <fluid_midi_router_t> -> Ptr <fluid_midi_router_rule_t> -> <fluid_midi_router_rule_type> -> IO CInt

#ccall new_fluid_midi_router_rule , IO (Ptr <fluid_midi_router_rule_t>)
#ccall delete_fluid_midi_router_rule , Ptr <fluid_midi_router_rule_t> -> IO ()
-- min, max, mul, add
#ccall fluid_midi_router_rule_set_chan , Ptr <fluid_midi_router_rule_t> -> CInt -> CInt -> CFloat -> CInt -> IO ()
-- min, max, mul, add
#ccall fluid_midi_router_rule_set_param1 , Ptr <fluid_midi_router_rule_t> -> CInt -> CInt -> CFloat -> CInt -> IO ()
-- min, max, mul, add
#ccall fluid_midi_router_rule_set_param2 , Ptr <fluid_midi_router_rule_t> -> CInt -> CInt -> CFloat -> CInt -> IO ()

#ccall fluid_midi_router_handle_midi_event , Ptr <fluid_midi_router_t> -> Ptr <fluid_midi_event_t> -> IO CInt

#ccall fluid_midi_dump_prerouter , Ptr <fluid_midi_router_t> -> Ptr <fluid_midi_event_t> -> IO CInt
#ccall fluid_midi_dump_postrouter , Ptr <fluid_midi_router_t> -> Ptr <fluid_midi_event_t> -> IO CInt

#ccall new_fluid_midi_driver , Ptr <fluid_settings_t> -> <handle_midi_event_func_t> -> Ptr a -> IO (Ptr <fluid_midi_driver_t>)

#ccall delete_fluid_midi_driver , Ptr <fluid_midi_driver_t> -> IO ()

#integral_t enum fluid_player_status
#num FLUID_PLAYER_READY
#num FLUID_PLAYER_PLAYING
#num FLUID_PLAYER_DONE

#ccall new_fluid_player , Ptr <fluid_synth_t> -> IO (Ptr <fluid_player_t>)
-- Originally returned CInt
#ccall delete_fluid_player , Ptr <fluid_player_t> -> IO ()

-- filename
#ccall fluid_player_add , Ptr <fluid_player_t> -> CString -> IO CInt
-- buffer, buffer length
#ccall fluid_player_add_mem , Ptr <fluid_player_t> -> Ptr () -> CSize -> IO CInt
#ccall fluid_player_play , Ptr <fluid_player_t> -> IO CInt
#ccall fluid_player_stop , Ptr <fluid_player_t> -> IO CInt
#ccall fluid_player_join , Ptr <fluid_player_t> -> IO CInt
#ccall fluid_player_set_loop , Ptr <fluid_player_t> -> CInt -> IO CInt
#ccall fluid_player_set_midi_tempo , Ptr <fluid_player_t> -> CInt -> IO CInt
#ccall fluid_player_set_bpm , Ptr <fluid_player_t> -> CInt -> IO CInt
#ccall fluid_player_get_status , Ptr <fluid_player_t> -> IO CInt
#ccall fluid_player_set_playback_callback , Ptr <fluid_player_t> -> <handle_midi_event_func_t> -> Ptr a -> IO CInt
