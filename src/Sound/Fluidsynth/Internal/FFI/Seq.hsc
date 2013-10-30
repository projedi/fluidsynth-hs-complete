#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Seq where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#callback_t fluid_event_callback_t , CUInt -> Ptr <fluid_event_t> -> Ptr <fluid_sequencer_t> -> Ptr () -> IO ()

#ccall new_fluid_sequencer , IO (Ptr <fluid_sequencer_t>)
-- use system timer? otherwise call fluid_sequence_process to advance
#ccall new_fluid_sequencer2 , CInt -> IO (Ptr <fluid_sequencer_t>)
#ccall delete_fluid_sequencer , Ptr <fluid_sequencer_t> -> IO ()
#ccall fluid_sequencer_get_use_system_timer , Ptr <fluid_sequencer_t> -> IO CInt
-- name, callback, data; returns unique client ID
#ccall fluid_sequencer_register_client , Ptr <fluid_sequencer_t> -> CString -> <fluid_event_callback_t> -> Ptr a -> IO CShort
#ccall fluid_sequencer_unregister_client , Ptr <fluid_sequencer_t> -> CShort -> IO ()
#ccall fluid_sequencer_count_clients , Ptr <fluid_sequencer_t> -> IO CInt
-- index of registered client(order in which it was registered)
#ccall fluid_sequencer_get_client_id , Ptr <fluid_sequencer_t> -> CInt -> IO CShort
-- client id
#ccall fluid_sequencer_get_client_name , Ptr <fluid_sequencer_t> -> CInt -> IO CString
-- client id
#ccall fluid_sequencer_client_is_dest , Ptr <fluid_sequencer_t> -> CInt -> IO CInt
#ccall fluid_sequencer_process , Ptr <fluid_sequencer_t> -> CUInt -> IO ()
#ccall fluid_sequencer_send_now , Ptr <fluid_sequencer_t> -> Ptr <fluid_event_t> -> IO ()
-- time value in ticks, is absolute sequencer time?
#ccall fluid_sequencer_send_at , Ptr <fluid_sequencer_t> -> Ptr <fluid_event_t> -> CUInt -> CInt -> IO CInt
-- source id, dest id, type to match(-1 is wildcard)
#ccall fluid_sequencer_remove_events , Ptr <fluid_sequencer_t> -> CShort -> CShort -> CInt -> IO ()
#ccall fluid_sequencer_get_tick , Ptr <fluid_sequencer_t> -> IO CUInt
#ccall fluid_sequencer_set_time_scale , Ptr <fluid_sequencer_t> -> CDouble -> IO ()
#ccall fluid_sequencer_get_time_scale , Ptr <fluid_sequencer_t> -> IO CDouble
