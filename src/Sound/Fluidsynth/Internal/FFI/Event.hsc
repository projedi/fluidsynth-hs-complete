#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Event where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#integral_t enum fluid_seq_event_type
-- Note with duration
#num FLUID_SEQ_NOTE
#num FLUID_SEQ_NOTEON
#num FLUID_SEQ_NOTEOFF
#num FLUID_SEQ_ALLSOUNDSOFF
#num FLUID_SEQ_ALLNOTESOFF
#num FLUID_SEQ_BANKSELECT
#num FLUID_SEQ_PROGRAMCHANGE
#num FLUID_SEQ_PROGRAMSELECT
#num FLUID_SEQ_PITCHBEND
-- Pitch wheel sensitivity
#num FLUID_SEQ_PITCHWHEELSENS
#num FLUID_SEQ_MODULATION
#num FLUID_SEQ_SUSTAIN
#num FLUID_SEQ_CONTROLCHANGE
#num FLUID_SEQ_PAN
#num FLUID_SEQ_VOLUME
#num FLUID_SEQ_REVERBSEND
#num FLUID_SEQ_CHORUSSEND
#num FLUID_SEQ_TIMER
#num FLUID_SEQ_ANYCONTROLCHANGE
-- Channel aftertouch event
#num FLUID_SEQ_CHANNELPRESSURE
#num FLUID_SEQ_SYSTEMRESET
-- Called when a sequencer client is being unregistered
#num FLUID_SEQ_UNREGISTERING
#num FLUID_SEQ_LASTEVENT

-- Event alloc/free
#ccall new_fluid_event , IO (Ptr <fluid_event_t>)
#ccall delete_fluid_event , Ptr <fluid_event_t> -> IO ()

-- Initializing events
#ccall fluid_event_set_source , Ptr <fluid_event_t> -> CShort -> IO ()
#ccall fluid_event_set_dest , Ptr <fluid_event_t> -> CShort -> IO ()

-- Timer events
#ccall fluid_event_timer , Ptr <fluid_event_t> -> Ptr a -> IO ()

-- Note events
-- channel, key, vel, duration
#ccall fluid_event_note , Ptr <fluid_event_t> -> CInt -> CShort -> CShort -> CUInt -> IO ()
-- channel, key, vel
#ccall fluid_event_noteon , Ptr <fluid_event_t> -> CInt -> CShort -> CShort -> IO ()
-- channel, key
#ccall fluid_event_noteoff, Ptr <fluid_event_t> -> CInt -> CShort -> IO ()
-- channel
#ccall fluid_event_all_sounds_off , Ptr <fluid_event_t> -> CInt -> IO ()
-- channel
#ccall fluid_event_all_notes_off , Ptr <fluid_event_t> -> CInt -> IO ()

-- Instrument selection
-- channel, bank_num
#ccall fluid_event_bank_select , Ptr <fluid_event_t> -> CInt -> CShort -> IO ()
-- channel, preset_num
#ccall fluid_event_program_change , Ptr <fluid_event_t> -> CInt -> CShort -> IO ()
-- channel, sfont_id, bank_num, preset_num
#ccall fluid_event_program_select , Ptr <fluid_event_t> -> CInt -> CUInt -> CShort -> CShort -> IO ()

-- Real-time generic instrument controllers
-- channel, control, val
#ccall fluid_event_control_change , Ptr <fluid_event_t> -> CInt -> CShort -> CShort -> IO ()

-- Real-time instrument controllers shortcuts: channel, val
#ccall fluid_event_pitch_bend , Ptr <fluid_event_t> -> CInt -> CInt -> IO ()
#ccall fluid_event_pitch_wheelsens , Ptr <fluid_event_t> -> CInt -> CShort -> IO ()
#ccall fluid_event_modulation , Ptr <fluid_event_t> -> CInt -> CShort -> IO ()
#ccall fluid_event_sustain , Ptr <fluid_event_t> -> CInt -> CShort -> IO ()
#ccall fluid_event_pan , Ptr <fluid_event_t> -> CInt -> CShort -> IO ()
#ccall fluid_event_volume , Ptr <fluid_event_t> -> CInt -> CShort -> IO ()
#ccall fluid_event_reverb_send , Ptr <fluid_event_t> -> CInt -> CShort -> IO ()
#ccall fluid_event_chorus_send , Ptr <fluid_event_t> -> CInt -> CShort -> IO ()
#ccall fluid_event_channel_pressure , Ptr <fluid_event_t> -> CInt -> CShort -> IO ()

#ccall fluid_event_system_reset , Ptr <fluid_event_t> -> IO ()

-- Only for removing events
-- channel
#ccall fluid_event_any_control_change , Ptr <fluid_event_t> -> CInt -> IO ()

-- Only when unregistering clients
#ccall fluid_event_unregistering , Ptr <fluid_event_t> -> IO ()

-- Accessing event data
#ccall fluid_event_get_type , Ptr <fluid_event_t> -> IO CInt
#ccall fluid_event_get_source , Ptr <fluid_event_t> -> IO CShort
#ccall fluid_event_get_dest , Ptr <fluid_event_t> -> IO CShort
#ccall fluid_event_get_channel , Ptr <fluid_event_t> -> IO CInt
#ccall fluid_event_get_key , Ptr <fluid_event_t> -> IO CShort
#ccall fluid_event_get_velocity , Ptr <fluid_event_t> -> IO CShort
#ccall fluid_event_get_control , Ptr <fluid_event_t> -> IO CShort
#ccall fluid_event_get_value , Ptr <fluid_event_t> -> IO CShort
#ccall fluid_event_get_program , Ptr <fluid_event_t> -> IO CShort
#ccall fluid_event_get_data , Ptr <fluid_event_t> -> IO (Ptr a)
#ccall fluid_event_get_duration , Ptr <fluid_event_t> -> IO CUInt
#ccall fluid_event_get_bank , Ptr <fluid_event_t> -> IO CShort
#ccall fluid_event_get_pitch , Ptr <fluid_event_t> -> IO CInt
#ccall fluid_event_get_sfont_id , Ptr <fluid_event_t> -> IO CUInt
