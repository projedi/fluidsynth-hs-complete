#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Seqbind where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#ccall fluid_sequencer_register_fluidsynth , Ptr <fluid_sequencer_t> -> Ptr <fluid_synth_t> -> IO CShort
#ccall fluid_sequencer_add_midi_event_to_buffer , Ptr <fluid_sequencer_t> -> Ptr <fluid_midi_event_t> -> IO CInt
