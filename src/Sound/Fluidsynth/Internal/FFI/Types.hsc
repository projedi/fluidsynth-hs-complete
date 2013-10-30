#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Types where
#strict_import

#opaque_t fluid_settings_t
#opaque_t fluid_synth_t
#opaque_t fluid_synth_channel_info_t
#opaque_t fluid_voice_t
#opaque_t fluid_sfloader_t
#opaque_t fluid_sfont_t
#opaque_t fluid_preset_t
#opaque_t fluid_sample_t
#opaque_t fluid_mod_t
#opaque_t fluid_audio_driver_t
#opaque_t fluid_file_renderer_t
#opaque_t fluid_player_t
#opaque_t fluid_midi_event_t
#opaque_t fluid_midi_driver_t
#opaque_t fluid_midi_router_t
#opaque_t fluid_midi_router_rule_t
#opaque_t fluid_cmd_handler_t
#opaque_t fluid_shell_t
#opaque_t fluid_server_t
#opaque_t fluid_event_t
#opaque_t fluid_sequencer_t
#opaque_t fluid_ramsfont_t
#opaque_t fluid_rampreset_t

#synonym_t fluid_istream_t , CInt
#synonym_t fluid_ostream_t , CInt
