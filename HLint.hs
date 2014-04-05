import "hint" HLint.HLint

-- ignore all the FFI modules because it has a lot of auto code generation
ignore = Sound.Fluidsynth.Internal.FFI.Audio
ignore = Sound.Fluidsynth.Internal.FFI.Event
ignore = Sound.Fluidsynth.Internal.FFI.Gen
ignore = Sound.Fluidsynth.Internal.FFI.Log
ignore = Sound.Fluidsynth.Internal.FFI.Midi
ignore = Sound.Fluidsynth.Internal.FFI.Misc
ignore = Sound.Fluidsynth.Internal.FFI.Mod
ignore = Sound.Fluidsynth.Internal.FFI.Ramsfont
ignore = Sound.Fluidsynth.Internal.FFI.Seq
ignore = Sound.Fluidsynth.Internal.FFI.Seqbind
ignore = Sound.Fluidsynth.Internal.FFI.Settings
ignore = Sound.Fluidsynth.Internal.FFI.Sfont
ignore = Sound.Fluidsynth.Internal.FFI.Shell
ignore = Sound.Fluidsynth.Internal.FFI.Synth
ignore = Sound.Fluidsynth.Internal.FFI.Types
ignore = Sound.Fluidsynth.Internal.FFI.Version
ignore = Sound.Fluidsynth.Internal.FFI.Voice

-- haddock breaks with this shortcut
ignore "Use import/export shortcut" = Sound.Fluidsynth
