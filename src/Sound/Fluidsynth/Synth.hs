{-# LANGUAGE TemplateHaskell #-}
module Sound.Fluidsynth.Synth
   ( -- * MIDI channel messages
     ChannelInfo(..)
   , channelInfoSoundFontID
   , channelInfoBankNumber
   , channelInfoProgramNumber
   , channelInfoName
   , ChannelType(..)
   , SYSEX
   , SynthEventType(..)
   , getChannelInfo
   , getControlValue
   , getPitchBend
   , getPitchWheelSensitivity
   , getProgram
   , sendSYSEX
   , sendSynthEvent
   -- * Low level access
   , AudioChannel
   , Preset(..)
   , VoiceGroupID
   , start
   , stop
   -- * SoundFont management
   -- * Reverb
   -- * Chorus
   -- * Audio and MIDI channels
   -- * Synthesis parameters
   -- * Generator interface
   -- * Tuning
   -- * Misc
   -- * Synthesizer plugin
   -- * Synthesizer's interface to handle SoundFont loaders
   ) where

import Control.Lens

import Sound.Fluidsynth.Internal.FFI.Synth
import Sound.Fluidsynth.Internal.FFI.Types
import Sound.Fluidsynth.Internal.Types
import qualified Sound.Fluidsynth.Internal.Event as Event

-- TODO: Implement _assigned :: Bool
data ChannelInfo = ChannelInfo
   { _channelInfoSoundFontID :: Event.SoundFontID
   , _channelInfoBankNumber :: Event.BankNum
   , _channelInfoProgramNumber :: Event.PresetNum
   , _channelInfoName :: String
   }

makeLenses ''ChannelInfo

data ChannelType = ChannelTypeMelodic | ChannelTypeDrum

data SynthEventType
   = SynthNoteOn Event.Channel Event.Key Event.Velocity
   | SynthNoteOff Event.Channel Event.Key
   | SynthControlChange Event.Channel Event.Control Event.Value
   | SynthPitchBend Event.Channel Event.Pitch
   | SynthPitchWheelSensitivity Event.Channel Event.Value
   | SynthProgramChange Event.Channel Event.PresetNum
   | SynthChannelPressure Event.Channel Event.Value
   | SynthBankSelect Event.Channel Event.BankNum
   | SynthSoundFontSelect Event.Channel Event.SoundFontID
   | SynthProgramSelect Event.Channel Event.SoundFontID Event.BankNum Event.PresetNum
   | SynthProgramSelectBySoundFontName Event.Channel String Event.BankNum Event.PresetNum
   | SynthUnsetProgram Event.Channel
   | SynthProgramReset
   | SynthSystemReset
   | SynthAllNotesOff Event.Channel
   | SynthAllSoundsOff Event.Channel
   | SynthSetChannelType Event.Channel ChannelType

sendSynthEvent :: SynthEventType -> FluidSynth Bool
sendSynthEvent = undefined

getControlValue :: Event.Channel -> Event.Control -> FluidSynth (Maybe Event.Value)
getControlValue = undefined

type SYSEX = String

-- TODO: should try different response_len while fluid_synth_sysex fails with FLUID_FAILED
sendSYSEX :: SYSEX
          -> Bool -- ^ Do a dry run? Helpfull for checking if SYSEX would be handled.
          -> FluidSynth (Maybe String) -- ^ Nothing if SYSEX is not handled, Just response otherwise
sendSYSEX = undefined

getPitchBend :: Event.Channel -> FluidSynth (Maybe Event.Pitch)
getPitchBend = undefined

getPitchWheelSensitivity :: Event.Channel -> FluidSynth (Maybe Event.Value)
getPitchWheelSensitivity = undefined

getProgram :: Event.Channel -> FluidSynth (Maybe (Event.SoundFontID, Event.BankNum, Event.PresetNum))
getProgram = undefined

getChannelInfo :: Event.Channel -> FluidSynth (Maybe ChannelInfo)
getChannelInfo = undefined

type VoiceGroupID = Int

newtype Preset = Preset C'fluid_preset_t

type AudioChannel = Int

-- | Must be called on synth thread (includes SoundFont noteon method)
start :: VoiceGroupID
      -> Preset
      -> AudioChannel -- ^ unused as of 1.1.6
      -> Event.Channel
      -> Event.Key
      -> Event.Velocity
      -> FluidSynth Bool
start = undefined

stop :: VoiceGroupID -> FluidSynth Bool
stop = undefined

{-
sfload :: String -> Bool -> FluidSynth (Maybe Event.SoundFontID)
sfreload :: Event.SoundFontID -> FluidSynth (Maybe Event.SoundFontID)
sfunload :: Event.SoundFontID -> Bool -> FluidSynth Bool
add_sfont :: SoundFont -> FluidSynth (Maybe Event.SoundFontID)
remove_sfont :: SoundFont -> FluidSynth ()
sfcount :: FluidSynth Int
get_sfont :: Int -> FluidSynth (Maybe SoundFont)
get_sfont_by_id :: Event.SoundFontID -> FluidSynth (Maybe SoundFont)
get_sfont_by_name :: String -> FluidSynth (Maybe SoundFont)
get_bank_offset :: Event.SoundFontID -> FluidSynth Int
set_bank_offset :: Event.SoundFontID -> Int -> FluidSynth Bool

-- 0.0 - 1.2
type ReverbRoomsize = Double
-- 0.0 - 1.0
type ReverbDamping = Double
-- 0.0 - 100.0
type ReverbWidth = Double
-- 0.0 - 1.0
type ReverbLevel = Double

set_reverb :: ReverbRoomsize -> ReverbDamping -> ReverbWidth -> RevertLevel -> FluidSynth ()
set_reverb_on :: Bool -> FluidSynth ()
get_reverb_roomsize :: FluidSynth ReverbRoomsize
get_reverb_damp :: FluidSynth ReverbDamp
get_reverb_level :: FluidSynth ReverbLevel
get_reverb_width :: FluidSynth ReverbWidth

default_reverb_roomsize :: ReverbRoomsize
default_reverb_damp :: ReverbDamp
default_reverb_width :: ReverbWidth
default_reverb_level :: ReverbLevel

data ChorusMod = ChorusModSine | ChorusModTriangle

-- 0 - 99
type ChorusVoiceCount = Int
-- 0.0 - 10.0
type ChorusLevel = Double
-- 0.29 - 5.0; in Hz
type ChorusSpeed = Double
-- depends on synth sample rate; 0.0 - 21.0 is safe for sample rate up to 96 KHz; in ms
type ChorusDepth = Double

set_chorus :: ChorusVoiceCount -> ChorusLevel -> ChorusSpeed -> ChorusDepth -> ChorusMod -> FluidSynth ()
set_chorus_on :: Bool -> FluidSynth ()
get_chorus_nr :: FluidSynth ChorusVoiceCount
get_chorus_level :: FluidSynth ChorusLevel
get_chorus_speed_Hz :: FluidSynth ChorusSpeed
get_chorus_depth_ms :: FluidSynth ChorusDepth
get_chorus_type :: FluidSynth ChorusMod

default_chorus_nr :: ChorusVoiceCount
default_chorus_level :: ChorusLevel
default_chorus_speed :: ChorusSpeed
default_chorus_depth :: ChorusDepth
default_chorus_type :: ChorusMod

count_midi_channels :: FluidSynth Int
-- original: 1 = 2, 2 = 4; return a (*2)
count_audio_channels :: FluidSynth Int
-- original: 1 = 2, 2 = 4; return a (*2)
count_audio_groups :: FluidSynth Int
count_effect_channels :: FluidSynth Int

type SampleRate = Float
-- 0.0 - 10.0
type Gain = Float
type Polyphony = Int
data InterpolationMethod =
   InterpolationNone | InterpolationLinear | Interpolation4thOrder | Interpolation7thOrder

-- experimental; do before any voice and notes are active and before rendering calls
set_sample_rate :: SampleRate -> FluidSynth ()
set_gain :: Gain -> FluidSynth ()
get_gain :: FluidSynth Gain
set_polyphony :: Polyphony -> FluidSynth ()
get_polyphony :: FluidSynth Polyphony
get_active_voice_count :: FluidSynth Int
get_internal_bufsize :: FluidSynth Int
-- TODO: Ok, this Maybe Event.Channel must go for some custom "AnyChannel | Channel Channel"
-- Nothing for all channels
set_interp_method :: Maybe Event.Channel -> InterpolationMethod -> FluidSynth Bool

interpolation_default :: InterpolationMethod
interpolation_highest :: InterapolationMethod

-- These two should be in Gen module and the first one is enum
type GenType = Int
type GenValue = Float


set_gen :: Event.Channel -> GenType -> GenValue -> FluidSynth Bool
set_gen2 :: Event.Channel -> GenType -> GenValue -> Bool -> Bool -> FluidSynth Bool
get_gen :: Event.Channel -> GenType -> FluidSynth GenValue

-- 0 - 127
type TuningBank = Int
-- 0 - 127
type TuningPreset = Int
type Cent = Float
-- length of 128; normally 0 = 0, 1 = 100, 60 = 6000
type CentArrayPitch = [Cent]
-- length of 12; starting from C
type CentArrayOctave = [Cent]
type KeyCentArray = [(Event.Key, Cent)]

-- empty CentArray(NULL) is for a well-tempered scale
create_key_tuning :: TuningBank -> TuningPreset -> String -> CentArrayPitch -> FluidSynth Bool
activate_key_tuning :: TuningBank -> TuningPreset -> String -> CentArrayPitch -> Bool -> FluidSynth Bool
create_octave_tuning :: TuningBank -> TuningPreset -> String -> CentArrayOctave -> FluidSynth Bool
activate_octave_tuning :: TuningBank -> TuningPreset -> String -> CentArrayOctave -> Bool -> FluidSynth Bool
tune_notes :: TuningBank -> TuningPreset -> KeyCentArray -> Bool -> FluidSynth Bool
select_tuning :: Event.Channel -> TuningBank -> TuningPreset -> FluidSynth Bool
activate_tuning :: Event.Channel -> TuningBank -> TuningPreset -> Bool -> FluidSynth Bool
reset_tuning :: Event.Channel -> FluidSynth Bool
deactivate_tuning :: Event.Channel -> Bool -> FluidSynth Bool
tuning_iteration_start :: FluidSynth ()
tuning_iteration_next :: FluidSynth (Maybe (TuningBank, TuningPreset))
tuning_dump :: TuningBank -> TuningPreset -> FluidSynth (Maybe (String, CentArrayPitch))

-- in percentage
get_cpu_load :: FluidSynth Double
get_synth_error :: FluidSynth String

-- all of them only on synth thread
write_s16 :: [Word16] -> Int -> Int -> [Word16] -> Int -> Int -> FluidSynth Bool
write_float :: [Float] -> Int -> Int -> [Float] -> Int -> Int -> FluidSynth Bool
nwrite_float :: [(Float, Float)] -> FluidSynth Bool
process :: AudioCallback

add_sfloader :: SoundFontLoader -> FluidSynth ()
-- next 3 only on synth thread
alloc_voice :: Sample -> Event.Channel -> Event.Key -> Event.Velocity -> FluidSynth (Maybe Voice)
start_voice :: Voice -> FluidSynth ()
get_voicelist :: VoiceID -> FluidSynth [Voice]

handle_midi_event :: HandleMidiEvent

-}
