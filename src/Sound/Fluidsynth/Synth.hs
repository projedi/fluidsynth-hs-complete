module Sound.Fluidsynth.Synth
   (
   ) where

import Sound.Fluidsynth.Internal.FFI.Synth
import Sound.Fluidsynth.Internal.FFI.Types
import qualified Sound.Fluidsynth.Internal.Event as Event

-- TODO: Implement _assigned :: Bool
data ChannelInfo = ChannelInfo
   { _soundFontID :: Event.SoundfontID
   , _bankNumber :: Event.BankNum
   , _programNumber :: Event.PresetNum
   , _name :: String
   }

-- Corresponding to Event.SeqEventType:
-- note
noteon :: Event.Channel -> Event.Key -> Event.Velocity -> FluidSynth Bool
noteoff :: Event.Channel -> Event.Key -> FluidSynth Bool
all_sounds_off :: Event.Channel -> FluidSynth Bool
all_notes_off :: Event.Channel -> FluidSynth Bool
bank_select :: Event.Channel -> Event.BankNum -> FluidSynth Bool
program_change :: Event.Channel -> Event.PresetNum -> FluidSynth Bool
program_select :: Event.Channel -> Event.SounfontID -> Event.BankNum -> Event.PresetNum -> FluidSynth Bool
pitch_bend :: Event.Channel -> Event.Pitch -> FluidSynth Bool
pitch_wheel_sens :: Event.Channel -> Event.Value ->  FluidSynth Bool
-- modulation
-- sustain
cc :: Event.Channel -> Event.Control -> Event.Value -> FluidSynth Bool
-- pan
-- volume
-- reverbsend
-- chorussend
-- timer
-- anycontrolchange
channel_pressure :: Event.Channel -> Event.Value -> FluidSynth Bool
system_reset :: FluidSynth Bool

get_cc :: Event.Channel -> Event.Control -> FluidSynth (Maybe Bool)
sysex :: String -> Bool -> FluidSynth (Maybe (String, Bool))
get_pitch_bend :: Event.Channel -> FluidSynth (Maybe Event.Pitch)
get_pitch_wheel_sens :: Event.Channel -> FluidSynth (Maybe Event.Value)
sfont_select :: Event.Channel -> Event.SoundfontID -> FluidSynth Bool
program_select_by_sfont_name :: Event.Channel -> String -> Event.BankNum -> Event.PresetNum -> FluidSynth Bool
get_program :: Event.Channel -> FluidSynth (Maybe (Event.SoundfontID, Event.BankNum, Event.PresetNum)
unset_program :: Event.Channel -> FluidSynth Bool
get_channel_info :: Event.Channel -> FluidSynth (Maybe ChannelInfo)
program_reset :: FluidSynth Bool

data ChannelType = ChannelTypeMelodic | ChannelTypeDrum

set_channel_type :: Event.Channel -> ChannelType -> FluidSynth Bool

-- Also after Preset there is an unused argument(set it to 0)
start :: VoiceGroupID -> Preset -> Event.Channel -> Event.Key -> Event.Velocity -> FluidSynth Bool
stop :: VoiceGroupID -> FluidSynth Bool

sfload :: String -> Bool -> FluidSynth (Maybe Event.SoundfontID)
sfreload :: Event.SoundfontID -> FluidSynth (Maybe Event.SoundfontID)
sfunload :: Event.SoundfontID -> Bool -> FluidSynth Bool
add_sfont :: Soundfont -> FluidSynth (Maybe Event.SoundfontID)
remove_sfont :: Soundfont -> FluidSynth ()
sfcount :: FluidSynth Int
get_sfont :: Int -> FluidSynth (Maybe Soundfont)
get_sfont_by_id :: Event.SoundfontID -> FluidSynth (Maybe Soundfont)
get_sfont_by_name :: String -> FluidSynth (Maybe Soundfont)
get_bank_offset :: Event.SoundfontID -> FluidSynth Int
set_bank_offset :: Event.SoundfontID -> Int -> FluidSynth Bool

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
-- Nothing for all channels
set_interp_method :: Maybe Event.Channel -> InterpolationMethod -> FluidSynth Bool

interpolation_default :: InterpolationMethod
interpolation_highest :: InterapolationMethod

-- These two should be in Gen module and the first one is enum
type GenType = Int
type GenValue = Float

-- TODO: Ok, this Maybe Event.Channel must go for some custom "AnyChannel | Channel Channel"

set_gen :: Event.Channel -> GenType -> GenValue -> FluidSynth Bool
set_gen2 :: Event.Channel -> GenType -> GenValue -> Bool -> Bool -> FluidSynth Bool
get_gen :: Event.Channel -> GenType -> FluidSynth GenValue
