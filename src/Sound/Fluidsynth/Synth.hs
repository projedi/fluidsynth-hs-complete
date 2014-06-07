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
