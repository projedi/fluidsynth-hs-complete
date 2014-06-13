{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Sound.Fluidsynth.Synth
   (
   -- * MIDI channel messages
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
   , SoundFont(..)
   , addSoundFont
   , getBankOffset
   , getSoundFontByID
   , getSoundFontByIndex
   , getSoundFontByName
   , removeSoundFont
   , setBankOffset
   , soundFontCount
   , soundFontLoad
   , soundFontReload
   , soundFontUnload
   -- * Reverb
   , ReverbParams(..)
   , reverbRoomsize
   , reverbDamping
   , reverbWidth
   , reverbLevel
   , defaultReverbParams
   , getReverbParams
   , setReverbOn
   , setReverbParams
   -- * Chorus
   , ChorusParams(..)
   , ChorusType(..)
   , chorusVoiceCount
   , chorusLevel
   , chorusSpeed
   , chorusDepth
   , chorusType
   , defaultChorusParams
   , getChorusParams
   , setChorusOn
   , setChorusParams
   -- * Audio and MIDI channels
   , audioChannelCount
   , audioGroupCount
   , effectChannelCount
   , midiChannelCount
   -- * Synthesis parameters
   , Gain
   , InterpolationMethod(..)
   , MaybeChannel(..)
   , Polyphony
   , SampleRate
   , defaultInterpolationMethod
   , getActiveVoiceCount
   , getGain
   , getInternalBufferSize
   , getPolyphony
   , highestInterpolationMethod
   , setGain
   , setInterpolationMethod
   , setPolyphony
   , setSampleRate
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

----- MIDI Channel messages -----

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

----- Low level accesss -----

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
      -> SynthThread Bool
start = undefined

stop :: VoiceGroupID -> FluidSynth Bool
stop = undefined

----- SoundFont management -----

soundFontLoad :: String -- ^ filename
              -> Bool -- ^ reset presets for all channels?
              -> FluidSynth (Maybe Event.SoundFontID)
soundFontLoad = undefined

-- TODO: Event.SoundFontID must be an abstract newtype

-- actually returns id but since it doesn't change let's not do that
soundFontReload :: Event.SoundFontID -> FluidSynth Bool
soundFontReload = undefined

soundFontUnload :: Event.SoundFontID
                -> Bool -- ^ reset presets for all channels?
                -> FluidSynth Bool
soundFontUnload = undefined

newtype SoundFont = SoundFont C'fluid_sfont_t

addSoundFont :: SoundFont -> FluidSynth (Maybe Event.SoundFontID)
addSoundFont = undefined

-- | Remove SoundFont from stack but without deleting it
removeSoundFont :: SoundFont -> FluidSynth ()
removeSoundFont = undefined

-- | Number of loaded SoundFonts
soundFontCount :: FluidSynth Int
soundFontCount = undefined

getSoundFontByIndex :: Int -> FluidSynth (Maybe SoundFont)
getSoundFontByIndex = undefined

getSoundFontByID :: Event.SoundFontID -> FluidSynth (Maybe SoundFont)
getSoundFontByID = undefined

getSoundFontByName :: String -> FluidSynth (Maybe SoundFont)
getSoundFontByName = undefined

getBankOffset :: Event.SoundFontID -> FluidSynth Int
getBankOffset = undefined

setBankOffset :: Event.SoundFontID -> Int -> FluidSynth Bool
setBankOffset = undefined

----- Reverb -----

data ReverbParams = ReverbParams
   { _reverbRoomsize :: Double -- ^ @ 0.0 - 1.2 @
   , _reverbDamping :: Double -- ^ @ 0.0 - 1.0 @
   , _reverbWidth :: Double -- ^ @ 0.0 - 100.0 @
   , _reverbLevel :: Double -- ^ @ 0.0 - 1.0 @
   }

makeLenses ''ReverbParams

-- | Not realtime-safe, should not be called on synth thread
setReverbParams :: ReverbParams -> FluidSynth ()
setReverbParams = undefined

getReverbParams :: FluidSynth ReverbParams
getReverbParams = undefined

setReverbOn :: Bool -> FluidSynth ()
setReverbOn = undefined

defaultReverbParams :: ReverbParams
defaultReverbParams = undefined

----- Chorus -----

data ChorusType = ChorusTypeSine | ChorusTypeTriangle

data ChorusParams = ChorusParams
   { _chorusVoiceCount :: Int -- ^ @ 0 - 99 @
   , _chorusLevel :: Double -- ^ @ 0 - 10 @
   , _chorusSpeed :: Double -- ^ @ 0.29 - 5.0; @ in Hz
   , _chorusDepth :: Double -- ^ Depends on sample rate; @ 0.0 - 21.0 @ is safe up to 96 kHz; in ms
   , _chorusType :: ChorusType
   }

makeLenses ''ChorusParams

setChorusParams :: ChorusParams -> FluidSynth ()
setChorusParams = undefined

getChorusParams :: FluidSynth ChorusParams
getChorusParams = undefined

setChorusOn :: Bool -> FluidSynth ()
setChorusOn = undefined

defaultChorusParams :: ChorusParams
defaultChorusParams = undefined

----- Audio and MIDI channels -----

midiChannelCount :: FluidSynth Int
midiChannelCount = undefined

-- | Original returns value 2 times less
audioChannelCount :: FluidSynth Int
audioChannelCount = undefined

-- | Original returns value 2 times less
audioGroupCount :: FluidSynth Int
audioGroupCount = undefined

effectChannelCount :: FluidSynth Int
effectChannelCount = undefined

----- Synthesis parameters -----

type SampleRate = Float

-- 0.0 - 10.0
type Gain = Float

type Polyphony = Int

data InterpolationMethod =
   InterpolationNone | InterpolationLinear | Interpolation4thOrder | Interpolation7thOrder

data MaybeChannel = AnyChannel | Channel Event.Channel

-- | Experimental; do before any voice and notes are active and before rendering calls
setSampleRate :: SampleRate -> FluidSynth ()
setSampleRate = undefined

setGain :: Gain -> FluidSynth ()
setGain = undefined

getGain :: FluidSynth Gain
getGain = undefined

setPolyphony :: Polyphony -> FluidSynth ()
setPolyphony = undefined

getPolyphony :: FluidSynth Polyphony
getPolyphony = undefined

-- | For accuracy should be called synchronously with the audio synthesis process (e.g. in audio callback)
getActiveVoiceCount :: FluidSynth Int
getActiveVoiceCount = undefined

getInternalBufferSize :: FluidSynth Int
getInternalBufferSize = undefined

setInterpolationMethod :: MaybeChannel -> InterpolationMethod -> FluidSynth Bool
setInterpolationMethod = undefined

defaultInterpolationMethod :: InterpolationMethod
defaultInterpolationMethod = undefined

highestInterpolationMethod :: InterpolationMethod
highestInterpolationMethod = undefined

-- TODO: This must be completely moved to Gen module
{-
----- Generator interface -----

-- These two should be in Gen module and the first one is enum
type GenType = Int
type GenValue = Float


set_gen :: Event.Channel -> GenType -> GenValue -> FluidSynth Bool
set_gen2 :: Event.Channel -> GenType -> GenValue -> Bool -> Bool -> FluidSynth Bool
get_gen :: Event.Channel -> GenType -> FluidSynth GenValue
-}

{-
----- Tuning -----

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

----- Misc -----

-- in percentage
get_cpu_load :: FluidSynth Double
get_synth_error :: FluidSynth String

----- Synthesizer plugin -----

-- all of them only on synth thread
write_s16 :: [Word16] -> Int -> Int -> [Word16] -> Int -> Int -> FluidSynth Bool
write_float :: [Float] -> Int -> Int -> [Float] -> Int -> Int -> FluidSynth Bool
nwrite_float :: [(Float, Float)] -> FluidSynth Bool
process :: AudioCallback

----- Synthesizer's interface to handle SoundFont loaders -----

add_sfloader :: SoundFontLoader -> FluidSynth ()
-- next 3 only on synth thread
alloc_voice :: Sample -> Event.Channel -> Event.Key -> Event.Velocity -> FluidSynth (Maybe Voice)
start_voice :: Voice -> FluidSynth ()
get_voicelist :: VoiceID -> FluidSynth [Voice]

handle_midi_event :: HandleMidiEvent

-}
