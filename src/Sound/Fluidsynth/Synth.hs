{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}
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
   -- * Tuning
   , ActualVector
   , iterateVector
   , vectorAt
   , vectorToList
   , Cent
   , CentArrayOctave
   , CentArrayPitch
   , KeyCentArray
   , TuningBank
   , TuningPreset
   , activateKeyTuning
   , activateOctaveTuning
   , tuneNotes
   , activateTuning
   , deactivateTuning
   , tuningIterationStart
   , tuningIterationNext
   , tuningDump
   -- * Misc
   , getCpuLoad
   , lastErrorString
   -- * Synthesizer plugin
   , nWriteFloat
   , process
   -- * Synthesizer's interface to handle SoundFont loaders
   , SoundFontLoader
   , Sample
   , Voice
   , VoiceID
   , addSoundFontLoader
   , allocVoice
   , getVoiceList
   , handleMidiEvent
   , startVoice
   ) where

import Control.Lens

import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import Data.Proxy(Proxy(..))
import GHC.TypeLits

import Sound.Fluidsynth.Audio(AudioCallback)
import Sound.Fluidsynth.Midi(HandleMidiEvent)
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

----- Tuning -----

newtype ActualVector (n :: Nat) a = ActualVector [a]
   deriving Functor

iterateVector :: KnownNat n => (a -> a) -> a -> ActualVector n a
iterateVector f x = go f x Proxy
 where go :: KnownNat n => (a -> a) -> a -> Proxy n -> ActualVector n a
       go f x proxy = ActualVector $ take (fromInteger $ natVal proxy) $ iterate f x

vectorAt :: (KnownNat m, m <= (n + 1)) => ActualVector n a -> Proxy m -> a
vectorAt (ActualVector lst) proxy = lst !! (fromInteger $ natVal proxy)

vectorToList :: ActualVector n a -> [a]
vectorToList (ActualVector lst) = lst

-- 0 - 127
type TuningBank = Int

-- 0 - 127
type TuningPreset = Int

type Cent = Float

-- normally [0] = 0, [1] = 100, ..., [60] = 6000, ...
type CentArrayPitch = ActualVector 128 Cent

-- starting from C
type CentArrayOctave = ActualVector 12 Cent

-- | From 'Event.Key' to 'Cent'
type KeyCentArray = IntMap Cent

-- empty CentArray(NULL) is for a well-tempered scale
activateKeyTuning :: TuningBank
                  -> TuningPreset
                  -> String -- ^ name
                  -> CentArrayPitch
                  -> Bool -- ^ apply in realtime to existing notes TODO: explain
                  -> FluidSynth Bool
activateKeyTuning = undefined

activateOctaveTuning :: TuningBank
                     -> TuningPreset
                     -> String -- ^ name
                     -> CentArrayOctave
                     -> Bool -- ^ apply in realtime to existing notes TODO: explain
                     -> FluidSynth Bool
activateOctaveTuning = undefined

tuneNotes :: TuningBank
          -> TuningPreset
          -> KeyCentArray
          -> Bool -- ^ apply in realtime to existing notes TODO: explain
          -> FluidSynth Bool
tuneNotes = undefined

activateTuning :: Event.Channel
               -> TuningBank
               -> TuningPreset
               -> Bool -- ^ apply in realtime to existing notes TODO: explain
               -> FluidSynth Bool
activateTuning = undefined

deactivateTuning :: Event.Channel
                 -> Bool -- ^ apply in realtime to existing notes TODO: explain
                 -> FluidSynth Bool
deactivateTuning = undefined

-- TODO: What is the effect of this iteration on the synth?
tuningIterationStart :: FluidSynth()
tuningIterationStart = undefined

tuningIterationNext :: FluidSynth (Maybe (TuningBank, TuningPreset))
tuningIterationNext = undefined

tuningDump :: TuningBank -> TuningPreset -> FluidSynth (Maybe (String, CentArrayPitch))
tuningDump = undefined

----- Misc -----

-- | in percentage
getCpuLoad :: FluidSynth Double
getCpuLoad = undefined

-- | Get the last string in internal error buffer (put there via fluid_log or fluid_debug)
lastErrorString :: FluidSynth String
lastErrorString = undefined

----- Synthesizer plugin -----

-- TODO: Somehow implement write_s16, write_float; and their type, fluid_audio_callback_t

nWriteFloat :: Int
            -> SynthThread (Maybe ([Float], [Float])) -- ^ left, right
nWriteFloat = undefined

process :: AudioCallback
process = undefined

----- Synthesizer's interface to handle SoundFont loaders -----

-- TODO: Following two should be moved to SoundFont module

newtype SoundFontLoader = SoundFontLoader C'fluid_sfloader_t

-- | Should be called before any SoundFont file is loaded
addSoundFontLoader :: SoundFontLoader -> FluidSynth ()
addSoundFontLoader = undefined

newtype Sample = Sample C'fluid_sample_t
newtype Voice = Voice C'fluid_voice_t

allocVoice :: Sample -> Event.Channel -> Event.Key -> Event.Velocity -> SynthThread (Maybe Voice)
allocVoice = undefined

startVoice :: Voice -> SynthThread ()
startVoice = undefined

type VoiceID = Int

getVoiceList :: Int -> VoiceID -> SynthThread [Voice]
getVoiceList = undefined

handleMidiEvent :: HandleMidiEvent
handleMidiEvent = undefined
