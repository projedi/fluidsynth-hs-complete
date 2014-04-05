{-# LANGUAGE TemplateHaskell #-}
module Sound.Fluidsynth.Midi
   ( MidiEvent(..)
   -- Lenses for MidiEvent
   , midiEventType
   , midiEventChannel
   , midiEventKey
   , midiEventVelocity
   , midiEventControl
   , midiEventValue
   , midiEventProgram
   , midiEventPitch
   , MidiRouterRuleType(..)
   , MidiRouterRuleData(..)
   -- Lenses for MidiRouterRuleData
   , midiRouterRuleDataMin
   , midiRouterRuleDataMax
   , midiRouterRuleDataMul
   , midiRouterRuleDataAdd
   , MidiRouterRule(..)
   -- Lenses for MidiRouterRule
   , midiRouterRuleChannel
   , midiRouterRuleParam1
   , midiRouterRuleParam2
   , HandleMidiEvent
   , FluidPlayerStatus(..)
   , replaceMidiRouter
   , deleteMidiRouter
   , setDefaultMidiRouterRules
   , clearMidiRouterRules
   , addMidiRouterRule
   , midiRouterHandleMidiEvent
   , midiDumpPreRouter
   , midiDumpPostRouter
   , replaceMidiDriver
   , deleteMidiDriver
   , addMidiPlayer
   , removeMidiPlayer
   , midiPlayerAddFile
   , midiPlayerPlay
   , midiPlayerStop
   , midiPlayerJoin
   , midiPlayerSetLoop
   , midiPlayerSetMidiTempo
   , midiPlayerSetBPM
   , midiPlayerGetStatus
   , setMidiPlayerCallback
   ) where

import Control.Lens

import Sound.Fluidsynth.Internal.Event
   (Channel, Control, Key, Pitch, PresetNum, Pitch, Value, Velocity)
import Sound.Fluidsynth.Internal.FFI.Midi
import Sound.Fluidsynth.Internal.Types

data MidiEvent = MidiEvent
   { _midiEventType :: Int
   , _midiEventChannel :: Channel
   , _midiEventKey :: Key
   , _midiEventVelocity :: Velocity
   , _midiEventControl :: Control
   , _midiEventValue :: Value
   , _midiEventProgram :: PresetNum
   , _midiEventPitch :: Pitch
   }

makeLenses ''MidiEvent

-- TODO: SYSEX

data MidiRouterRuleType
   = MidiRouterRuleNote
   | MidiRouterRuleCC
   | MidiRouterRuleProgChange
   | MidiRouterRulePitchBend
   | MidiRouterRuleChannelPressure
   | MidiRouterRuleKeyPressure

data MidiRouterRuleData = MidiRouterRuleData
   { _midiRouterRuleDataMin :: Int
   , _midiRouterRuleDataMax :: Int
   , _midiRouterRuleDataMul :: Float
   , _midiRouterRuleDataAdd :: Int
   }

makeLenses ''MidiRouterRuleData

data MidiRouterRule = MidiRouterRule
   { _midiRouterRuleChannel :: MidiRouterRuleData
   , _midiRouterRuleParam1 :: MidiRouterRuleData
   , _midiRouterRuleParam2 :: MidiRouterRuleData
   }

makeLenses ''MidiRouterRule

type HandleMidiEvent = MidiEvent -> FluidSynth Bool

replaceMidiRouter :: HandleMidiEvent -> FluidSynth Bool
replaceMidiRouter = undefined

deleteMidiRouter :: FluidSynth ()
deleteMidiRouter = undefined

setDefaultMidiRouterRules :: FluidSynth Bool
setDefaultMidiRouterRules = undefined

clearMidiRouterRules :: FluidSynth Bool
clearMidiRouterRules = undefined

addMidiRouterRule :: MidiRouterRule -> MidiRouterRuleType -> FluidSynth Bool
addMidiRouterRule = undefined

midiRouterHandleMidiEvent :: HandleMidiEvent
midiRouterHandleMidiEvent = undefined

midiDumpPreRouter :: HandleMidiEvent
midiDumpPreRouter = undefined

midiDumpPostRouter :: HandleMidiEvent
midiDumpPostRouter = undefined

replaceMidiDriver :: HandleMidiEvent -> FluidSynth Bool
replaceMidiDriver = undefined

deleteMidiDriver :: FluidSynth ()
deleteMidiDriver = undefined

data FluidPlayerStatus
   = FluidPlayerReady
   | FluidPlayerPlaying
   | FluidPlayerDone

addMidiPlayer :: FluidSynth (Maybe MidiPlayer)
addMidiPlayer = undefined

removeMidiPlayer :: MidiPlayer -> FluidSynth Bool
removeMidiPlayer = undefined

midiPlayerAddFile :: MidiPlayer -> String -> FluidSynth Bool
midiPlayerAddFile = undefined

-- TODO: What is best to use here? ByteString?
-- midiPlayerAddBuffer :: MidiPlayer -> ??? -> FluidSynth Bool

midiPlayerPlay :: MidiPlayer -> FluidSynth Bool
midiPlayerPlay = undefined

midiPlayerStop :: MidiPlayer -> FluidSynth ()
midiPlayerStop = undefined

midiPlayerJoin :: MidiPlayer -> FluidSynth Bool
midiPlayerJoin = undefined

midiPlayerSetLoop :: MidiPlayer -> Bool -> FluidSynth ()
midiPlayerSetLoop = undefined

midiPlayerSetMidiTempo :: MidiPlayer -> Bool -> FluidSynth ()
midiPlayerSetMidiTempo = undefined

midiPlayerSetBPM :: MidiPlayer -> Bool -> FluidSynth ()
midiPlayerSetBPM = undefined

midiPlayerGetStatus :: MidiPlayer -> FluidSynth FluidPlayerStatus
midiPlayerGetStatus = undefined

setMidiPlayerCallback :: MidiPlayer -> HandleMidiEvent -> FluidSynth ()
setMidiPlayerCallback = undefined
