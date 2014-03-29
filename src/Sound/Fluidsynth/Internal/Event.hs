{-# LANGUAGE TemplateHaskell #-}
module Sound.Fluidsynth.Internal.Event
	( Channel
	, Key
	, Velocity
	, Duration
	, BankNum
	, PresetNum
	, SoundfontID
	, Value
	, Control
	, ClientID
	, SeqEventType(..)
	, SeqEvent(..)
	, seqEventType
	, seqEventSource
	, seqEventDestination
	) where

import Control.Applicative
import Control.Lens
import Control.Monad

import Sound.Fluidsynth.Internal.FFI.Event
import Sound.Fluidsynth.Internal.FFI.Types

type Channel = Int

-- | 0 - 127
type Key = Int

-- | 0 - 127
type Velocity = Int

type Duration = Int

-- | 0 - 16383
type BankNum = Int

-- | 0 - 127
type PresetNum = Int

type SoundfontID = Int

type Value = Int

-- | 0 - 127
type Control = Int

type ClientID = Int

data SeqEventType
	= SeqNote Channel Key Velocity Duration
	| SeqNoteOn Channel Key Velocity
	| SeqNoteOff Channel Key
	| SeqAllSoundsOff Channel
	| SeqAllNotesOff Channel
	| SeqBankSelect Channel BankNum
	| SeqProgramChange Channel PresetNum
	| SeqProgramSelect Channel SoundfontID BankNum PresetNum
	| SeqPitchBend Channel Value -- ^ 0 - 16383, 8192 - no bend
	| SeqPitchWheelSensitivity Channel Value -- ^ TODO: What are the units
	| SeqModulation Channel Value -- ^ 0 - 127
	| SeqSustain Channel Value -- ^ 0 - 127
	| SeqControlChange Channel Control Value -- ^ depends on the controller
	| SeqPan Channel Value -- ^ 0 - 127, 0 - left, 64 - middle, 127 - right
	| SeqVolume Channel Value -- ^ 0 - 127
	| SeqReverbSend Channel Value -- ^ 0 - 127
	| SeqChorusSend Channel Value -- ^ 0 - 127
	-- Used for some callback fluid_sequencer_register_callback
	| SeqTimer -- TODO: Some data
	-- Only used as a pattern match to remove all controls
	| SeqAnyControlChange -- TODO: Do I even need it?
	| SeqChannelPressure Channel Value -- ^ 0 - 127, aka aftertouch
	| SeqSystemReset
	| SeqUnregistering -- ^ Used to unregister a client

data SeqEvent = SeqEvent
	{ _seqEventType :: SeqEventType
	, _seqEventSource :: ClientID
	, _seqEventDestination :: ClientID
	}

makeLenses ''SeqEvent
