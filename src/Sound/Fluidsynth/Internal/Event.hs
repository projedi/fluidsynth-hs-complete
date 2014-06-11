{-# LANGUAGE TemplateHaskell #-}
module Sound.Fluidsynth.Internal.Event
   ( Channel
   , Key
   , Velocity
   , Duration
   , BankNum
   , PresetNum
   , SoundFontID
   , Value
   , Control
   , ClientID
   , Pitch
   , SeqEventType(..)
   , SeqEvent(..)
   , createEvent
   , parseEvent
   -- Lenses for SeqEvent
   , seqEventType
   , seqEventSource
   , seqEventDestination
   ) where

import Control.Applicative
import Control.Lens

import Foreign.Ptr(Ptr, nullPtr)

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

type SoundFontID = Int

type Value = Int

-- | 0 - 127
type Control = Int

type ClientID = Int

-- | 0 - 16383, 8192 - no bend
type Pitch = Int

data SeqEventType
   = SeqNote Channel Key Velocity Duration
   | SeqNoteOn Channel Key Velocity
   | SeqNoteOff Channel Key
   | SeqAllSoundsOff Channel -- ^ All notes are terminated without going through off phase.
   | SeqAllNotesOff Channel
   | SeqBankSelect Channel BankNum
   | SeqProgramChange Channel PresetNum
   | SeqProgramSelect Channel SoundFontID BankNum PresetNum
   | SeqPitchBend Channel Pitch
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
   | SeqAnyControlChange Channel -- TODO: Do I even need it?
   | SeqChannelPressure Channel Value -- ^ 0 - 127, aka aftertouch
   | SeqSystemReset
   | SeqUnregistering -- ^ Used to unregister a client

data SeqEvent = SeqEvent
   { _seqEventType :: SeqEventType
   , _seqEventSource :: ClientID
   , _seqEventDestination :: ClientID
   }

makeLenses ''SeqEvent

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

createEvent :: SeqEvent -> IO (Ptr C'fluid_event_t)
createEvent event = do
   evptr <- c'new_fluid_event
   c'fluid_event_set_source evptr $ fi (event ^. seqEventSource)
   c'fluid_event_set_dest evptr $ fi (event ^. seqEventDestination)
   setType evptr (event ^. seqEventType)
   return evptr
 where setType p (SeqNote c k v d) = c'fluid_event_note p (fi c) (fi k) (fi v) (fi d)
       setType p (SeqNoteOn c k v) = c'fluid_event_noteon p (fi c) (fi k) (fi v)
       setType p (SeqNoteOff c k) = c'fluid_event_noteoff p (fi c) (fi k)
       setType p (SeqAllSoundsOff c) = c'fluid_event_all_sounds_off p (fi c)
       setType p (SeqAllNotesOff c) = c'fluid_event_all_notes_off p (fi c)
       setType p (SeqBankSelect c b) = c'fluid_event_bank_select p (fi c) (fi b)
       setType p (SeqProgramChange c pr) = c'fluid_event_program_change p (fi c) (fi pr)
       setType p (SeqProgramSelect c s b pr) =
          c'fluid_event_program_select p (fi c) (fi s) (fi b) (fi pr)
       setType p (SeqPitchBend c pt) = c'fluid_event_pitch_bend p (fi c) (fi pt)
       setType p (SeqPitchWheelSensitivity c v) =
          c'fluid_event_pitch_wheelsens p (fi c) (fi v)
       setType p (SeqModulation c v) = c'fluid_event_modulation p (fi c) (fi v)
       setType p (SeqSustain c v) = c'fluid_event_sustain p (fi c) (fi v)
       setType p (SeqControlChange c ct v) =
          c'fluid_event_control_change p (fi c) (fi ct) (fi v)
       setType p (SeqPan c v) = c'fluid_event_pan p (fi c) (fi v)
       setType p (SeqVolume c v) = c'fluid_event_volume p (fi c) (fi v)
       setType p (SeqReverbSend c v) = c'fluid_event_reverb_send p (fi c) (fi v)
       setType p (SeqChorusSend c v) = c'fluid_event_chorus_send p (fi c) (fi v)
       setType p SeqTimer = c'fluid_event_timer p nullPtr
       setType p (SeqAnyControlChange c) = c'fluid_event_any_control_change p (fi c)
       setType p (SeqChannelPressure c v) = c'fluid_event_channel_pressure p (fi c) (fi v)
       setType p SeqSystemReset = c'fluid_event_system_reset p
       setType p SeqUnregistering = c'fluid_event_unregistering p
  

parseEvent :: Ptr C'fluid_event_t -> IO (Maybe SeqEvent)
parseEvent p = do
   t <- c'fluid_event_get_type p
   c <- fi <$> c'fluid_event_get_channel p
   k <- fi <$> c'fluid_event_get_key p
   ve <- fi <$> c'fluid_event_get_velocity p
   ct <- fi <$> c'fluid_event_get_control p
   va <- fi <$> c'fluid_event_get_value p
   pr <- fi <$> c'fluid_event_get_program p
   da <- c'fluid_event_get_data p
   du <- fi <$> c'fluid_event_get_duration p
   b <- fi <$> c'fluid_event_get_bank p
   pt <- fi <$> c'fluid_event_get_pitch p
   sf <- fi <$> c'fluid_event_get_sfont_id p
   s <- fi <$> c'fluid_event_get_source p 
   d <- fi <$> c'fluid_event_get_dest p
   return $ SeqEvent <$> getType t c k ve ct va pr da du b pt sf <*> pure s <*> pure d
 where getType t c k ve ct va pr _ du b pt sf =
          case () of
           _ | t == c'FLUID_SEQ_NOTE -> Just $ SeqNote c k ve du
             | t == c'FLUID_SEQ_NOTEON -> Just $ SeqNoteOn c k ve
             | t == c'FLUID_SEQ_NOTEOFF -> Just $ SeqNoteOff c k
             | t == c'FLUID_SEQ_ALLSOUNDSOFF -> Just $ SeqAllSoundsOff c
             | t == c'FLUID_SEQ_ALLNOTESOFF -> Just $ SeqAllNotesOff c
             | t == c'FLUID_SEQ_BANKSELECT -> Just $ SeqBankSelect c b
             | t == c'FLUID_SEQ_PROGRAMCHANGE -> Just $ SeqProgramChange c pr
             | t == c'FLUID_SEQ_PROGRAMSELECT -> Just $ SeqProgramSelect c sf b pr
             | t == c'FLUID_SEQ_PITCHBEND -> Just $ SeqPitchBend c pt
             | t == c'FLUID_SEQ_PITCHWHEELSENS -> Just $ SeqPitchWheelSensitivity c va
             | t == c'FLUID_SEQ_MODULATION -> Just $ SeqModulation c va
             | t == c'FLUID_SEQ_SUSTAIN -> Just $ SeqSustain c va
             | t == c'FLUID_SEQ_CONTROLCHANGE -> Just $ SeqControlChange c ct va
             | t == c'FLUID_SEQ_PAN -> Just $ SeqPan c va
             | t == c'FLUID_SEQ_VOLUME -> Just $ SeqVolume c va
             | t == c'FLUID_SEQ_REVERBSEND -> Just $ SeqReverbSend c va
             | t == c'FLUID_SEQ_CHORUSSEND -> Just $ SeqChorusSend c va
             | t == c'FLUID_SEQ_TIMER -> Just SeqTimer
             | t == c'FLUID_SEQ_ANYCONTROLCHANGE -> Just $ SeqAnyControlChange c
             | t == c'FLUID_SEQ_CHANNELPRESSURE -> Just $ SeqChannelPressure c va
             | t == c'FLUID_SEQ_SYSTEMRESET -> Just SeqSystemReset
             | t == c'FLUID_SEQ_UNREGISTERING -> Just SeqUnregistering
             | otherwise -> Nothing
