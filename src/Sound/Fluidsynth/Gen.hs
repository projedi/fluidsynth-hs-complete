module Sound.Fluidsynth.Gen
   ( Generator(..)
   , generatorFlags
   , generatorValue
   , generatorModulatorChange
   , generatorNrpnChange
   , GeneratorFlags(..)
   , GeneratorType(..)
   , defaultGenerators
   , generatorCount
   , getGenerator
   , setGenerator
   ) where

import Sound.Fluidsynth.Internal.FFI.Gen
import Sound.Fluidsynth.Internal.FFI.Synth
import Sound.Fluidsynth.Internal.FFI.Types
import Sound.Fluidsynth.Internal.Types

data GeneratorFlags
   = GeneratorFlagsValueNotSet
   | GeneratorFlagsValueSet
   | GeneratorFlagsValueAbsolute

data Generator = Generator
   { _generatorFlags :: [GeneratorFlags]
   , _generatorValue :: Double
   , _generatorModulatorChange :: Double
   , _generatorNrpnChange :: Double
   }

makeLenses ''Generator

-- | Soundfont 2.01 specifications section 8.1.3
data GeneratorType
   = GeneratorTypeStartAddressOffset
   | GeneratorTypeEndAddressOffset
   | GeneratorTypeLoopStartAddressOffset
   | GeneratorTypeLoopEndAddressOffset
   | GeneratorTypeStartAddressCoarseOffset
   | GeneratorTypeModulationLFOPitch
   | GeneratorTypeVibratoLFOPitch
   | GeneratorTypeModulationEnvelopePitch
   | GeneratorTypeFilterCutoff
   | GeneratorTypeFilterQ
   | GeneratorTypeModulationLFOFilterCutoff
   | GeneratorTypeModulationEnvelopeFilterCutoff
   | GeneratorTypeEndAddressCoarseOffset
   | GeneratorTypeModulationLFOVolume
   | GeneratorTypeUnused1
   | GeneratorTypeChorusSend
   | GeneratorTypeReverbSend
   | GeneratorTypePan
   | GeneratorTypeUnused2
   | GeneratorTypeUnused3
   | GeneratorTypeUnused4
   | GeneratorTypeModulationLFODelay
   | GeneratorTypeModulationLFOFrequency
   | GeneratorTypeVibratoLFODelay
   | GeneratorTypeVibratoLFOFrequency
   | GeneratorTypeModulationEnvelopeDelay
   | GeneratorTypeModulationEnvelopeAttack
   | GeneratorTypeModulationEnvelopeHold
   | GeneratorTypeModulationEnvelopeDecay
   | GeneratorTypeModulationEnvelopeSustain
   | GeneratorTypeModulationEnvelopeRelease
   | GeneratorTypeKeyToModulationEnvelopeHold
   | GeneratorTypeKeyToModulationEnvelopeDecay
   | GeneratorTypeVolumeEnvelopeDelay
   | GeneratorTypeVolumeEnvelopeAttack
   | GeneratorTypeVolumeEnvelopeHold
   | GeneratorTypeVolumeEnvelopeDecay
   | GeneratorTypeVolumeEnvelopeSustain
   | GeneratorTypeVolumeEnvelopeRelease
   | GeneratorTypeKeyToVolumeEnvelopeHold
   | GeneratorTypeKeyToVolumeEnvelopeDecay
   | GeneratorTypeInstrumentID -- ^ Should not be set by user
   | GeneratorTypeReserved1
   | GeneratorTypeKeyRange
   | GeneratorTypeVelocityRange
   | GeneratorTypeLoopStartAddressCoarseOffset
   | GeneratorTypeKeyNumber
   | GeneratorTypeVelocityValue
   | GeneratorTypeVolumeAttenuation
   | GeneratorTypeReserved2
   | GeneratorTypeLoopEndAddressCoarseOffset
   | GeneratorTypeCoarseTuning
   | GeneratorTypeFineTuning
   | GeneratorTypeSampleID -- ^ Should not be set by user
   | GeneratorTypeSampleMode
   | GeneratorTypeReserved3
   | GeneratorTypeScaleTuning
   | GeneratorTypeExclusiveClass
   | GeneratorTypeOverrideRootKey
   | GeneratorTypePitch -- ^ Not a real SoundFont generator

generatorCount :: Int
generatorCount = fromIntegral c'GEN_LAST

defaultGenerators :: FluidSynth (Map GeneratorType Generator)
defaultGenerators = undefined

-- set_gen2 from synth.h
setGenerator :: Event.Channel -> GeneratorType -> Float -> Bool -> Bool -> FluidSynth Bool
setGenerator = undefined

-- get_gen from synth.h
getGenerator :: Event.Channel -> GeneratorType -> FluidSynth Float
getGenerator = undefined
