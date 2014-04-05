{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Gen where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#integral_t enum fluid_gen_type
#num GEN_STARTADDROFS
#num GEN_ENDADDROFS
#num GEN_STARTLOOPADDROFS
#num GEN_ENDLOOPADDROFS
#num GEN_STARTADDRCOARSEOFS
#num GEN_MODLFOTOPITCH
#num GEN_VIBLFOTOPITCH
#num GEN_MODENVTOPITCH
#num GEN_FILTERFC
#num GEN_FILTERQ
#num GEN_MODLFOTOFILTERFC
#num GEN_MODENVTOFILTERFC
#num GEN_ENDADDRCOARSEOFS
#num GEN_MODLFOTOVOL
#num GEN_UNUSED1
#num GEN_CHORUSSEND
#num GEN_REVERBSEND
#num GEN_PAN
#num GEN_UNUSED2
#num GEN_UNUSED3
#num GEN_UNUSED4
#num GEN_MODLFODELAY
#num GEN_MODLFOFREQ
#num GEN_VIBLFODELAY
#num GEN_VIBLFOFREQ
#num GEN_MODENVDELAY
#num GEN_MODENVATTACK
#num GEN_MODENVHOLD
#num GEN_MODENVDECAY
#num GEN_MODENVSUSTAIN
#num GEN_MODENVRELEASE
#num GEN_KEYTOMODENVHOLD
#num GEN_KEYTOMODENVDECAY
#num GEN_VOLENVDELAY
#num GEN_VOLENVATTACK
#num GEN_VOLENVHOLD
#num GEN_VOLENVDECAY
#num GEN_VOLENVSUSTAIN
#num GEN_VOLENVRELEASE
#num GEN_KEYTOVOLENVHOLD
#num GEN_KEYTOVOLENVDECAY
#num GEN_INSTRUMENT
#num GEN_RESERVED1
#num GEN_KEYRANGE
#num GEN_VELRANGE
#num GEN_STARTLOOPADDRCOARSEOFS
#num GEN_KEYNUM
#num GEN_VELOCITY
#num GEN_ATTENUATION
#num GEN_RESERVED2
#num GEN_ENDLOOPADDRCOARSEOFS
#num GEN_COARSETUNE
#num GEN_FINETUNE
#num GEN_SAMPLEID
#num GEN_SAMPLEMODE
#num GEN_RESERVED3
#num GEN_SCALETUNE
#num GEN_EXCLUSIVECLASS
#num GEN_OVERRIDEROOTKEY
{- the initial pitch is not a "standard" generator. It is not
   mentioned in the list of generator in the SF2 specifications. It
   is used, however, as the destination for the default pitch wheel
   modulator.
-}
#num GEN_PITCH
#num GEN_LAST

#integral_t enum fluid_gen_flags
#num GEN_UNUSED
#num GEN_SET
-- Generator is an absolute value
#num GEN_ABS_NRPN

#starttype struct _fluid_gen_t
#field flags , <fluid_gen_flags>
-- The nominal value
#field val , CDouble
-- Change by modulators 
#field mod , CDouble
-- Change by NRPN messages
#field nrpn , CDouble
#stoptype

#synonym_t fluid_gen_t , <_fluid_gen_t>

#ccall fluid_gen_set_default_values , Ptr <fluid_gen_t> -> IO CInt
