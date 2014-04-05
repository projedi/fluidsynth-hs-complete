-- | fluid_log is not present because it is a vararg function
--   and ffi to them is deprecated:
--
--   \"Note that for a C function defined to accept a variable number of arguments,
--   all arguments beyond the explicitly typed arguments suffer argument promotion.
--   However, because C permits the calling convention to be different for such
--   functions, a Haskell system will, in general, not be able to make use of
--   variable argument functions. Hence, their use is deprecated in portable code.\"
--
--   from <http://www.haskell.org/onlinereport/haskell2010/haskellch8.html>
--   (8.5.1 at the very end)
module Sound.Fluidsynth.Log
   ( LogLevel(..)
   , Logger
   , defaultLogger
   , setLogger
   ) where

import Control.Monad.Trans(liftIO)
import Control.Exception(assert)
import Foreign.C.String(peekCString, withCString)
import Foreign.Ptr(nullPtr)

import Sound.Fluidsynth.Internal.Types
import Sound.Fluidsynth.Internal.FFI.Log

data LogLevel
   = LogLevelPanic
   | LogLevelError
   | LogLevelWarning
   | LogLevelInfo
   | LogLevelDebug

llToC :: LogLevel -> C'fluid_log_level
llToC LogLevelPanic = c'FLUID_PANIC
llToC LogLevelError = c'FLUID_ERR
llToC LogLevelWarning = c'FLUID_WARN
llToC LogLevelInfo = c'FLUID_INFO
llToC LogLevelDebug = c'FLUID_DBG

llFromC :: C'fluid_log_level -> LogLevel
llFromC ll =
   case () of
    _ | ll == c'FLUID_PANIC -> LogLevelPanic
      | ll == c'FLUID_ERR -> LogLevelError
      | ll == c'FLUID_WARN -> LogLevelWarning
      | ll == c'FLUID_INFO -> LogLevelInfo
      | ll == c'FLUID_DBG -> LogLevelDebug
      | otherwise -> assert False undefined

type Logger = LogLevel -> String -> FluidSynth ()

-- | Will just print to stderr
defaultLogger :: Logger
defaultLogger lvl str = FluidSynth $
   liftIO $ withCString str $ \cstr ->
      c'fluid_default_log_function (llToC lvl) cstr nullPtr

-- | Set custom 'Logger' for a specific 'LogLevel'
setLogger :: LogLevel
          -> Logger -- ^ new 'Logger'
          -> FluidSynth Logger -- ^ previous 'Logger'
setLogger ll logger = do
   dataPtr <- fluidDataPtr
   cback <- liftIO $ mk'fluid_log_function_t callback
   prevcback <- liftIO $ c'fluid_set_log_function (llToC ll) cback dataPtr
   return $ fromCallback $ mK'fluid_log_function_t prevcback
 where callback cll cstr dataPtr = do
          str <- peekCString cstr
          runFluidMonad (logger (llFromC cll) str) dataPtr
       fromCallback f ll' str = do
          dataPtr <- fluidDataPtr
          liftIO $ withCString str $ \cstr -> f (llToC ll') cstr dataPtr
