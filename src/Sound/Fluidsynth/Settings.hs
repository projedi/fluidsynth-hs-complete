{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Settings for fluidsynth.
--
-- Settings are typed maps from \"string.like.this\" to \"value\". Where
-- value is either a 'SettingString', 'SettingInt' or 'SettingDouble'. All settings have
-- a current value, a default value and can be set at runtime. Integer
-- and double also have an acceptable range of values and string might have
-- an acceptable list of options.
--
-- Even though one can change any setting at runtime only some will actually
-- change behaviour. These are indicated by 'isMutable' function.
--
-- There is actually an extra type called 'SettingSet' and it means
-- a setting \"string.like\" is a collection of settings (e.g. \"string.like.this\",
-- \"string.like.that\", ...)
--
-- There is also a notion of hints but it is not implemented yet.
-- TODO: Add support for hints.
--
-- getX, getRangeX, getDefaultX will return Nothing if no setting of this type
-- was found.
--
-- setX will return False if 
--
-- * Setting was found but was of wrong type
--
-- * Setting was not found and could not have been created(e.g. creating
--   \"string.like.this\" would fail if there exist a setting \"string.like\"
--   which is not of type 'SettingSet')
module Sound.Fluidsynth.Settings( Settings
                                , runSettings
                                , SettingType(..)
                                , isMutable
                                , getType
                                , forSetting
                                -- * Dealing with integer settings
                                , getInt
                                , getRangeInt
                                , getDefaultInt
                                , setInt
                                -- * Dealing with floating point settings
                                , getDouble
                                , getRangeDouble
                                , getDefaultDouble
                                , setDouble
                                -- * Dealing with string settings
                                , getString
                                , getOptionsString
                                , getDefaultString
                                , setString
                                ) where

import Control.Applicative
import Control.Monad

import Control.Arrow((***))
import Control.Exception(assert)
import Control.Monad.Reader(ReaderT, ask, runReaderT)
import Control.Monad.Trans(MonadIO, liftIO)
import Data.IORef(modifyIORef, newIORef, readIORef)
import Foreign.C.String(CString, newCString, peekCString, withCString)
import Foreign.C.Types(CInt)
import Foreign.Marshal.Alloc(alloca)
import Foreign.Ptr(Ptr, castPtr, nullPtr)
import Foreign.Storable(Storable, peek)

import Sound.Fluidsynth.Internal.FFI.Settings
import Sound.Fluidsynth.Internal.FFI.Types
import Sound.Fluidsynth.Internal.Types( MonadFluid, MonadSettings, fluidDataPtr
                                      , runFluidMonad, settingsPtr)

newtype Settings a = Settings (ReaderT (Ptr C'fluid_settings_t) IO a)
   deriving (Functor, Applicative, Monad, MonadIO)

instance MonadFluid Settings where
   runFluidMonad ptr (Settings m) = runReaderT m (castPtr ptr)
   fluidDataPtr = Settings (castPtr <$> ask)

instance MonadSettings Settings where
   settingsPtr = Settings ask

runSettings :: Settings a -> IO a
runSettings (Settings m) = do
   ptr <- c'new_fluid_settings
   res <- runReaderT m (castPtr ptr)
   c'delete_fluid_settings ptr
   return res

-- | It is called \"realtime\" in fluidsynth.
isMutable :: (MonadSettings m) => String -> m Bool
isMutable name = do
   setptr <- settingsPtr
   res <- liftIO $ withCString name $ c'fluid_settings_is_realtime setptr
   return $ res /= 0

data SettingType = SettingInt
                 | SettingDouble
                 | SettingString
                 | SettingSet -- ^ not a setting but contains settings

getType :: (MonadSettings m) => String -> m (Maybe SettingType)
getType name = do
   setptr <- settingsPtr
   res <- liftIO $ withCString name $ c'fluid_settings_get_type setptr
   case () of
    _ | res == c'FLUID_NO_TYPE -> return $ Nothing
      | res == c'FLUID_NUM_TYPE -> return $ Just SettingDouble
      | res == c'FLUID_INT_TYPE -> return $ Just SettingInt
      | res == c'FLUID_STR_TYPE -> return $ Just SettingString
      | res == c'FLUID_SET_TYPE -> return $ Just SettingSet
      | otherwise -> assert False undefined

-- | Traverse all of the available settings
forSetting :: (MonadSettings m) => (String -> SettingType -> m ()) -> m ()
forSetting f = do
   setptr <- settingsPtr
   ptr <- fluidDataPtr
   liftIO $ do
      ccback <- mk'fluid_settings_foreach_t callback
      c'fluid_settings_foreach setptr ptr ccback
 where callback ptr cname sctype = do
          name <- peekCString cname
          runFluidMonad ptr $ f name (stype sctype)
       stype sctype
        | sctype == c'FLUID_NUM_TYPE = SettingDouble
        | sctype == c'FLUID_INT_TYPE = SettingInt
        | sctype == c'FLUID_STR_TYPE = SettingString
        | otherwise = assert False undefined

getValue :: (MonadSettings m, Storable a) => String -> CInt ->
   (Ptr C'fluid_settings_t -> CString -> Ptr a -> IO CInt) -> m (Maybe a)
getValue name wantedType getter = do
   setptr <- settingsPtr
   liftIO $ withCString name $ \cname -> do
      ctype <- c'fluid_settings_get_type setptr cname
      case () of
       _ | ctype == wantedType -> alloca $ \valptr -> do
              res <- getter setptr cname valptr
              case () of
               _ | res == 1 -> Just <$> peek valptr
                 | otherwise -> return Nothing
         | otherwise -> return Nothing

getDefaultValue :: (MonadSettings m) => String -> CInt ->
   (Ptr C'fluid_settings_t -> CString -> IO a) -> m (Maybe a)
getDefaultValue name wantedType getter = do
   setptr <- settingsPtr
   liftIO $ withCString name $ \cname -> do
      ctype <- c'fluid_settings_get_type setptr cname
      case () of
       _ | ctype == wantedType -> Just <$> getter setptr cname
         | otherwise -> return Nothing

getRange :: (MonadSettings m, Storable a) => String -> CInt ->
   (Ptr C'fluid_settings_t -> CString -> Ptr a -> Ptr a -> IO ()) -> m (Maybe (a, a))
getRange name wantedType getter = do
   setptr <- settingsPtr
   liftIO $ withCString name $ \cname -> do
      ctype <- c'fluid_settings_get_type setptr cname
      case () of
       _ | ctype == wantedType -> alloca $ \minptr -> alloca $ \maxptr -> do
          getter setptr cname minptr maxptr
          Just <$> ((,) <$> peek minptr <*> peek maxptr)
         | otherwise -> return Nothing

setValue :: (MonadSettings m) => String -> CInt ->
   (Ptr C'fluid_settings_t -> CString -> a -> IO CInt) -> a -> m Bool
setValue name wantedType setter val = do
   setptr <- settingsPtr
   liftIO $ withCString name $ \cname -> do
      ctype <- c'fluid_settings_get_type setptr cname
      case () of
       _ | ctype == wantedType -> do
              res <- setter setptr cname val
              return $ res == 1
         | otherwise -> return False

getInt :: (MonadSettings m) => String -> m (Maybe Int)
getInt name = (fromIntegral <$>) <$>
   getValue name c'FLUID_INT_TYPE c'fluid_settings_getint

getRangeInt :: (MonadSettings m) => String -> m (Maybe (Int, Int))
getRangeInt name = ((fromIntegral *** fromIntegral) <$>) <$>
   getRange name c'FLUID_INT_TYPE c'fluid_settings_getint_range

getDefaultInt :: (MonadSettings m) => String -> m (Maybe Int)
getDefaultInt name = (fromIntegral <$>) <$>
   getDefaultValue name c'FLUID_INT_TYPE c'fluid_settings_getint_default

setInt :: (MonadSettings m) => String -> Int -> m Bool
setInt name = setValue name c'FLUID_INT_TYPE c'fluid_settings_setint . fromIntegral


getDouble :: (MonadSettings m) => String -> m (Maybe Double)
getDouble name = (realToFrac <$>) <$>
   getValue name c'FLUID_NUM_TYPE c'fluid_settings_getnum

getRangeDouble :: (MonadSettings m) => String -> m (Maybe (Double, Double))
getRangeDouble name = ((realToFrac *** realToFrac) <$>) <$>
   getRange name c'FLUID_NUM_TYPE c'fluid_settings_getnum_range

getDefaultDouble :: (MonadSettings m) => String -> m (Maybe Double)
getDefaultDouble name = (realToFrac <$>) <$>
   getDefaultValue name c'FLUID_NUM_TYPE c'fluid_settings_getnum_default

setDouble :: (MonadSettings m) => String -> Double -> m Bool
setDouble name = setValue name c'FLUID_NUM_TYPE c'fluid_settings_setnum . realToFrac


getString :: (MonadSettings m) => String -> m (Maybe String)
getString name = do
   mcstr <- getValue name c'FLUID_STR_TYPE c'fluid_settings_getstr
   case mcstr of
    Just cstr -> Just <$> liftIO (peekCString cstr)
    Nothing -> return Nothing

-- | Get allowed values for this string option. Nothing if any value is accepted.
getOptionsString :: (MonadSettings m) => String -> m (Maybe [String])
getOptionsString name = do
   setptr <- settingsPtr
   liftIO $ withCString name $ \cname -> do
      ctype <- c'fluid_settings_get_type setptr cname
      case () of
       _ | ctype == c'FLUID_STR_TYPE -> do
              count <- c'fluid_settings_option_count setptr cname
              iterateOverOptions nullPtr setptr cname count
         | otherwise -> return Nothing
 where iterateOverOptions _ _ _ 0 = return $ Just []
       iterateOverOptions ptr setptr cname _ = do
          res <- newIORef []
          ccback <- mk'fluid_settings_foreach_option_t (callback res)
          c'fluid_settings_foreach_option setptr cname ptr ccback
          Just <$> readIORef res
       callback res _ _ option = do
          str <- peekCString option
          modifyIORef res (str:)

getDefaultString :: (MonadSettings m) => String -> m (Maybe String)
getDefaultString name = do
   mcstr <- getDefaultValue name c'FLUID_STR_TYPE c'fluid_settings_getstr_default
   case mcstr of
    Just cstr -> Just <$> liftIO (peekCString cstr)
    Nothing -> return Nothing

setString :: (MonadSettings m) => String -> String -> m Bool
setString name val = do
   cstr <- liftIO $ newCString val
   setValue name c'FLUID_STR_TYPE c'fluid_settings_setstr cstr
