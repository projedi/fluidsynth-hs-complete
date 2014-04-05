{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <fluidsynth.h>

module Sound.Fluidsynth.Internal.FFI.Shell where
#strict_import

import Sound.Fluidsynth.Internal.FFI.Types

#ccall fluid_get_stdin , IO <fluid_istream_t>
#ccall fluid_get_stdout , IO <fluid_ostream_t>

-- buf,len
#ccall fluid_get_userconf , CString -> CInt -> IO CString
-- buf,len
#ccall fluid_get_sysconf , CString -> CInt -> IO CString

#callback_t fluid_cmd_func_t , Ptr () -> CInt -> Ptr CString -> <fluid_ostream_t> -> IO CInt

#starttype fluid_cmd_t
#field name , CString
#field topic , CString
#field handler , <fluid_cmd_func_t>
#field data , Ptr ()
#field help , CString
#stoptype

-- The command handler

#ccall new_fluid_cmd_handler , Ptr <fluid_synth_t> -> IO (Ptr <fluid_cmd_handler_t>)
#ccall delete_fluid_cmd_handler , Ptr <fluid_cmd_handler_t> -> IO ()
#ccall fluid_cmd_handler_set_synth , Ptr <fluid_cmd_handler_t> -> Ptr <fluid_synth_t> -> IO ()
#ccall fluid_cmd_handler_register , Ptr <fluid_cmd_handler_t> -> Ptr <fluid_cmd_t> -> IO CInt
#ccall fluid_cmd_handler_unregister , Ptr <fluid_cmd_handler_t> -> CString -> IO CInt

-- Command function

#ccall fluid_command , Ptr <fluid_cmd_handler_t> -> CString -> <fluid_ostream_t> -> IO CInt
#ccall fluid_source , Ptr <fluid_cmd_handler_t> -> CString -> IO CInt
#ccall fluid_usershell , Ptr <fluid_settings_t> -> Ptr <fluid_cmd_handler_t> -> IO ()

-- Shell

#ccall new_fluid_shell , Ptr <fluid_settings_t> -> Ptr <fluid_cmd_handler_t> -> <fluid_istream_t> -> <fluid_ostream_t> -> CInt -> IO (Ptr <fluid_shell_t>)
#ccall delete_fluid_shell , Ptr <fluid_shell_t> -> IO ()

-- TCP/IP server

#callback_t fluid_server_newclient_func_t , Ptr () -> CString -> IO (Ptr <fluid_cmd_handler_t>)

#ccall new_fluid_server , Ptr <fluid_settings_t> -> <fluid_server_newclient_func_t> -> Ptr a -> IO (Ptr <fluid_server_t>)
#ccall delete_fluid_server , Ptr <fluid_server_t> -> IO ()
#ccall fluid_server_join , Ptr <fluid_server_t> -> IO CInt
