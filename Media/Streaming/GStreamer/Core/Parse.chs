--  GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software: you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public License
--  as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--  
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--  
--  You should have received a copy of the GNU Lesser General Public
--  License along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.
--  
--  GStreamer, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GStreamer documentation.
--  
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module Media.Streaming.GStreamer.Core.Parse (
  parseLaunch,
  parseLaunchFromArgs,
  parseBinFromDescription
  ) where

{#import Media.Streaming.GStreamer.Core.Types#}
import System.Glib.GError
import System.Glib.UTFString
import System.Glib.FFI

{# context lib = "gstreamer" prefix = "gst" #}

-- N.B. When passing a GError to a parse* function, it is important to set its
-- contents to NULL. GStreamer has assertions of the form
-- 
--     g_return_val_if_fail (error == NULL || *error == NULL, NULL);
--     
-- Violating them may lead to a segfault when assertions are enabled.

parseLaunch :: String
            -> IO (Maybe Element, Maybe GError)
parseLaunch pipelineDescription =
    withUTFString pipelineDescription $ \cPipelineDescription ->
        alloca $ \gErrorPtr ->
            do poke gErrorPtr nullPtr
               element <- {# call parse_launch #} cPipelineDescription (castPtr gErrorPtr) >>=
                              maybePeek takeObject
               gError <- peek gErrorPtr >>= maybePeek peek
               return (element, gError)

parseLaunchFromArgs :: [String]
                    -> IO (Maybe Element, Maybe GError)
parseLaunchFromArgs args =
    withUTFStringArray0 args $ \cArgs ->
        alloca $ \gErrorPtr ->
            do poke gErrorPtr nullPtr
               element <- {# call parse_launchv #} (castPtr cArgs) (castPtr gErrorPtr) >>=
                              maybePeek takeObject
               gError <- peek gErrorPtr >>= maybePeek peek
               return (element, gError)

parseBinFromDescription :: String
                        -> Bool
                        -> IO (Maybe Element, Maybe GError)
parseBinFromDescription binDescription ghostUnconnectedPads =
    withUTFString binDescription $ \cBinDescription ->
        alloca $ \gErrorPtr ->
            do poke gErrorPtr nullPtr
               element <- {# call parse_bin_from_description #} cBinDescription
                                                                (fromBool ghostUnconnectedPads)
                                                                (castPtr gErrorPtr) >>=
                              maybePeek takeObject
               gError <- peek gErrorPtr >>= maybePeek peek
               return (element, gError)
