-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry SourceTagTable@
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--          
--  Created: 22 October 2003
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- @description@ --------------------------------------------------------------
--
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------
--
--
module SourceTagTable (
  SourceTagTable,
  SourceTagTableClass,
  sourceTagTableNew,
  sourceTagTableAddTags,
  sourceTagTableRemoveSourceTags
) where

import Monad	(liftM)
import FFI
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import SourceViewType#}
{#import Signal#}
import SourceTag
import GList	(fromGSList, toGSList)

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor sourceTagTableNew@ Create a new @ref type SourceTagTable@
--
sourceTagTableNew :: IO SourceTagTable
sourceTagTableNew = makeNewGObject mkSourceTagTable
  {#call unsafe source_tag_table_new#} 


-- @method sourceTagTableAddTags@ Add a list of tag to the table.
-- 
-- * The added tags are assigned the highest priority in the table. If a tag is
--   already present in table or has the same name as an already-added tag, then
--   it is not added to the table.
-- 
sourceTagTableAddTags :: SourceTagTable -> [SourceTag] -> IO ()
sourceTagTableAddTags tt tags = do
  let tagForeignPtrs = map (unSourceTag . toSourceTag) tags
  tagList <- toGSList (map foreignPtrToPtr tagForeignPtrs)
  {#call source_tag_table_add_tags#} tt tagList
  -- destroy the list
  fromGSList tagList
  -- make sure the ForeignPtrs are not gc'd while we are still using the Ptrs
  mapM_ touchForeignPtr tagForeignPtrs

-- @method sourceTagTableRemoveSourceTags@
-- 
sourceTagTableRemoveSourceTags :: SourceTagTable -> IO ()
sourceTagTableRemoveSourceTags tt =
  {#call source_tag_table_remove_source_tags#} tt 

-- @signal onTagChanged@ The source tag table has changed.
--
onTagChanged, afterTagChanged :: 
  SourceTagTableClass stt => stt -> IO () -> IO (ConnectId stt)
onTagChanged = connect_NONE__NONE "changed" False
afterTagChanged = connect_NONE__NONE "changed" True