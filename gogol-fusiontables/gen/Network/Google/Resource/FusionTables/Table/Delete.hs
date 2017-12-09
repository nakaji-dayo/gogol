{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.FusionTables.Table.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a table.
--
-- /See:/ <https://developers.google.com/fusiontables Fusion Tables API Reference> for @fusiontables.table.delete@.
module Network.Google.Resource.FusionTables.Table.Delete
    (
    -- * REST Resource
      TableDeleteResource

    -- * Creating a Request
    , tableDelete
    , TableDelete

    -- * Request Lenses
    , tddTableId
    , tddFields
    ) where

import Network.Google.FusionTables.Types
import Network.Google.Prelude

-- | A resource alias for @fusiontables.table.delete@ method which the
-- 'TableDelete' request conforms to.
type TableDeleteResource =
     "fusiontables" :>
       "v2" :>
         "tables" :>
           Capture "tableId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Deletes a table.
--
-- /See:/ 'tableDelete' smart constructor.
data TableDelete = TableDelete'
    { _tddTableId :: !Text
    , _tddFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TableDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tddTableId'
--
-- * 'tddFields'
tableDelete
    :: Text -- ^ 'tddTableId'
    -> TableDelete
tableDelete pTddTableId_ = 
    TableDelete'
    { _tddTableId = pTddTableId_
    , _tddFields = Nothing
    }

-- | ID of the table to be deleted.
tddTableId :: Lens' TableDelete Text
tddTableId
  = lens _tddTableId (\ s a -> s{_tddTableId = a})

-- | Selector specifying which fields to include in a partial response.
tddFields :: Lens' TableDelete (Maybe Text)
tddFields
  = lens _tddFields (\ s a -> s{_tddFields = a})

instance GoogleRequest TableDelete where
        type Rs TableDelete = ()
        type Scopes TableDelete =
             '["https://www.googleapis.com/auth/fusiontables"]
        requestClient TableDelete'{..}
          = go _tddTableId _tddFields (Just AltJSON)
              fusionTablesService
          where go
                  = buildClient (Proxy :: Proxy TableDeleteResource)
                      mempty
