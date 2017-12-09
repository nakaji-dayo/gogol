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
-- Module      : Network.Google.Resource.FusionTables.Table.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new table.
--
-- /See:/ <https://developers.google.com/fusiontables Fusion Tables API Reference> for @fusiontables.table.insert@.
module Network.Google.Resource.FusionTables.Table.Insert
    (
    -- * REST Resource
      TableInsertResource

    -- * Creating a Request
    , tableInsert
    , TableInsert

    -- * Request Lenses
    , tiPayload
    , tiFields
    ) where

import Network.Google.FusionTables.Types
import Network.Google.Prelude

-- | A resource alias for @fusiontables.table.insert@ method which the
-- 'TableInsert' request conforms to.
type TableInsertResource =
     "fusiontables" :>
       "v2" :>
         "tables" :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :>
               ReqBody '[JSON] Table :> Post '[JSON] Table

-- | Creates a new table.
--
-- /See:/ 'tableInsert' smart constructor.
data TableInsert = TableInsert'
    { _tiPayload :: !Table
    , _tiFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TableInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiPayload'
--
-- * 'tiFields'
tableInsert
    :: Table -- ^ 'tiPayload'
    -> TableInsert
tableInsert pTiPayload_ = 
    TableInsert'
    { _tiPayload = pTiPayload_
    , _tiFields = Nothing
    }

-- | Multipart request metadata.
tiPayload :: Lens' TableInsert Table
tiPayload
  = lens _tiPayload (\ s a -> s{_tiPayload = a})

-- | Selector specifying which fields to include in a partial response.
tiFields :: Lens' TableInsert (Maybe Text)
tiFields = lens _tiFields (\ s a -> s{_tiFields = a})

instance GoogleRequest TableInsert where
        type Rs TableInsert = Table
        type Scopes TableInsert =
             '["https://www.googleapis.com/auth/fusiontables"]
        requestClient TableInsert'{..}
          = go _tiFields (Just AltJSON) _tiPayload
              fusionTablesService
          where go
                  = buildClient (Proxy :: Proxy TableInsertResource)
                      mempty
