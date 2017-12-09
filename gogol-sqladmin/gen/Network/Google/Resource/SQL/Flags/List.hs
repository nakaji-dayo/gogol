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
-- Module      : Network.Google.Resource.SQL.Flags.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all available database flags for Google Cloud SQL instances.
--
-- /See:/ <https://cloud.google.com/sql/docs/reference/latest Cloud SQL Administration API Reference> for @sql.flags.list@.
module Network.Google.Resource.SQL.Flags.List
    (
    -- * REST Resource
      FlagsListResource

    -- * Creating a Request
    , flagsList
    , FlagsList

    -- * Request Lenses
    , flDatabaseVersion
    , flFields
    ) where

import Network.Google.Prelude
import Network.Google.SQLAdmin.Types

-- | A resource alias for @sql.flags.list@ method which the
-- 'FlagsList' request conforms to.
type FlagsListResource =
     "sql" :>
       "v1beta4" :>
         "flags" :>
           QueryParam "databaseVersion" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 Get '[JSON] FlagsListResponse

-- | List all available database flags for Google Cloud SQL instances.
--
-- /See:/ 'flagsList' smart constructor.
data FlagsList = FlagsList'
    { _flDatabaseVersion :: !(Maybe Text)
    , _flFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FlagsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flDatabaseVersion'
--
-- * 'flFields'
flagsList
    :: FlagsList
flagsList = 
    FlagsList'
    { _flDatabaseVersion = Nothing
    , _flFields = Nothing
    }

-- | Database version for flag retrieval. Flags are specific to the database
-- version.
flDatabaseVersion :: Lens' FlagsList (Maybe Text)
flDatabaseVersion
  = lens _flDatabaseVersion
      (\ s a -> s{_flDatabaseVersion = a})

-- | Selector specifying which fields to include in a partial response.
flFields :: Lens' FlagsList (Maybe Text)
flFields = lens _flFields (\ s a -> s{_flFields = a})

instance GoogleRequest FlagsList where
        type Rs FlagsList = FlagsListResponse
        type Scopes FlagsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/sqlservice.admin"]
        requestClient FlagsList'{..}
          = go _flDatabaseVersion _flFields (Just AltJSON)
              sQLAdminService
          where go
                  = buildClient (Proxy :: Proxy FlagsListResource)
                      mempty
