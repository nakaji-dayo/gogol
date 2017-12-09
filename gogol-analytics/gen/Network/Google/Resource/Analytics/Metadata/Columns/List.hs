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
-- Module      : Network.Google.Resource.Analytics.Metadata.Columns.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all columns for a report type
--
-- /See:/ <https://developers.google.com/analytics/ Google Analytics API Reference> for @analytics.metadata.columns.list@.
module Network.Google.Resource.Analytics.Metadata.Columns.List
    (
    -- * REST Resource
      MetadataColumnsListResource

    -- * Creating a Request
    , metadataColumnsList
    , MetadataColumnsList

    -- * Request Lenses
    , mclReportType
    , mclFields
    ) where

import Network.Google.Analytics.Types
import Network.Google.Prelude

-- | A resource alias for @analytics.metadata.columns.list@ method which the
-- 'MetadataColumnsList' request conforms to.
type MetadataColumnsListResource =
     "analytics" :>
       "v3" :>
         "metadata" :>
           Capture "reportType" Text :>
             "columns" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] Columns

-- | Lists all columns for a report type
--
-- /See:/ 'metadataColumnsList' smart constructor.
data MetadataColumnsList = MetadataColumnsList'
    { _mclReportType :: !Text
    , _mclFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetadataColumnsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mclReportType'
--
-- * 'mclFields'
metadataColumnsList
    :: Text -- ^ 'mclReportType'
    -> MetadataColumnsList
metadataColumnsList pMclReportType_ = 
    MetadataColumnsList'
    { _mclReportType = pMclReportType_
    , _mclFields = Nothing
    }

-- | Report type. Allowed Values: \'ga\'. Where \'ga\' corresponds to the
-- Core Reporting API
mclReportType :: Lens' MetadataColumnsList Text
mclReportType
  = lens _mclReportType
      (\ s a -> s{_mclReportType = a})

-- | Selector specifying which fields to include in a partial response.
mclFields :: Lens' MetadataColumnsList (Maybe Text)
mclFields
  = lens _mclFields (\ s a -> s{_mclFields = a})

instance GoogleRequest MetadataColumnsList where
        type Rs MetadataColumnsList = Columns
        type Scopes MetadataColumnsList =
             '["https://www.googleapis.com/auth/analytics",
               "https://www.googleapis.com/auth/analytics.edit",
               "https://www.googleapis.com/auth/analytics.readonly"]
        requestClient MetadataColumnsList'{..}
          = go _mclReportType _mclFields (Just AltJSON)
              analyticsService
          where go
                  = buildClient
                      (Proxy :: Proxy MetadataColumnsListResource)
                      mempty
