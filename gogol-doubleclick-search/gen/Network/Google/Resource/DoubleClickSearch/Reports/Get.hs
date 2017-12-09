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
-- Module      : Network.Google.Resource.DoubleClickSearch.Reports.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Polls for the status of a report request.
--
-- /See:/ <https://developers.google.com/doubleclick-search/ DoubleClick Search API Reference> for @doubleclicksearch.reports.get@.
module Network.Google.Resource.DoubleClickSearch.Reports.Get
    (
    -- * REST Resource
      ReportsGetResource

    -- * Creating a Request
    , reportsGet
    , ReportsGet

    -- * Request Lenses
    , rgReportId
    , rgFields
    ) where

import Network.Google.DoubleClickSearch.Types
import Network.Google.Prelude

-- | A resource alias for @doubleclicksearch.reports.get@ method which the
-- 'ReportsGet' request conforms to.
type ReportsGetResource =
     "doubleclicksearch" :>
       "v2" :>
         "reports" :>
           Capture "reportId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] Report

-- | Polls for the status of a report request.
--
-- /See:/ 'reportsGet' smart constructor.
data ReportsGet = ReportsGet'
    { _rgReportId :: !Text
    , _rgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReportsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgReportId'
--
-- * 'rgFields'
reportsGet
    :: Text -- ^ 'rgReportId'
    -> ReportsGet
reportsGet pRgReportId_ = 
    ReportsGet'
    { _rgReportId = pRgReportId_
    , _rgFields = Nothing
    }

-- | ID of the report request being polled.
rgReportId :: Lens' ReportsGet Text
rgReportId
  = lens _rgReportId (\ s a -> s{_rgReportId = a})

-- | Selector specifying which fields to include in a partial response.
rgFields :: Lens' ReportsGet (Maybe Text)
rgFields = lens _rgFields (\ s a -> s{_rgFields = a})

instance GoogleRequest ReportsGet where
        type Rs ReportsGet = Report
        type Scopes ReportsGet =
             '["https://www.googleapis.com/auth/doubleclicksearch"]
        requestClient ReportsGet'{..}
          = go _rgReportId _rgFields (Just AltJSON)
              doubleClickSearchService
          where go
                  = buildClient (Proxy :: Proxy ReportsGetResource)
                      mempty
