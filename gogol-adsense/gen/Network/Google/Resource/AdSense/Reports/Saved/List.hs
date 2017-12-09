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
-- Module      : Network.Google.Resource.AdSense.Reports.Saved.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all saved reports in this AdSense account.
--
-- /See:/ <https://developers.google.com/adsense/management/ AdSense Management API Reference> for @adsense.reports.saved.list@.
module Network.Google.Resource.AdSense.Reports.Saved.List
    (
    -- * REST Resource
      ReportsSavedListResource

    -- * Creating a Request
    , reportsSavedList
    , ReportsSavedList

    -- * Request Lenses
    , rslPageToken
    , rslMaxResults
    , rslFields
    ) where

import Network.Google.AdSense.Types
import Network.Google.Prelude

-- | A resource alias for @adsense.reports.saved.list@ method which the
-- 'ReportsSavedList' request conforms to.
type ReportsSavedListResource =
     "adsense" :>
       "v1.4" :>
         "reports" :>
           "saved" :>
             QueryParam "pageToken" Text :>
               QueryParam "maxResults" (Textual Int32) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] SavedReports

-- | List all saved reports in this AdSense account.
--
-- /See:/ 'reportsSavedList' smart constructor.
data ReportsSavedList = ReportsSavedList'
    { _rslPageToken :: !(Maybe Text)
    , _rslMaxResults :: !(Maybe (Textual Int32))
    , _rslFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReportsSavedList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rslPageToken'
--
-- * 'rslMaxResults'
--
-- * 'rslFields'
reportsSavedList
    :: ReportsSavedList
reportsSavedList = 
    ReportsSavedList'
    { _rslPageToken = Nothing
    , _rslMaxResults = Nothing
    , _rslFields = Nothing
    }

-- | A continuation token, used to page through saved reports. To retrieve
-- the next page, set this parameter to the value of \"nextPageToken\" from
-- the previous response.
rslPageToken :: Lens' ReportsSavedList (Maybe Text)
rslPageToken
  = lens _rslPageToken (\ s a -> s{_rslPageToken = a})

-- | The maximum number of saved reports to include in the response, used for
-- paging.
rslMaxResults :: Lens' ReportsSavedList (Maybe Int32)
rslMaxResults
  = lens _rslMaxResults
      (\ s a -> s{_rslMaxResults = a})
      . mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
rslFields :: Lens' ReportsSavedList (Maybe Text)
rslFields
  = lens _rslFields (\ s a -> s{_rslFields = a})

instance GoogleRequest ReportsSavedList where
        type Rs ReportsSavedList = SavedReports
        type Scopes ReportsSavedList =
             '["https://www.googleapis.com/auth/adsense",
               "https://www.googleapis.com/auth/adsense.readonly"]
        requestClient ReportsSavedList'{..}
          = go _rslPageToken _rslMaxResults _rslFields
              (Just AltJSON)
              adSenseService
          where go
                  = buildClient
                      (Proxy :: Proxy ReportsSavedListResource)
                      mempty
