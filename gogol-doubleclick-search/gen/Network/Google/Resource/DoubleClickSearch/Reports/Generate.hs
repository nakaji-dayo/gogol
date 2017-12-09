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
-- Module      : Network.Google.Resource.DoubleClickSearch.Reports.Generate
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates and returns a report immediately.
--
-- /See:/ <https://developers.google.com/doubleclick-search/ DoubleClick Search API Reference> for @doubleclicksearch.reports.generate@.
module Network.Google.Resource.DoubleClickSearch.Reports.Generate
    (
    -- * REST Resource
      ReportsGenerateResource

    -- * Creating a Request
    , reportsGenerate
    , ReportsGenerate

    -- * Request Lenses
    , rPayload
    , rFields
    ) where

import Network.Google.DoubleClickSearch.Types
import Network.Google.Prelude

-- | A resource alias for @doubleclicksearch.reports.generate@ method which the
-- 'ReportsGenerate' request conforms to.
type ReportsGenerateResource =
     "doubleclicksearch" :>
       "v2" :>
         "reports" :>
           "generate" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] ReportRequest :> Post '[JSON] Report

-- | Generates and returns a report immediately.
--
-- /See:/ 'reportsGenerate' smart constructor.
data ReportsGenerate = ReportsGenerate'
    { _rPayload :: !ReportRequest
    , _rFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReportsGenerate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rPayload'
--
-- * 'rFields'
reportsGenerate
    :: ReportRequest -- ^ 'rPayload'
    -> ReportsGenerate
reportsGenerate pRPayload_ = 
    ReportsGenerate'
    { _rPayload = pRPayload_
    , _rFields = Nothing
    }

-- | Multipart request metadata.
rPayload :: Lens' ReportsGenerate ReportRequest
rPayload = lens _rPayload (\ s a -> s{_rPayload = a})

-- | Selector specifying which fields to include in a partial response.
rFields :: Lens' ReportsGenerate (Maybe Text)
rFields = lens _rFields (\ s a -> s{_rFields = a})

instance GoogleRequest ReportsGenerate where
        type Rs ReportsGenerate = Report
        type Scopes ReportsGenerate =
             '["https://www.googleapis.com/auth/doubleclicksearch"]
        requestClient ReportsGenerate'{..}
          = go _rFields (Just AltJSON) _rPayload
              doubleClickSearchService
          where go
                  = buildClient
                      (Proxy :: Proxy ReportsGenerateResource)
                      mempty
