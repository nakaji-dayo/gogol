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
-- Module      : Network.Google.Resource.DFAReporting.Browsers.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of browsers.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.browsers.list@.
module Network.Google.Resource.DFAReporting.Browsers.List
    (
    -- * REST Resource
      BrowsersListResource

    -- * Creating a Request
    , browsersList
    , BrowsersList

    -- * Request Lenses
    , blProFileId
    , blFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.browsers.list@ method which the
-- 'BrowsersList' request conforms to.
type BrowsersListResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "browsers" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   Get '[JSON] BrowsersListResponse

-- | Retrieves a list of browsers.
--
-- /See:/ 'browsersList' smart constructor.
data BrowsersList = BrowsersList'
    { _blProFileId :: !(Textual Int64)
    , _blFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BrowsersList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blProFileId'
--
-- * 'blFields'
browsersList
    :: Int64 -- ^ 'blProFileId'
    -> BrowsersList
browsersList pBlProFileId_ = 
    BrowsersList'
    { _blProFileId = _Coerce # pBlProFileId_
    , _blFields = Nothing
    }

-- | User profile ID associated with this request.
blProFileId :: Lens' BrowsersList Int64
blProFileId
  = lens _blProFileId (\ s a -> s{_blProFileId = a}) .
      _Coerce

-- | Selector specifying which fields to include in a partial response.
blFields :: Lens' BrowsersList (Maybe Text)
blFields = lens _blFields (\ s a -> s{_blFields = a})

instance GoogleRequest BrowsersList where
        type Rs BrowsersList = BrowsersListResponse
        type Scopes BrowsersList =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient BrowsersList'{..}
          = go _blProFileId _blFields (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient (Proxy :: Proxy BrowsersListResource)
                      mempty
