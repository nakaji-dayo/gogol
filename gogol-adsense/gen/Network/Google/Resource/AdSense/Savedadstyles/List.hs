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
-- Module      : Network.Google.Resource.AdSense.Savedadstyles.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all saved ad styles in the user\'s account.
--
-- /See:/ <https://developers.google.com/adsense/management/ AdSense Management API Reference> for @adsense.savedadstyles.list@.
module Network.Google.Resource.AdSense.Savedadstyles.List
    (
    -- * REST Resource
      SavedadstylesListResource

    -- * Creating a Request
    , savedadstylesList
    , SavedadstylesList

    -- * Request Lenses
    , slPageToken
    , slMaxResults
    , slFields
    ) where

import Network.Google.AdSense.Types
import Network.Google.Prelude

-- | A resource alias for @adsense.savedadstyles.list@ method which the
-- 'SavedadstylesList' request conforms to.
type SavedadstylesListResource =
     "adsense" :>
       "v1.4" :>
         "savedadstyles" :>
           QueryParam "pageToken" Text :>
             QueryParam "maxResults" (Textual Int32) :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] SavedAdStyles

-- | List all saved ad styles in the user\'s account.
--
-- /See:/ 'savedadstylesList' smart constructor.
data SavedadstylesList = SavedadstylesList'
    { _slPageToken :: !(Maybe Text)
    , _slMaxResults :: !(Maybe (Textual Int32))
    , _slFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SavedadstylesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slPageToken'
--
-- * 'slMaxResults'
--
-- * 'slFields'
savedadstylesList
    :: SavedadstylesList
savedadstylesList = 
    SavedadstylesList'
    { _slPageToken = Nothing
    , _slMaxResults = Nothing
    , _slFields = Nothing
    }

-- | A continuation token, used to page through saved ad styles. To retrieve
-- the next page, set this parameter to the value of \"nextPageToken\" from
-- the previous response.
slPageToken :: Lens' SavedadstylesList (Maybe Text)
slPageToken
  = lens _slPageToken (\ s a -> s{_slPageToken = a})

-- | The maximum number of saved ad styles to include in the response, used
-- for paging.
slMaxResults :: Lens' SavedadstylesList (Maybe Int32)
slMaxResults
  = lens _slMaxResults (\ s a -> s{_slMaxResults = a})
      . mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
slFields :: Lens' SavedadstylesList (Maybe Text)
slFields = lens _slFields (\ s a -> s{_slFields = a})

instance GoogleRequest SavedadstylesList where
        type Rs SavedadstylesList = SavedAdStyles
        type Scopes SavedadstylesList =
             '["https://www.googleapis.com/auth/adsense",
               "https://www.googleapis.com/auth/adsense.readonly"]
        requestClient SavedadstylesList'{..}
          = go _slPageToken _slMaxResults _slFields
              (Just AltJSON)
              adSenseService
          where go
                  = buildClient
                      (Proxy :: Proxy SavedadstylesListResource)
                      mempty
