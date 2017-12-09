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
-- Module      : Network.Google.Resource.AdSense.Accounts.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all accounts available to this AdSense account.
--
-- /See:/ <https://developers.google.com/adsense/management/ AdSense Management API Reference> for @adsense.accounts.list@.
module Network.Google.Resource.AdSense.Accounts.List
    (
    -- * REST Resource
      AccountsListResource

    -- * Creating a Request
    , accountsList
    , AccountsList

    -- * Request Lenses
    , aPageToken
    , aMaxResults
    , aFields
    ) where

import Network.Google.AdSense.Types
import Network.Google.Prelude

-- | A resource alias for @adsense.accounts.list@ method which the
-- 'AccountsList' request conforms to.
type AccountsListResource =
     "adsense" :>
       "v1.4" :>
         "accounts" :>
           QueryParam "pageToken" Text :>
             QueryParam "maxResults" (Textual Int32) :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] Accounts

-- | List all accounts available to this AdSense account.
--
-- /See:/ 'accountsList' smart constructor.
data AccountsList = AccountsList'
    { _aPageToken :: !(Maybe Text)
    , _aMaxResults :: !(Maybe (Textual Int32))
    , _aFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aPageToken'
--
-- * 'aMaxResults'
--
-- * 'aFields'
accountsList
    :: AccountsList
accountsList = 
    AccountsList'
    { _aPageToken = Nothing
    , _aMaxResults = Nothing
    , _aFields = Nothing
    }

-- | A continuation token, used to page through accounts. To retrieve the
-- next page, set this parameter to the value of \"nextPageToken\" from the
-- previous response.
aPageToken :: Lens' AccountsList (Maybe Text)
aPageToken
  = lens _aPageToken (\ s a -> s{_aPageToken = a})

-- | The maximum number of accounts to include in the response, used for
-- paging.
aMaxResults :: Lens' AccountsList (Maybe Int32)
aMaxResults
  = lens _aMaxResults (\ s a -> s{_aMaxResults = a}) .
      mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
aFields :: Lens' AccountsList (Maybe Text)
aFields = lens _aFields (\ s a -> s{_aFields = a})

instance GoogleRequest AccountsList where
        type Rs AccountsList = Accounts
        type Scopes AccountsList =
             '["https://www.googleapis.com/auth/adsense",
               "https://www.googleapis.com/auth/adsense.readonly"]
        requestClient AccountsList'{..}
          = go _aPageToken _aMaxResults _aFields (Just AltJSON)
              adSenseService
          where go
                  = buildClient (Proxy :: Proxy AccountsListResource)
                      mempty
