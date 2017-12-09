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
-- Module      : Network.Google.Resource.AdSenseHost.Accounts.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about the selected associated AdSense account.
--
-- /See:/ <https://developers.google.com/adsense/host/ AdSense Host API Reference> for @adsensehost.accounts.get@.
module Network.Google.Resource.AdSenseHost.Accounts.Get
    (
    -- * REST Resource
      AccountsGetResource

    -- * Creating a Request
    , accountsGet
    , AccountsGet

    -- * Request Lenses
    , agAccountId
    , agFields
    ) where

import Network.Google.AdSenseHost.Types
import Network.Google.Prelude

-- | A resource alias for @adsensehost.accounts.get@ method which the
-- 'AccountsGet' request conforms to.
type AccountsGetResource =
     "adsensehost" :>
       "v4.1" :>
         "accounts" :>
           Capture "accountId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] Account

-- | Get information about the selected associated AdSense account.
--
-- /See:/ 'accountsGet' smart constructor.
data AccountsGet = AccountsGet'
    { _agAccountId :: !Text
    , _agFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agAccountId'
--
-- * 'agFields'
accountsGet
    :: Text -- ^ 'agAccountId'
    -> AccountsGet
accountsGet pAgAccountId_ = 
    AccountsGet'
    { _agAccountId = pAgAccountId_
    , _agFields = Nothing
    }

-- | Account to get information about.
agAccountId :: Lens' AccountsGet Text
agAccountId
  = lens _agAccountId (\ s a -> s{_agAccountId = a})

-- | Selector specifying which fields to include in a partial response.
agFields :: Lens' AccountsGet (Maybe Text)
agFields = lens _agFields (\ s a -> s{_agFields = a})

instance GoogleRequest AccountsGet where
        type Rs AccountsGet = Account
        type Scopes AccountsGet =
             '["https://www.googleapis.com/auth/adsensehost"]
        requestClient AccountsGet'{..}
          = go _agAccountId _agFields (Just AltJSON)
              adSenseHostService
          where go
                  = buildClient (Proxy :: Proxy AccountsGetResource)
                      mempty
