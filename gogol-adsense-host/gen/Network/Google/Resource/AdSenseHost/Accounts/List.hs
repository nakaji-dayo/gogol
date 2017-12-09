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
-- Module      : Network.Google.Resource.AdSenseHost.Accounts.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List hosted accounts associated with this AdSense account by ad client
-- id.
--
-- /See:/ <https://developers.google.com/adsense/host/ AdSense Host API Reference> for @adsensehost.accounts.list@.
module Network.Google.Resource.AdSenseHost.Accounts.List
    (
    -- * REST Resource
      AccountsListResource

    -- * Creating a Request
    , accountsList
    , AccountsList

    -- * Request Lenses
    , alFilterAdClientId
    , alFields
    ) where

import Network.Google.AdSenseHost.Types
import Network.Google.Prelude

-- | A resource alias for @adsensehost.accounts.list@ method which the
-- 'AccountsList' request conforms to.
type AccountsListResource =
     "adsensehost" :>
       "v4.1" :>
         "accounts" :>
           QueryParams "filterAdClientId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] Accounts

-- | List hosted accounts associated with this AdSense account by ad client
-- id.
--
-- /See:/ 'accountsList' smart constructor.
data AccountsList = AccountsList'
    { _alFilterAdClientId :: ![Text]
    , _alFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alFilterAdClientId'
--
-- * 'alFields'
accountsList
    :: [Text] -- ^ 'alFilterAdClientId'
    -> AccountsList
accountsList pAlFilterAdClientId_ = 
    AccountsList'
    { _alFilterAdClientId = _Coerce # pAlFilterAdClientId_
    , _alFields = Nothing
    }

-- | Ad clients to list accounts for.
alFilterAdClientId :: Lens' AccountsList [Text]
alFilterAdClientId
  = lens _alFilterAdClientId
      (\ s a -> s{_alFilterAdClientId = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
alFields :: Lens' AccountsList (Maybe Text)
alFields = lens _alFields (\ s a -> s{_alFields = a})

instance GoogleRequest AccountsList where
        type Rs AccountsList = Accounts
        type Scopes AccountsList =
             '["https://www.googleapis.com/auth/adsensehost"]
        requestClient AccountsList'{..}
          = go _alFilterAdClientId _alFields (Just AltJSON)
              adSenseHostService
          where go
                  = buildClient (Proxy :: Proxy AccountsListResource)
                      mempty
