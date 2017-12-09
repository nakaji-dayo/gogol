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
-- Module      : Network.Google.Resource.AdSense.Accounts.Payments.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the payments for the specified AdSense account.
--
-- /See:/ <https://developers.google.com/adsense/management/ AdSense Management API Reference> for @adsense.accounts.payments.list@.
module Network.Google.Resource.AdSense.Accounts.Payments.List
    (
    -- * REST Resource
      AccountsPaymentsListResource

    -- * Creating a Request
    , accountsPaymentsList
    , AccountsPaymentsList

    -- * Request Lenses
    , aplAccountId
    , aplFields
    ) where

import Network.Google.AdSense.Types
import Network.Google.Prelude

-- | A resource alias for @adsense.accounts.payments.list@ method which the
-- 'AccountsPaymentsList' request conforms to.
type AccountsPaymentsListResource =
     "adsense" :>
       "v1.4" :>
         "accounts" :>
           Capture "accountId" Text :>
             "payments" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] Payments

-- | List the payments for the specified AdSense account.
--
-- /See:/ 'accountsPaymentsList' smart constructor.
data AccountsPaymentsList = AccountsPaymentsList'
    { _aplAccountId :: !Text
    , _aplFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsPaymentsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aplAccountId'
--
-- * 'aplFields'
accountsPaymentsList
    :: Text -- ^ 'aplAccountId'
    -> AccountsPaymentsList
accountsPaymentsList pAplAccountId_ = 
    AccountsPaymentsList'
    { _aplAccountId = pAplAccountId_
    , _aplFields = Nothing
    }

-- | Account for which to retrieve the payments.
aplAccountId :: Lens' AccountsPaymentsList Text
aplAccountId
  = lens _aplAccountId (\ s a -> s{_aplAccountId = a})

-- | Selector specifying which fields to include in a partial response.
aplFields :: Lens' AccountsPaymentsList (Maybe Text)
aplFields
  = lens _aplFields (\ s a -> s{_aplFields = a})

instance GoogleRequest AccountsPaymentsList where
        type Rs AccountsPaymentsList = Payments
        type Scopes AccountsPaymentsList =
             '["https://www.googleapis.com/auth/adsense",
               "https://www.googleapis.com/auth/adsense.readonly"]
        requestClient AccountsPaymentsList'{..}
          = go _aplAccountId _aplFields (Just AltJSON)
              adSenseService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsPaymentsListResource)
                      mempty
