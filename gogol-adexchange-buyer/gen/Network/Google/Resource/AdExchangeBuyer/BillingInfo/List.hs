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
-- Module      : Network.Google.Resource.AdExchangeBuyer.BillingInfo.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of billing information for all accounts of the
-- authenticated user.
--
-- /See:/ <https://developers.google.com/ad-exchange/buyer-rest Ad Exchange Buyer API Reference> for @adexchangebuyer.billingInfo.list@.
module Network.Google.Resource.AdExchangeBuyer.BillingInfo.List
    (
    -- * REST Resource
      BillingInfoListResource

    -- * Creating a Request
    , billingInfoList'
    , BillingInfoList'

    -- * Request Lenses
    , bilFields
    ) where

import Network.Google.AdExchangeBuyer.Types
import Network.Google.Prelude

-- | A resource alias for @adexchangebuyer.billingInfo.list@ method which the
-- 'BillingInfoList'' request conforms to.
type BillingInfoListResource =
     "adexchangebuyer" :>
       "v1.4" :>
         "billinginfo" :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :>
               Get '[JSON] BillingInfoList

-- | Retrieves a list of billing information for all accounts of the
-- authenticated user.
--
-- /See:/ 'billingInfoList'' smart constructor.
newtype BillingInfoList' = BillingInfoList''
    { _bilFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BillingInfoList'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bilFields'
billingInfoList'
    :: BillingInfoList'
billingInfoList' = 
    BillingInfoList''
    { _bilFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
bilFields :: Lens' BillingInfoList' (Maybe Text)
bilFields
  = lens _bilFields (\ s a -> s{_bilFields = a})

instance GoogleRequest BillingInfoList' where
        type Rs BillingInfoList' = BillingInfoList
        type Scopes BillingInfoList' =
             '["https://www.googleapis.com/auth/adexchange.buyer"]
        requestClient BillingInfoList''{..}
          = go _bilFields (Just AltJSON) adExchangeBuyerService
          where go
                  = buildClient
                      (Proxy :: Proxy BillingInfoListResource)
                      mempty
