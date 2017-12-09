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
-- Module      : Network.Google.Resource.AdExchangeSeller.Accounts.Metadata.Metrics.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the metadata for the metrics available to this AdExchange account.
--
-- /See:/ <https://developers.google.com/ad-exchange/seller-rest/ Ad Exchange Seller API Reference> for @adexchangeseller.accounts.metadata.metrics.list@.
module Network.Google.Resource.AdExchangeSeller.Accounts.Metadata.Metrics.List
    (
    -- * REST Resource
      AccountsMetadataMetricsListResource

    -- * Creating a Request
    , accountsMetadataMetricsList
    , AccountsMetadataMetricsList

    -- * Request Lenses
    , ammlAccountId
    , ammlFields
    ) where

import Network.Google.AdExchangeSeller.Types
import Network.Google.Prelude

-- | A resource alias for @adexchangeseller.accounts.metadata.metrics.list@ method which the
-- 'AccountsMetadataMetricsList' request conforms to.
type AccountsMetadataMetricsListResource =
     "adexchangeseller" :>
       "v2.0" :>
         "accounts" :>
           Capture "accountId" Text :>
             "metadata" :>
               "metrics" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] Metadata

-- | List the metadata for the metrics available to this AdExchange account.
--
-- /See:/ 'accountsMetadataMetricsList' smart constructor.
data AccountsMetadataMetricsList = AccountsMetadataMetricsList'
    { _ammlAccountId :: !Text
    , _ammlFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsMetadataMetricsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ammlAccountId'
--
-- * 'ammlFields'
accountsMetadataMetricsList
    :: Text -- ^ 'ammlAccountId'
    -> AccountsMetadataMetricsList
accountsMetadataMetricsList pAmmlAccountId_ = 
    AccountsMetadataMetricsList'
    { _ammlAccountId = pAmmlAccountId_
    , _ammlFields = Nothing
    }

-- | Account with visibility to the metrics.
ammlAccountId :: Lens' AccountsMetadataMetricsList Text
ammlAccountId
  = lens _ammlAccountId
      (\ s a -> s{_ammlAccountId = a})

-- | Selector specifying which fields to include in a partial response.
ammlFields :: Lens' AccountsMetadataMetricsList (Maybe Text)
ammlFields
  = lens _ammlFields (\ s a -> s{_ammlFields = a})

instance GoogleRequest AccountsMetadataMetricsList
         where
        type Rs AccountsMetadataMetricsList = Metadata
        type Scopes AccountsMetadataMetricsList =
             '["https://www.googleapis.com/auth/adexchange.seller",
               "https://www.googleapis.com/auth/adexchange.seller.readonly"]
        requestClient AccountsMetadataMetricsList'{..}
          = go _ammlAccountId _ammlFields (Just AltJSON)
              adExchangeSellerService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsMetadataMetricsListResource)
                      mempty
