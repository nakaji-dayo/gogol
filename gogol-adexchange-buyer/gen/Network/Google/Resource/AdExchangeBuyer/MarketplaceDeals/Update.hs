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
-- Module      : Network.Google.Resource.AdExchangeBuyer.MarketplaceDeals.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces all the deals in the proposal with the passed in deals
--
-- /See:/ <https://developers.google.com/ad-exchange/buyer-rest Ad Exchange Buyer API Reference> for @adexchangebuyer.marketplacedeals.update@.
module Network.Google.Resource.AdExchangeBuyer.MarketplaceDeals.Update
    (
    -- * REST Resource
      MarketplaceDealsUpdateResource

    -- * Creating a Request
    , marketplaceDealsUpdate
    , MarketplaceDealsUpdate

    -- * Request Lenses
    , mduPayload
    , mduProposalId
    , mduFields
    ) where

import Network.Google.AdExchangeBuyer.Types
import Network.Google.Prelude

-- | A resource alias for @adexchangebuyer.marketplacedeals.update@ method which the
-- 'MarketplaceDealsUpdate' request conforms to.
type MarketplaceDealsUpdateResource =
     "adexchangebuyer" :>
       "v1.4" :>
         "proposals" :>
           Capture "proposalId" Text :>
             "deals" :>
               "update" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] EditAllOrderDealsRequest :>
                       Post '[JSON] EditAllOrderDealsResponse

-- | Replaces all the deals in the proposal with the passed in deals
--
-- /See:/ 'marketplaceDealsUpdate' smart constructor.
data MarketplaceDealsUpdate = MarketplaceDealsUpdate'
    { _mduPayload :: !EditAllOrderDealsRequest
    , _mduProposalId :: !Text
    , _mduFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MarketplaceDealsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mduPayload'
--
-- * 'mduProposalId'
--
-- * 'mduFields'
marketplaceDealsUpdate
    :: EditAllOrderDealsRequest -- ^ 'mduPayload'
    -> Text -- ^ 'mduProposalId'
    -> MarketplaceDealsUpdate
marketplaceDealsUpdate pMduPayload_ pMduProposalId_ = 
    MarketplaceDealsUpdate'
    { _mduPayload = pMduPayload_
    , _mduProposalId = pMduProposalId_
    , _mduFields = Nothing
    }

-- | Multipart request metadata.
mduPayload :: Lens' MarketplaceDealsUpdate EditAllOrderDealsRequest
mduPayload
  = lens _mduPayload (\ s a -> s{_mduPayload = a})

-- | The proposalId to edit deals on.
mduProposalId :: Lens' MarketplaceDealsUpdate Text
mduProposalId
  = lens _mduProposalId
      (\ s a -> s{_mduProposalId = a})

-- | Selector specifying which fields to include in a partial response.
mduFields :: Lens' MarketplaceDealsUpdate (Maybe Text)
mduFields
  = lens _mduFields (\ s a -> s{_mduFields = a})

instance GoogleRequest MarketplaceDealsUpdate where
        type Rs MarketplaceDealsUpdate =
             EditAllOrderDealsResponse
        type Scopes MarketplaceDealsUpdate =
             '["https://www.googleapis.com/auth/adexchange.buyer"]
        requestClient MarketplaceDealsUpdate'{..}
          = go _mduProposalId _mduFields (Just AltJSON)
              _mduPayload
              adExchangeBuyerService
          where go
                  = buildClient
                      (Proxy :: Proxy MarketplaceDealsUpdateResource)
                      mempty
