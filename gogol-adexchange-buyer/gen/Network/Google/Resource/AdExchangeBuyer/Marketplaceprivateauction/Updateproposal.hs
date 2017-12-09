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
-- Module      : Network.Google.Resource.AdExchangeBuyer.Marketplaceprivateauction.Updateproposal
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a given private auction proposal
--
-- /See:/ <https://developers.google.com/ad-exchange/buyer-rest Ad Exchange Buyer API Reference> for @adexchangebuyer.marketplaceprivateauction.updateproposal@.
module Network.Google.Resource.AdExchangeBuyer.Marketplaceprivateauction.Updateproposal
    (
    -- * REST Resource
      MarketplaceprivateauctionUpdateproposalResource

    -- * Creating a Request
    , marketplaceprivateauctionUpdateproposal
    , MarketplaceprivateauctionUpdateproposal

    -- * Request Lenses
    , muPrivateAuctionId
    , muPayload
    , muFields
    ) where

import Network.Google.AdExchangeBuyer.Types
import Network.Google.Prelude

-- | A resource alias for @adexchangebuyer.marketplaceprivateauction.updateproposal@ method which the
-- 'MarketplaceprivateauctionUpdateproposal' request conforms to.
type MarketplaceprivateauctionUpdateproposalResource
     =
     "adexchangebuyer" :>
       "v1.4" :>
         "privateauction" :>
           Capture "privateAuctionId" Text :>
             "updateproposal" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] UpdatePrivateAuctionProposalRequest
                     :> Post '[JSON] ()

-- | Update a given private auction proposal
--
-- /See:/ 'marketplaceprivateauctionUpdateproposal' smart constructor.
data MarketplaceprivateauctionUpdateproposal = MarketplaceprivateauctionUpdateproposal'
    { _muPrivateAuctionId :: !Text
    , _muPayload :: !UpdatePrivateAuctionProposalRequest
    , _muFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MarketplaceprivateauctionUpdateproposal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'muPrivateAuctionId'
--
-- * 'muPayload'
--
-- * 'muFields'
marketplaceprivateauctionUpdateproposal
    :: Text -- ^ 'muPrivateAuctionId'
    -> UpdatePrivateAuctionProposalRequest -- ^ 'muPayload'
    -> MarketplaceprivateauctionUpdateproposal
marketplaceprivateauctionUpdateproposal pMuPrivateAuctionId_ pMuPayload_ = 
    MarketplaceprivateauctionUpdateproposal'
    { _muPrivateAuctionId = pMuPrivateAuctionId_
    , _muPayload = pMuPayload_
    , _muFields = Nothing
    }

-- | The private auction id to be updated.
muPrivateAuctionId :: Lens' MarketplaceprivateauctionUpdateproposal Text
muPrivateAuctionId
  = lens _muPrivateAuctionId
      (\ s a -> s{_muPrivateAuctionId = a})

-- | Multipart request metadata.
muPayload :: Lens' MarketplaceprivateauctionUpdateproposal UpdatePrivateAuctionProposalRequest
muPayload
  = lens _muPayload (\ s a -> s{_muPayload = a})

-- | Selector specifying which fields to include in a partial response.
muFields :: Lens' MarketplaceprivateauctionUpdateproposal (Maybe Text)
muFields = lens _muFields (\ s a -> s{_muFields = a})

instance GoogleRequest
         MarketplaceprivateauctionUpdateproposal where
        type Rs MarketplaceprivateauctionUpdateproposal = ()
        type Scopes MarketplaceprivateauctionUpdateproposal =
             '["https://www.googleapis.com/auth/adexchange.buyer"]
        requestClient
          MarketplaceprivateauctionUpdateproposal'{..}
          = go _muPrivateAuctionId _muFields (Just AltJSON)
              _muPayload
              adExchangeBuyerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           MarketplaceprivateauctionUpdateproposalResource)
                      mempty
