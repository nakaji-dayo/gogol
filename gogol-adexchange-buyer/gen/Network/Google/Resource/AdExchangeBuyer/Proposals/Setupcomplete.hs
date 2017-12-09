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
-- Module      : Network.Google.Resource.AdExchangeBuyer.Proposals.Setupcomplete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the given proposal to indicate that setup has been completed.
--
-- /See:/ <https://developers.google.com/ad-exchange/buyer-rest Ad Exchange Buyer API Reference> for @adexchangebuyer.proposals.setupcomplete@.
module Network.Google.Resource.AdExchangeBuyer.Proposals.Setupcomplete
    (
    -- * REST Resource
      ProposalsSetupcompleteResource

    -- * Creating a Request
    , proposalsSetupcomplete
    , ProposalsSetupcomplete

    -- * Request Lenses
    , proProposalId
    , proFields
    ) where

import Network.Google.AdExchangeBuyer.Types
import Network.Google.Prelude

-- | A resource alias for @adexchangebuyer.proposals.setupcomplete@ method which the
-- 'ProposalsSetupcomplete' request conforms to.
type ProposalsSetupcompleteResource =
     "adexchangebuyer" :>
       "v1.4" :>
         "proposals" :>
           Capture "proposalId" Text :>
             "setupcomplete" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Post '[JSON] ()

-- | Update the given proposal to indicate that setup has been completed.
--
-- /See:/ 'proposalsSetupcomplete' smart constructor.
data ProposalsSetupcomplete = ProposalsSetupcomplete'
    { _proProposalId :: !Text
    , _proFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProposalsSetupcomplete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'proProposalId'
--
-- * 'proFields'
proposalsSetupcomplete
    :: Text -- ^ 'proProposalId'
    -> ProposalsSetupcomplete
proposalsSetupcomplete pProProposalId_ = 
    ProposalsSetupcomplete'
    { _proProposalId = pProProposalId_
    , _proFields = Nothing
    }

-- | The proposal id for which the setup is complete
proProposalId :: Lens' ProposalsSetupcomplete Text
proProposalId
  = lens _proProposalId
      (\ s a -> s{_proProposalId = a})

-- | Selector specifying which fields to include in a partial response.
proFields :: Lens' ProposalsSetupcomplete (Maybe Text)
proFields
  = lens _proFields (\ s a -> s{_proFields = a})

instance GoogleRequest ProposalsSetupcomplete where
        type Rs ProposalsSetupcomplete = ()
        type Scopes ProposalsSetupcomplete =
             '["https://www.googleapis.com/auth/adexchange.buyer"]
        requestClient ProposalsSetupcomplete'{..}
          = go _proProposalId _proFields (Just AltJSON)
              adExchangeBuyerService
          where go
                  = buildClient
                      (Proxy :: Proxy ProposalsSetupcompleteResource)
                      mempty
