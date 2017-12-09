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
-- Module      : Network.Google.Resource.IdentityToolkit.RelyingParty.CreateAuthURI
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the URI used by the IdP to authenticate the user.
--
-- /See:/ <https://developers.google.com/identity-toolkit/v3/ Google Identity Toolkit API Reference> for @identitytoolkit.relyingparty.createAuthUri@.
module Network.Google.Resource.IdentityToolkit.RelyingParty.CreateAuthURI
    (
    -- * REST Resource
      RelyingPartyCreateAuthURIResource

    -- * Creating a Request
    , relyingPartyCreateAuthURI
    , RelyingPartyCreateAuthURI

    -- * Request Lenses
    , rpcauPayload
    , rpcauFields
    ) where

import Network.Google.IdentityToolkit.Types
import Network.Google.Prelude

-- | A resource alias for @identitytoolkit.relyingparty.createAuthUri@ method which the
-- 'RelyingPartyCreateAuthURI' request conforms to.
type RelyingPartyCreateAuthURIResource =
     "identitytoolkit" :>
       "v3" :>
         "relyingparty" :>
           "createAuthUri" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON]
                   IdentitytoolkitRelyingPartyCreateAuthURIRequest
                   :> Post '[JSON] CreateAuthURIResponse

-- | Creates the URI used by the IdP to authenticate the user.
--
-- /See:/ 'relyingPartyCreateAuthURI' smart constructor.
data RelyingPartyCreateAuthURI = RelyingPartyCreateAuthURI'
    { _rpcauPayload :: !IdentitytoolkitRelyingPartyCreateAuthURIRequest
    , _rpcauFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RelyingPartyCreateAuthURI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpcauPayload'
--
-- * 'rpcauFields'
relyingPartyCreateAuthURI
    :: IdentitytoolkitRelyingPartyCreateAuthURIRequest -- ^ 'rpcauPayload'
    -> RelyingPartyCreateAuthURI
relyingPartyCreateAuthURI pRpcauPayload_ = 
    RelyingPartyCreateAuthURI'
    { _rpcauPayload = pRpcauPayload_
    , _rpcauFields = Nothing
    }

-- | Multipart request metadata.
rpcauPayload :: Lens' RelyingPartyCreateAuthURI IdentitytoolkitRelyingPartyCreateAuthURIRequest
rpcauPayload
  = lens _rpcauPayload (\ s a -> s{_rpcauPayload = a})

-- | Selector specifying which fields to include in a partial response.
rpcauFields :: Lens' RelyingPartyCreateAuthURI (Maybe Text)
rpcauFields
  = lens _rpcauFields (\ s a -> s{_rpcauFields = a})

instance GoogleRequest RelyingPartyCreateAuthURI
         where
        type Rs RelyingPartyCreateAuthURI =
             CreateAuthURIResponse
        type Scopes RelyingPartyCreateAuthURI =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient RelyingPartyCreateAuthURI'{..}
          = go _rpcauFields (Just AltJSON) _rpcauPayload
              identityToolkitService
          where go
                  = buildClient
                      (Proxy :: Proxy RelyingPartyCreateAuthURIResource)
                      mempty
