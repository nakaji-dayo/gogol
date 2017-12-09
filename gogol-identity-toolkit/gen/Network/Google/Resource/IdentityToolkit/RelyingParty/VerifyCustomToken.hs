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
-- Module      : Network.Google.Resource.IdentityToolkit.RelyingParty.VerifyCustomToken
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies the developer asserted ID token.
--
-- /See:/ <https://developers.google.com/identity-toolkit/v3/ Google Identity Toolkit API Reference> for @identitytoolkit.relyingparty.verifyCustomToken@.
module Network.Google.Resource.IdentityToolkit.RelyingParty.VerifyCustomToken
    (
    -- * REST Resource
      RelyingPartyVerifyCustomTokenResource

    -- * Creating a Request
    , relyingPartyVerifyCustomToken
    , RelyingPartyVerifyCustomToken

    -- * Request Lenses
    , rpvctPayload
    , rpvctFields
    ) where

import Network.Google.IdentityToolkit.Types
import Network.Google.Prelude

-- | A resource alias for @identitytoolkit.relyingparty.verifyCustomToken@ method which the
-- 'RelyingPartyVerifyCustomToken' request conforms to.
type RelyingPartyVerifyCustomTokenResource =
     "identitytoolkit" :>
       "v3" :>
         "relyingparty" :>
           "verifyCustomToken" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON]
                   IdentitytoolkitRelyingPartyVerifyCustomTokenRequest
                   :> Post '[JSON] VerifyCustomTokenResponse

-- | Verifies the developer asserted ID token.
--
-- /See:/ 'relyingPartyVerifyCustomToken' smart constructor.
data RelyingPartyVerifyCustomToken = RelyingPartyVerifyCustomToken'
    { _rpvctPayload :: !IdentitytoolkitRelyingPartyVerifyCustomTokenRequest
    , _rpvctFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RelyingPartyVerifyCustomToken' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpvctPayload'
--
-- * 'rpvctFields'
relyingPartyVerifyCustomToken
    :: IdentitytoolkitRelyingPartyVerifyCustomTokenRequest -- ^ 'rpvctPayload'
    -> RelyingPartyVerifyCustomToken
relyingPartyVerifyCustomToken pRpvctPayload_ = 
    RelyingPartyVerifyCustomToken'
    { _rpvctPayload = pRpvctPayload_
    , _rpvctFields = Nothing
    }

-- | Multipart request metadata.
rpvctPayload :: Lens' RelyingPartyVerifyCustomToken IdentitytoolkitRelyingPartyVerifyCustomTokenRequest
rpvctPayload
  = lens _rpvctPayload (\ s a -> s{_rpvctPayload = a})

-- | Selector specifying which fields to include in a partial response.
rpvctFields :: Lens' RelyingPartyVerifyCustomToken (Maybe Text)
rpvctFields
  = lens _rpvctFields (\ s a -> s{_rpvctFields = a})

instance GoogleRequest RelyingPartyVerifyCustomToken
         where
        type Rs RelyingPartyVerifyCustomToken =
             VerifyCustomTokenResponse
        type Scopes RelyingPartyVerifyCustomToken =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient RelyingPartyVerifyCustomToken'{..}
          = go _rpvctFields (Just AltJSON) _rpvctPayload
              identityToolkitService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy RelyingPartyVerifyCustomTokenResource)
                      mempty
