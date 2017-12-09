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
-- Module      : Network.Google.Resource.IdentityToolkit.RelyingParty.GetPublicKeys
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get token signing public key.
--
-- /See:/ <https://developers.google.com/identity-toolkit/v3/ Google Identity Toolkit API Reference> for @identitytoolkit.relyingparty.getPublicKeys@.
module Network.Google.Resource.IdentityToolkit.RelyingParty.GetPublicKeys
    (
    -- * REST Resource
      RelyingPartyGetPublicKeysResource

    -- * Creating a Request
    , relyingPartyGetPublicKeys
    , RelyingPartyGetPublicKeys

    -- * Request Lenses
    , rpgpkFields
    ) where

import Network.Google.IdentityToolkit.Types
import Network.Google.Prelude

-- | A resource alias for @identitytoolkit.relyingparty.getPublicKeys@ method which the
-- 'RelyingPartyGetPublicKeys' request conforms to.
type RelyingPartyGetPublicKeysResource =
     "identitytoolkit" :>
       "v3" :>
         "relyingparty" :>
           "publicKeys" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 Get '[JSON]
                   IdentitytoolkitRelyingPartyGetPublicKeysResponse

-- | Get token signing public key.
--
-- /See:/ 'relyingPartyGetPublicKeys' smart constructor.
newtype RelyingPartyGetPublicKeys = RelyingPartyGetPublicKeys'
    { _rpgpkFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RelyingPartyGetPublicKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpgpkFields'
relyingPartyGetPublicKeys
    :: RelyingPartyGetPublicKeys
relyingPartyGetPublicKeys = 
    RelyingPartyGetPublicKeys'
    { _rpgpkFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
rpgpkFields :: Lens' RelyingPartyGetPublicKeys (Maybe Text)
rpgpkFields
  = lens _rpgpkFields (\ s a -> s{_rpgpkFields = a})

instance GoogleRequest RelyingPartyGetPublicKeys
         where
        type Rs RelyingPartyGetPublicKeys =
             IdentitytoolkitRelyingPartyGetPublicKeysResponse
        type Scopes RelyingPartyGetPublicKeys =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient RelyingPartyGetPublicKeys'{..}
          = go _rpgpkFields (Just AltJSON)
              identityToolkitService
          where go
                  = buildClient
                      (Proxy :: Proxy RelyingPartyGetPublicKeysResource)
                      mempty
