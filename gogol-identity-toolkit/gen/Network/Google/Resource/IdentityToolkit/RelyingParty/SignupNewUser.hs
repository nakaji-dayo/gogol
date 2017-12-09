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
-- Module      : Network.Google.Resource.IdentityToolkit.RelyingParty.SignupNewUser
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signup new user.
--
-- /See:/ <https://developers.google.com/identity-toolkit/v3/ Google Identity Toolkit API Reference> for @identitytoolkit.relyingparty.signupNewUser@.
module Network.Google.Resource.IdentityToolkit.RelyingParty.SignupNewUser
    (
    -- * REST Resource
      RelyingPartySignupNewUserResource

    -- * Creating a Request
    , relyingPartySignupNewUser
    , RelyingPartySignupNewUser

    -- * Request Lenses
    , rpsnuPayload
    , rpsnuFields
    ) where

import Network.Google.IdentityToolkit.Types
import Network.Google.Prelude

-- | A resource alias for @identitytoolkit.relyingparty.signupNewUser@ method which the
-- 'RelyingPartySignupNewUser' request conforms to.
type RelyingPartySignupNewUserResource =
     "identitytoolkit" :>
       "v3" :>
         "relyingparty" :>
           "signupNewUser" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON]
                   IdentitytoolkitRelyingPartySignupNewUserRequest
                   :> Post '[JSON] SignupNewUserResponse

-- | Signup new user.
--
-- /See:/ 'relyingPartySignupNewUser' smart constructor.
data RelyingPartySignupNewUser = RelyingPartySignupNewUser'
    { _rpsnuPayload :: !IdentitytoolkitRelyingPartySignupNewUserRequest
    , _rpsnuFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RelyingPartySignupNewUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpsnuPayload'
--
-- * 'rpsnuFields'
relyingPartySignupNewUser
    :: IdentitytoolkitRelyingPartySignupNewUserRequest -- ^ 'rpsnuPayload'
    -> RelyingPartySignupNewUser
relyingPartySignupNewUser pRpsnuPayload_ = 
    RelyingPartySignupNewUser'
    { _rpsnuPayload = pRpsnuPayload_
    , _rpsnuFields = Nothing
    }

-- | Multipart request metadata.
rpsnuPayload :: Lens' RelyingPartySignupNewUser IdentitytoolkitRelyingPartySignupNewUserRequest
rpsnuPayload
  = lens _rpsnuPayload (\ s a -> s{_rpsnuPayload = a})

-- | Selector specifying which fields to include in a partial response.
rpsnuFields :: Lens' RelyingPartySignupNewUser (Maybe Text)
rpsnuFields
  = lens _rpsnuFields (\ s a -> s{_rpsnuFields = a})

instance GoogleRequest RelyingPartySignupNewUser
         where
        type Rs RelyingPartySignupNewUser =
             SignupNewUserResponse
        type Scopes RelyingPartySignupNewUser =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient RelyingPartySignupNewUser'{..}
          = go _rpsnuFields (Just AltJSON) _rpsnuPayload
              identityToolkitService
          where go
                  = buildClient
                      (Proxy :: Proxy RelyingPartySignupNewUserResource)
                      mempty
