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
-- Module      : Network.Google.Resource.OAuth2.UserInfo.V2.Me.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /See:/ <https://developers.google.com/accounts/docs/OAuth2 Google OAuth2 API Reference> for @oauth2.userinfo.v2.me.get@.
module Network.Google.Resource.OAuth2.UserInfo.V2.Me.Get
    (
    -- * REST Resource
      UserInfoV2MeGetResource

    -- * Creating a Request
    , userInfoV2MeGet
    , UserInfoV2MeGet

    -- * Request Lenses
    , uivmgFields
    ) where

import Network.Google.OAuth2.Types
import Network.Google.Prelude

-- | A resource alias for @oauth2.userinfo.v2.me.get@ method which the
-- 'UserInfoV2MeGet' request conforms to.
type UserInfoV2MeGetResource =
     "userinfo" :>
       "v2" :>
         "me" :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :> Get '[JSON] UserInfoplus

--
-- /See:/ 'userInfoV2MeGet' smart constructor.
newtype UserInfoV2MeGet = UserInfoV2MeGet'
    { _uivmgFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserInfoV2MeGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uivmgFields'
userInfoV2MeGet
    :: UserInfoV2MeGet
userInfoV2MeGet = 
    UserInfoV2MeGet'
    { _uivmgFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
uivmgFields :: Lens' UserInfoV2MeGet (Maybe Text)
uivmgFields
  = lens _uivmgFields (\ s a -> s{_uivmgFields = a})

instance GoogleRequest UserInfoV2MeGet where
        type Rs UserInfoV2MeGet = UserInfoplus
        type Scopes UserInfoV2MeGet =
             '["https://www.googleapis.com/auth/plus.login",
               "https://www.googleapis.com/auth/plus.me",
               "https://www.googleapis.com/auth/userinfo.email",
               "https://www.googleapis.com/auth/userinfo.profile"]
        requestClient UserInfoV2MeGet'{..}
          = go _uivmgFields (Just AltJSON) oAuth2Service
          where go
                  = buildClient
                      (Proxy :: Proxy UserInfoV2MeGetResource)
                      mempty
