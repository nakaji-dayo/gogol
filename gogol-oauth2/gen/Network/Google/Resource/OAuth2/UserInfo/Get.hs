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
-- Module      : Network.Google.Resource.OAuth2.UserInfo.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /See:/ <https://developers.google.com/accounts/docs/OAuth2 Google OAuth2 API Reference> for @oauth2.userinfo.get@.
module Network.Google.Resource.OAuth2.UserInfo.Get
    (
    -- * REST Resource
      UserInfoGetResource

    -- * Creating a Request
    , userInfoGet
    , UserInfoGet

    -- * Request Lenses
    , uigFields
    ) where

import Network.Google.OAuth2.Types
import Network.Google.Prelude

-- | A resource alias for @oauth2.userinfo.get@ method which the
-- 'UserInfoGet' request conforms to.
type UserInfoGetResource =
     "oauth2" :>
       "v2" :>
         "userinfo" :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :> Get '[JSON] UserInfoplus

--
-- /See:/ 'userInfoGet' smart constructor.
newtype UserInfoGet = UserInfoGet'
    { _uigFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserInfoGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uigFields'
userInfoGet
    :: UserInfoGet
userInfoGet = 
    UserInfoGet'
    { _uigFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
uigFields :: Lens' UserInfoGet (Maybe Text)
uigFields
  = lens _uigFields (\ s a -> s{_uigFields = a})

instance GoogleRequest UserInfoGet where
        type Rs UserInfoGet = UserInfoplus
        type Scopes UserInfoGet =
             '["https://www.googleapis.com/auth/plus.login",
               "https://www.googleapis.com/auth/plus.me",
               "https://www.googleapis.com/auth/userinfo.email",
               "https://www.googleapis.com/auth/userinfo.profile"]
        requestClient UserInfoGet'{..}
          = go _uigFields (Just AltJSON) oAuth2Service
          where go
                  = buildClient (Proxy :: Proxy UserInfoGetResource)
                      mempty
