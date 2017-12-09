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
-- Module      : Network.Google.Resource.Gmail.Users.Settings.GetAutoForwarding
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the auto-forwarding setting for the specified account.
--
-- /See:/ <https://developers.google.com/gmail/api/ Gmail API Reference> for @gmail.users.settings.getAutoForwarding@.
module Network.Google.Resource.Gmail.Users.Settings.GetAutoForwarding
    (
    -- * REST Resource
      UsersSettingsGetAutoForwardingResource

    -- * Creating a Request
    , usersSettingsGetAutoForwarding
    , UsersSettingsGetAutoForwarding

    -- * Request Lenses
    , usgafUserId
    , usgafFields
    ) where

import Network.Google.Gmail.Types
import Network.Google.Prelude

-- | A resource alias for @gmail.users.settings.getAutoForwarding@ method which the
-- 'UsersSettingsGetAutoForwarding' request conforms to.
type UsersSettingsGetAutoForwardingResource =
     "gmail" :>
       "v1" :>
         "users" :>
           Capture "userId" Text :>
             "settings" :>
               "autoForwarding" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] AutoForwarding

-- | Gets the auto-forwarding setting for the specified account.
--
-- /See:/ 'usersSettingsGetAutoForwarding' smart constructor.
data UsersSettingsGetAutoForwarding = UsersSettingsGetAutoForwarding'
    { _usgafUserId :: !Text
    , _usgafFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersSettingsGetAutoForwarding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usgafUserId'
--
-- * 'usgafFields'
usersSettingsGetAutoForwarding
    :: UsersSettingsGetAutoForwarding
usersSettingsGetAutoForwarding = 
    UsersSettingsGetAutoForwarding'
    { _usgafUserId = "me"
    , _usgafFields = Nothing
    }

-- | User\'s email address. The special value \"me\" can be used to indicate
-- the authenticated user.
usgafUserId :: Lens' UsersSettingsGetAutoForwarding Text
usgafUserId
  = lens _usgafUserId (\ s a -> s{_usgafUserId = a})

-- | Selector specifying which fields to include in a partial response.
usgafFields :: Lens' UsersSettingsGetAutoForwarding (Maybe Text)
usgafFields
  = lens _usgafFields (\ s a -> s{_usgafFields = a})

instance GoogleRequest UsersSettingsGetAutoForwarding
         where
        type Rs UsersSettingsGetAutoForwarding =
             AutoForwarding
        type Scopes UsersSettingsGetAutoForwarding =
             '["https://mail.google.com/",
               "https://www.googleapis.com/auth/gmail.modify",
               "https://www.googleapis.com/auth/gmail.readonly",
               "https://www.googleapis.com/auth/gmail.settings.basic"]
        requestClient UsersSettingsGetAutoForwarding'{..}
          = go _usgafUserId _usgafFields (Just AltJSON)
              gmailService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy UsersSettingsGetAutoForwardingResource)
                      mempty
