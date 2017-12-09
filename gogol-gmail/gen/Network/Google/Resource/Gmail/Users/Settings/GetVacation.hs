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
-- Module      : Network.Google.Resource.Gmail.Users.Settings.GetVacation
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets vacation responder settings.
--
-- /See:/ <https://developers.google.com/gmail/api/ Gmail API Reference> for @gmail.users.settings.getVacation@.
module Network.Google.Resource.Gmail.Users.Settings.GetVacation
    (
    -- * REST Resource
      UsersSettingsGetVacationResource

    -- * Creating a Request
    , usersSettingsGetVacation
    , UsersSettingsGetVacation

    -- * Request Lenses
    , usgvUserId
    , usgvFields
    ) where

import Network.Google.Gmail.Types
import Network.Google.Prelude

-- | A resource alias for @gmail.users.settings.getVacation@ method which the
-- 'UsersSettingsGetVacation' request conforms to.
type UsersSettingsGetVacationResource =
     "gmail" :>
       "v1" :>
         "users" :>
           Capture "userId" Text :>
             "settings" :>
               "vacation" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] VacationSettings

-- | Gets vacation responder settings.
--
-- /See:/ 'usersSettingsGetVacation' smart constructor.
data UsersSettingsGetVacation = UsersSettingsGetVacation'
    { _usgvUserId :: !Text
    , _usgvFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersSettingsGetVacation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usgvUserId'
--
-- * 'usgvFields'
usersSettingsGetVacation
    :: UsersSettingsGetVacation
usersSettingsGetVacation = 
    UsersSettingsGetVacation'
    { _usgvUserId = "me"
    , _usgvFields = Nothing
    }

-- | User\'s email address. The special value \"me\" can be used to indicate
-- the authenticated user.
usgvUserId :: Lens' UsersSettingsGetVacation Text
usgvUserId
  = lens _usgvUserId (\ s a -> s{_usgvUserId = a})

-- | Selector specifying which fields to include in a partial response.
usgvFields :: Lens' UsersSettingsGetVacation (Maybe Text)
usgvFields
  = lens _usgvFields (\ s a -> s{_usgvFields = a})

instance GoogleRequest UsersSettingsGetVacation where
        type Rs UsersSettingsGetVacation = VacationSettings
        type Scopes UsersSettingsGetVacation =
             '["https://mail.google.com/",
               "https://www.googleapis.com/auth/gmail.modify",
               "https://www.googleapis.com/auth/gmail.readonly",
               "https://www.googleapis.com/auth/gmail.settings.basic"]
        requestClient UsersSettingsGetVacation'{..}
          = go _usgvUserId _usgvFields (Just AltJSON)
              gmailService
          where go
                  = buildClient
                      (Proxy :: Proxy UsersSettingsGetVacationResource)
                      mempty
