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
-- Module      : Network.Google.Resource.Gmail.Users.Settings.Filters.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the message filters of a Gmail user.
--
-- /See:/ <https://developers.google.com/gmail/api/ Gmail API Reference> for @gmail.users.settings.filters.list@.
module Network.Google.Resource.Gmail.Users.Settings.Filters.List
    (
    -- * REST Resource
      UsersSettingsFiltersListResource

    -- * Creating a Request
    , usersSettingsFiltersList
    , UsersSettingsFiltersList

    -- * Request Lenses
    , usflUserId
    , usflFields
    ) where

import Network.Google.Gmail.Types
import Network.Google.Prelude

-- | A resource alias for @gmail.users.settings.filters.list@ method which the
-- 'UsersSettingsFiltersList' request conforms to.
type UsersSettingsFiltersListResource =
     "gmail" :>
       "v1" :>
         "users" :>
           Capture "userId" Text :>
             "settings" :>
               "filters" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] ListFiltersResponse

-- | Lists the message filters of a Gmail user.
--
-- /See:/ 'usersSettingsFiltersList' smart constructor.
data UsersSettingsFiltersList = UsersSettingsFiltersList'
    { _usflUserId :: !Text
    , _usflFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersSettingsFiltersList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usflUserId'
--
-- * 'usflFields'
usersSettingsFiltersList
    :: UsersSettingsFiltersList
usersSettingsFiltersList = 
    UsersSettingsFiltersList'
    { _usflUserId = "me"
    , _usflFields = Nothing
    }

-- | User\'s email address. The special value \"me\" can be used to indicate
-- the authenticated user.
usflUserId :: Lens' UsersSettingsFiltersList Text
usflUserId
  = lens _usflUserId (\ s a -> s{_usflUserId = a})

-- | Selector specifying which fields to include in a partial response.
usflFields :: Lens' UsersSettingsFiltersList (Maybe Text)
usflFields
  = lens _usflFields (\ s a -> s{_usflFields = a})

instance GoogleRequest UsersSettingsFiltersList where
        type Rs UsersSettingsFiltersList =
             ListFiltersResponse
        type Scopes UsersSettingsFiltersList =
             '["https://mail.google.com/",
               "https://www.googleapis.com/auth/gmail.modify",
               "https://www.googleapis.com/auth/gmail.readonly",
               "https://www.googleapis.com/auth/gmail.settings.basic"]
        requestClient UsersSettingsFiltersList'{..}
          = go _usflUserId _usflFields (Just AltJSON)
              gmailService
          where go
                  = buildClient
                      (Proxy :: Proxy UsersSettingsFiltersListResource)
                      mempty
