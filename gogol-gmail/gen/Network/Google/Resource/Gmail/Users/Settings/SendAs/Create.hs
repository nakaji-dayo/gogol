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
-- Module      : Network.Google.Resource.Gmail.Users.Settings.SendAs.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom \"from\" send-as alias. If an SMTP MSA is specified,
-- Gmail will attempt to connect to the SMTP service to validate the
-- configuration before creating the alias. If ownership verification is
-- required for the alias, a message will be sent to the email address and
-- the resource\'s verification status will be set to pending; otherwise,
-- the resource will be created with verification status set to accepted.
-- If a signature is provided, Gmail will sanitize the HTML before saving
-- it with the alias. This method is only available to service account
-- clients that have been delegated domain-wide authority.
--
-- /See:/ <https://developers.google.com/gmail/api/ Gmail API Reference> for @gmail.users.settings.sendAs.create@.
module Network.Google.Resource.Gmail.Users.Settings.SendAs.Create
    (
    -- * REST Resource
      UsersSettingsSendAsCreateResource

    -- * Creating a Request
    , usersSettingsSendAsCreate
    , UsersSettingsSendAsCreate

    -- * Request Lenses
    , ussacPayload
    , ussacUserId
    , ussacFields
    ) where

import Network.Google.Gmail.Types
import Network.Google.Prelude

-- | A resource alias for @gmail.users.settings.sendAs.create@ method which the
-- 'UsersSettingsSendAsCreate' request conforms to.
type UsersSettingsSendAsCreateResource =
     "gmail" :>
       "v1" :>
         "users" :>
           Capture "userId" Text :>
             "settings" :>
               "sendAs" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] SendAs :> Post '[JSON] SendAs

-- | Creates a custom \"from\" send-as alias. If an SMTP MSA is specified,
-- Gmail will attempt to connect to the SMTP service to validate the
-- configuration before creating the alias. If ownership verification is
-- required for the alias, a message will be sent to the email address and
-- the resource\'s verification status will be set to pending; otherwise,
-- the resource will be created with verification status set to accepted.
-- If a signature is provided, Gmail will sanitize the HTML before saving
-- it with the alias. This method is only available to service account
-- clients that have been delegated domain-wide authority.
--
-- /See:/ 'usersSettingsSendAsCreate' smart constructor.
data UsersSettingsSendAsCreate = UsersSettingsSendAsCreate'
    { _ussacPayload :: !SendAs
    , _ussacUserId :: !Text
    , _ussacFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersSettingsSendAsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ussacPayload'
--
-- * 'ussacUserId'
--
-- * 'ussacFields'
usersSettingsSendAsCreate
    :: SendAs -- ^ 'ussacPayload'
    -> UsersSettingsSendAsCreate
usersSettingsSendAsCreate pUssacPayload_ = 
    UsersSettingsSendAsCreate'
    { _ussacPayload = pUssacPayload_
    , _ussacUserId = "me"
    , _ussacFields = Nothing
    }

-- | Multipart request metadata.
ussacPayload :: Lens' UsersSettingsSendAsCreate SendAs
ussacPayload
  = lens _ussacPayload (\ s a -> s{_ussacPayload = a})

-- | User\'s email address. The special value \"me\" can be used to indicate
-- the authenticated user.
ussacUserId :: Lens' UsersSettingsSendAsCreate Text
ussacUserId
  = lens _ussacUserId (\ s a -> s{_ussacUserId = a})

-- | Selector specifying which fields to include in a partial response.
ussacFields :: Lens' UsersSettingsSendAsCreate (Maybe Text)
ussacFields
  = lens _ussacFields (\ s a -> s{_ussacFields = a})

instance GoogleRequest UsersSettingsSendAsCreate
         where
        type Rs UsersSettingsSendAsCreate = SendAs
        type Scopes UsersSettingsSendAsCreate =
             '["https://www.googleapis.com/auth/gmail.settings.sharing"]
        requestClient UsersSettingsSendAsCreate'{..}
          = go _ussacUserId _ussacFields (Just AltJSON)
              _ussacPayload
              gmailService
          where go
                  = buildClient
                      (Proxy :: Proxy UsersSettingsSendAsCreateResource)
                      mempty
