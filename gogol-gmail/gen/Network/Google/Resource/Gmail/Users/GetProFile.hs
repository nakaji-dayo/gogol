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
-- Module      : Network.Google.Resource.Gmail.Users.GetProFile
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current user\'s Gmail profile.
--
-- /See:/ <https://developers.google.com/gmail/api/ Gmail API Reference> for @gmail.users.getProfile@.
module Network.Google.Resource.Gmail.Users.GetProFile
    (
    -- * REST Resource
      UsersGetProFileResource

    -- * Creating a Request
    , usersGetProFile
    , UsersGetProFile

    -- * Request Lenses
    , ugpfUserId
    , ugpfFields
    ) where

import Network.Google.Gmail.Types
import Network.Google.Prelude

-- | A resource alias for @gmail.users.getProfile@ method which the
-- 'UsersGetProFile' request conforms to.
type UsersGetProFileResource =
     "gmail" :>
       "v1" :>
         "users" :>
           Capture "userId" Text :>
             "profile" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] ProFile

-- | Gets the current user\'s Gmail profile.
--
-- /See:/ 'usersGetProFile' smart constructor.
data UsersGetProFile = UsersGetProFile'
    { _ugpfUserId :: !Text
    , _ugpfFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersGetProFile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugpfUserId'
--
-- * 'ugpfFields'
usersGetProFile
    :: UsersGetProFile
usersGetProFile = 
    UsersGetProFile'
    { _ugpfUserId = "me"
    , _ugpfFields = Nothing
    }

-- | The user\'s email address. The special value me can be used to indicate
-- the authenticated user.
ugpfUserId :: Lens' UsersGetProFile Text
ugpfUserId
  = lens _ugpfUserId (\ s a -> s{_ugpfUserId = a})

-- | Selector specifying which fields to include in a partial response.
ugpfFields :: Lens' UsersGetProFile (Maybe Text)
ugpfFields
  = lens _ugpfFields (\ s a -> s{_ugpfFields = a})

instance GoogleRequest UsersGetProFile where
        type Rs UsersGetProFile = ProFile
        type Scopes UsersGetProFile =
             '["https://mail.google.com/",
               "https://www.googleapis.com/auth/gmail.compose",
               "https://www.googleapis.com/auth/gmail.metadata",
               "https://www.googleapis.com/auth/gmail.modify",
               "https://www.googleapis.com/auth/gmail.readonly"]
        requestClient UsersGetProFile'{..}
          = go _ugpfUserId _ugpfFields (Just AltJSON)
              gmailService
          where go
                  = buildClient
                      (Proxy :: Proxy UsersGetProFileResource)
                      mempty
