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
-- Module      : Network.Google.Resource.Gmail.Users.Messages.Trash
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified message to the trash.
--
-- /See:/ <https://developers.google.com/gmail/api/ Gmail API Reference> for @gmail.users.messages.trash@.
module Network.Google.Resource.Gmail.Users.Messages.Trash
    (
    -- * REST Resource
      UsersMessagesTrashResource

    -- * Creating a Request
    , usersMessagesTrash
    , UsersMessagesTrash

    -- * Request Lenses
    , umtUserId
    , umtId
    , umtFields
    ) where

import Network.Google.Gmail.Types
import Network.Google.Prelude

-- | A resource alias for @gmail.users.messages.trash@ method which the
-- 'UsersMessagesTrash' request conforms to.
type UsersMessagesTrashResource =
     "gmail" :>
       "v1" :>
         "users" :>
           Capture "userId" Text :>
             "messages" :>
               Capture "id" Text :>
                 "trash" :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :> Post '[JSON] Message

-- | Moves the specified message to the trash.
--
-- /See:/ 'usersMessagesTrash' smart constructor.
data UsersMessagesTrash = UsersMessagesTrash'
    { _umtUserId :: !Text
    , _umtId :: !Text
    , _umtFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersMessagesTrash' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umtUserId'
--
-- * 'umtId'
--
-- * 'umtFields'
usersMessagesTrash
    :: Text -- ^ 'umtId'
    -> UsersMessagesTrash
usersMessagesTrash pUmtId_ = 
    UsersMessagesTrash'
    { _umtUserId = "me"
    , _umtId = pUmtId_
    , _umtFields = Nothing
    }

-- | The user\'s email address. The special value me can be used to indicate
-- the authenticated user.
umtUserId :: Lens' UsersMessagesTrash Text
umtUserId
  = lens _umtUserId (\ s a -> s{_umtUserId = a})

-- | The ID of the message to Trash.
umtId :: Lens' UsersMessagesTrash Text
umtId = lens _umtId (\ s a -> s{_umtId = a})

-- | Selector specifying which fields to include in a partial response.
umtFields :: Lens' UsersMessagesTrash (Maybe Text)
umtFields
  = lens _umtFields (\ s a -> s{_umtFields = a})

instance GoogleRequest UsersMessagesTrash where
        type Rs UsersMessagesTrash = Message
        type Scopes UsersMessagesTrash =
             '["https://mail.google.com/",
               "https://www.googleapis.com/auth/gmail.modify"]
        requestClient UsersMessagesTrash'{..}
          = go _umtUserId _umtId _umtFields (Just AltJSON)
              gmailService
          where go
                  = buildClient
                      (Proxy :: Proxy UsersMessagesTrashResource)
                      mempty
