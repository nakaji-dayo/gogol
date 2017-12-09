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
-- Module      : Network.Google.Resource.Directory.Users.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- create user.
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.users.insert@.
module Network.Google.Resource.Directory.Users.Insert
    (
    -- * REST Resource
      UsersInsertResource

    -- * Creating a Request
    , usersInsert
    , UsersInsert

    -- * Request Lenses
    , uiPayload
    , uiFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.users.insert@ method which the
-- 'UsersInsert' request conforms to.
type UsersInsertResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "users" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] User :> Post '[JSON] User

-- | create user.
--
-- /See:/ 'usersInsert' smart constructor.
data UsersInsert = UsersInsert'
    { _uiPayload :: !User
    , _uiFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiPayload'
--
-- * 'uiFields'
usersInsert
    :: User -- ^ 'uiPayload'
    -> UsersInsert
usersInsert pUiPayload_ = 
    UsersInsert'
    { _uiPayload = pUiPayload_
    , _uiFields = Nothing
    }

-- | Multipart request metadata.
uiPayload :: Lens' UsersInsert User
uiPayload
  = lens _uiPayload (\ s a -> s{_uiPayload = a})

-- | Selector specifying which fields to include in a partial response.
uiFields :: Lens' UsersInsert (Maybe Text)
uiFields = lens _uiFields (\ s a -> s{_uiFields = a})

instance GoogleRequest UsersInsert where
        type Rs UsersInsert = User
        type Scopes UsersInsert =
             '["https://www.googleapis.com/auth/admin.directory.user"]
        requestClient UsersInsert'{..}
          = go _uiFields (Just AltJSON) _uiPayload
              directoryService
          where go
                  = buildClient (Proxy :: Proxy UsersInsertResource)
                      mempty
