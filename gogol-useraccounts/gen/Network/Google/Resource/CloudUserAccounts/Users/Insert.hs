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
-- Module      : Network.Google.Resource.CloudUserAccounts.Users.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a User resource in the specified project using the data included
-- in the request.
--
-- /See:/ <https://cloud.google.com/compute/docs/access/user-accounts/api/latest/ Cloud User Accounts API Reference> for @clouduseraccounts.users.insert@.
module Network.Google.Resource.CloudUserAccounts.Users.Insert
    (
    -- * REST Resource
      UsersInsertResource

    -- * Creating a Request
    , usersInsert
    , UsersInsert

    -- * Request Lenses
    , uiProject
    , uiPayload
    , uiFields
    ) where

import Network.Google.Prelude
import Network.Google.UserAccounts.Types

-- | A resource alias for @clouduseraccounts.users.insert@ method which the
-- 'UsersInsert' request conforms to.
type UsersInsertResource =
     "clouduseraccounts" :>
       "beta" :>
         "projects" :>
           Capture "project" Text :>
             "global" :>
               "users" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] User :> Post '[JSON] Operation

-- | Creates a User resource in the specified project using the data included
-- in the request.
--
-- /See:/ 'usersInsert' smart constructor.
data UsersInsert = UsersInsert'
    { _uiProject :: !Text
    , _uiPayload :: !User
    , _uiFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiProject'
--
-- * 'uiPayload'
--
-- * 'uiFields'
usersInsert
    :: Text -- ^ 'uiProject'
    -> User -- ^ 'uiPayload'
    -> UsersInsert
usersInsert pUiProject_ pUiPayload_ = 
    UsersInsert'
    { _uiProject = pUiProject_
    , _uiPayload = pUiPayload_
    , _uiFields = Nothing
    }

-- | Project ID for this request.
uiProject :: Lens' UsersInsert Text
uiProject
  = lens _uiProject (\ s a -> s{_uiProject = a})

-- | Multipart request metadata.
uiPayload :: Lens' UsersInsert User
uiPayload
  = lens _uiPayload (\ s a -> s{_uiPayload = a})

-- | Selector specifying which fields to include in a partial response.
uiFields :: Lens' UsersInsert (Maybe Text)
uiFields = lens _uiFields (\ s a -> s{_uiFields = a})

instance GoogleRequest UsersInsert where
        type Rs UsersInsert = Operation
        type Scopes UsersInsert =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/cloud.useraccounts"]
        requestClient UsersInsert'{..}
          = go _uiProject _uiFields (Just AltJSON) _uiPayload
              userAccountsService
          where go
                  = buildClient (Proxy :: Proxy UsersInsertResource)
                      mempty
