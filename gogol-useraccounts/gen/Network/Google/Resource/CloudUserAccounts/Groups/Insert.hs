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
-- Module      : Network.Google.Resource.CloudUserAccounts.Groups.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Group resource in the specified project using the data
-- included in the request.
--
-- /See:/ <https://cloud.google.com/compute/docs/access/user-accounts/api/latest/ Cloud User Accounts API Reference> for @clouduseraccounts.groups.insert@.
module Network.Google.Resource.CloudUserAccounts.Groups.Insert
    (
    -- * REST Resource
      GroupsInsertResource

    -- * Creating a Request
    , groupsInsert
    , GroupsInsert

    -- * Request Lenses
    , giProject
    , giPayload
    , giFields
    ) where

import Network.Google.Prelude
import Network.Google.UserAccounts.Types

-- | A resource alias for @clouduseraccounts.groups.insert@ method which the
-- 'GroupsInsert' request conforms to.
type GroupsInsertResource =
     "clouduseraccounts" :>
       "beta" :>
         "projects" :>
           Capture "project" Text :>
             "global" :>
               "groups" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] Group :> Post '[JSON] Operation

-- | Creates a Group resource in the specified project using the data
-- included in the request.
--
-- /See:/ 'groupsInsert' smart constructor.
data GroupsInsert = GroupsInsert'
    { _giProject :: !Text
    , _giPayload :: !Group
    , _giFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupsInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giProject'
--
-- * 'giPayload'
--
-- * 'giFields'
groupsInsert
    :: Text -- ^ 'giProject'
    -> Group -- ^ 'giPayload'
    -> GroupsInsert
groupsInsert pGiProject_ pGiPayload_ = 
    GroupsInsert'
    { _giProject = pGiProject_
    , _giPayload = pGiPayload_
    , _giFields = Nothing
    }

-- | Project ID for this request.
giProject :: Lens' GroupsInsert Text
giProject
  = lens _giProject (\ s a -> s{_giProject = a})

-- | Multipart request metadata.
giPayload :: Lens' GroupsInsert Group
giPayload
  = lens _giPayload (\ s a -> s{_giPayload = a})

-- | Selector specifying which fields to include in a partial response.
giFields :: Lens' GroupsInsert (Maybe Text)
giFields = lens _giFields (\ s a -> s{_giFields = a})

instance GoogleRequest GroupsInsert where
        type Rs GroupsInsert = Operation
        type Scopes GroupsInsert =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/cloud.useraccounts"]
        requestClient GroupsInsert'{..}
          = go _giProject _giFields (Just AltJSON) _giPayload
              userAccountsService
          where go
                  = buildClient (Proxy :: Proxy GroupsInsertResource)
                      mempty
