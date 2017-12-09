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
-- Module      : Network.Google.Resource.TagManager.Accounts.UserPermissions.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a user from the account, revoking access to it and all of its
-- containers.
--
-- /See:/ <https://developers.google.com/tag-manager/api/v2/ Tag Manager API Reference> for @tagmanager.accounts.user_permissions.delete@.
module Network.Google.Resource.TagManager.Accounts.UserPermissions.Delete
    (
    -- * REST Resource
      AccountsUserPermissionsDeleteResource

    -- * Creating a Request
    , accountsUserPermissionsDelete
    , AccountsUserPermissionsDelete

    -- * Request Lenses
    , aupdPath
    , aupdFields
    ) where

import Network.Google.Prelude
import Network.Google.TagManager.Types

-- | A resource alias for @tagmanager.accounts.user_permissions.delete@ method which the
-- 'AccountsUserPermissionsDelete' request conforms to.
type AccountsUserPermissionsDeleteResource =
     "tagmanager" :>
       "v2" :>
         Capture "path" Text :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Removes a user from the account, revoking access to it and all of its
-- containers.
--
-- /See:/ 'accountsUserPermissionsDelete' smart constructor.
data AccountsUserPermissionsDelete = AccountsUserPermissionsDelete'
    { _aupdPath :: !Text
    , _aupdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsUserPermissionsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aupdPath'
--
-- * 'aupdFields'
accountsUserPermissionsDelete
    :: Text -- ^ 'aupdPath'
    -> AccountsUserPermissionsDelete
accountsUserPermissionsDelete pAupdPath_ = 
    AccountsUserPermissionsDelete'
    { _aupdPath = pAupdPath_
    , _aupdFields = Nothing
    }

-- | GTM UserPermission\'s API relative path. Example:
-- accounts\/{account_id}\/user_permissions\/{user_permission_id}
aupdPath :: Lens' AccountsUserPermissionsDelete Text
aupdPath = lens _aupdPath (\ s a -> s{_aupdPath = a})

-- | Selector specifying which fields to include in a partial response.
aupdFields :: Lens' AccountsUserPermissionsDelete (Maybe Text)
aupdFields
  = lens _aupdFields (\ s a -> s{_aupdFields = a})

instance GoogleRequest AccountsUserPermissionsDelete
         where
        type Rs AccountsUserPermissionsDelete = ()
        type Scopes AccountsUserPermissionsDelete =
             '["https://www.googleapis.com/auth/tagmanager.manage.users"]
        requestClient AccountsUserPermissionsDelete'{..}
          = go _aupdPath _aupdFields (Just AltJSON)
              tagManagerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AccountsUserPermissionsDeleteResource)
                      mempty
