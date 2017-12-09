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
-- Module      : Network.Google.Resource.TagManager.Accounts.UserPermissions.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a user\'s Account & Container access.
--
-- /See:/ <https://developers.google.com/tag-manager/api/v2/ Tag Manager API Reference> for @tagmanager.accounts.user_permissions.get@.
module Network.Google.Resource.TagManager.Accounts.UserPermissions.Get
    (
    -- * REST Resource
      AccountsUserPermissionsGetResource

    -- * Creating a Request
    , accountsUserPermissionsGet
    , AccountsUserPermissionsGet

    -- * Request Lenses
    , aupgPath
    , aupgFields
    ) where

import Network.Google.Prelude
import Network.Google.TagManager.Types

-- | A resource alias for @tagmanager.accounts.user_permissions.get@ method which the
-- 'AccountsUserPermissionsGet' request conforms to.
type AccountsUserPermissionsGetResource =
     "tagmanager" :>
       "v2" :>
         Capture "path" Text :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :>
               Get '[JSON] UserPermission

-- | Gets a user\'s Account & Container access.
--
-- /See:/ 'accountsUserPermissionsGet' smart constructor.
data AccountsUserPermissionsGet = AccountsUserPermissionsGet'
    { _aupgPath :: !Text
    , _aupgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsUserPermissionsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aupgPath'
--
-- * 'aupgFields'
accountsUserPermissionsGet
    :: Text -- ^ 'aupgPath'
    -> AccountsUserPermissionsGet
accountsUserPermissionsGet pAupgPath_ = 
    AccountsUserPermissionsGet'
    { _aupgPath = pAupgPath_
    , _aupgFields = Nothing
    }

-- | GTM UserPermission\'s API relative path. Example:
-- accounts\/{account_id}\/user_permissions\/{user_permission_id}
aupgPath :: Lens' AccountsUserPermissionsGet Text
aupgPath = lens _aupgPath (\ s a -> s{_aupgPath = a})

-- | Selector specifying which fields to include in a partial response.
aupgFields :: Lens' AccountsUserPermissionsGet (Maybe Text)
aupgFields
  = lens _aupgFields (\ s a -> s{_aupgFields = a})

instance GoogleRequest AccountsUserPermissionsGet
         where
        type Rs AccountsUserPermissionsGet = UserPermission
        type Scopes AccountsUserPermissionsGet =
             '["https://www.googleapis.com/auth/tagmanager.manage.users"]
        requestClient AccountsUserPermissionsGet'{..}
          = go _aupgPath _aupgFields (Just AltJSON)
              tagManagerService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsUserPermissionsGetResource)
                      mempty
