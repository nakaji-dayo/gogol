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
-- Module      : Network.Google.Resource.TagManager.Accounts.Containers.Workspaces.GetStatus
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds conflicting and modified entities in the workspace.
--
-- /See:/ <https://developers.google.com/tag-manager/api/v2/ Tag Manager API Reference> for @tagmanager.accounts.containers.workspaces.getStatus@.
module Network.Google.Resource.TagManager.Accounts.Containers.Workspaces.GetStatus
    (
    -- * REST Resource
      AccountsContainersWorkspacesGetStatusResource

    -- * Creating a Request
    , accountsContainersWorkspacesGetStatus
    , AccountsContainersWorkspacesGetStatus

    -- * Request Lenses
    , acwgsPath
    , acwgsFields
    ) where

import Network.Google.Prelude
import Network.Google.TagManager.Types

-- | A resource alias for @tagmanager.accounts.containers.workspaces.getStatus@ method which the
-- 'AccountsContainersWorkspacesGetStatus' request conforms to.
type AccountsContainersWorkspacesGetStatusResource =
     "tagmanager" :>
       "v2" :>
         Capture "path" Text :>
           "status" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 Get '[JSON] GetWorkspaceStatusResponse

-- | Finds conflicting and modified entities in the workspace.
--
-- /See:/ 'accountsContainersWorkspacesGetStatus' smart constructor.
data AccountsContainersWorkspacesGetStatus = AccountsContainersWorkspacesGetStatus'
    { _acwgsPath :: !Text
    , _acwgsFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsContainersWorkspacesGetStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acwgsPath'
--
-- * 'acwgsFields'
accountsContainersWorkspacesGetStatus
    :: Text -- ^ 'acwgsPath'
    -> AccountsContainersWorkspacesGetStatus
accountsContainersWorkspacesGetStatus pAcwgsPath_ = 
    AccountsContainersWorkspacesGetStatus'
    { _acwgsPath = pAcwgsPath_
    , _acwgsFields = Nothing
    }

-- | GTM Workspace\'s API relative path. Example:
-- accounts\/{account_id}\/containers\/{container_id}\/workspaces\/{workspace_id}
acwgsPath :: Lens' AccountsContainersWorkspacesGetStatus Text
acwgsPath
  = lens _acwgsPath (\ s a -> s{_acwgsPath = a})

-- | Selector specifying which fields to include in a partial response.
acwgsFields :: Lens' AccountsContainersWorkspacesGetStatus (Maybe Text)
acwgsFields
  = lens _acwgsFields (\ s a -> s{_acwgsFields = a})

instance GoogleRequest
         AccountsContainersWorkspacesGetStatus where
        type Rs AccountsContainersWorkspacesGetStatus =
             GetWorkspaceStatusResponse
        type Scopes AccountsContainersWorkspacesGetStatus =
             '["https://www.googleapis.com/auth/tagmanager.edit.containers",
               "https://www.googleapis.com/auth/tagmanager.readonly"]
        requestClient
          AccountsContainersWorkspacesGetStatus'{..}
          = go _acwgsPath _acwgsFields (Just AltJSON)
              tagManagerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AccountsContainersWorkspacesGetStatusResource)
                      mempty
