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
-- Module      : Network.Google.Resource.TagManager.Accounts.Containers.Workspaces.Variables.Revert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reverts changes to a GTM Variable in a GTM Workspace.
--
-- /See:/ <https://developers.google.com/tag-manager/api/v2/ Tag Manager API Reference> for @tagmanager.accounts.containers.workspaces.variables.revert@.
module Network.Google.Resource.TagManager.Accounts.Containers.Workspaces.Variables.Revert
    (
    -- * REST Resource
      AccountsContainersWorkspacesVariablesRevertResource

    -- * Creating a Request
    , accountsContainersWorkspacesVariablesRevert
    , AccountsContainersWorkspacesVariablesRevert

    -- * Request Lenses
    , acwvrPath
    , acwvrFingerprint
    , acwvrFields
    ) where

import Network.Google.Prelude
import Network.Google.TagManager.Types

-- | A resource alias for @tagmanager.accounts.containers.workspaces.variables.revert@ method which the
-- 'AccountsContainersWorkspacesVariablesRevert' request conforms to.
type AccountsContainersWorkspacesVariablesRevertResource
     =
     "tagmanager" :>
       "v2" :>
         CaptureMode "path" "revert" Text :>
           QueryParam "fingerprint" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 Post '[JSON] RevertVariableResponse

-- | Reverts changes to a GTM Variable in a GTM Workspace.
--
-- /See:/ 'accountsContainersWorkspacesVariablesRevert' smart constructor.
data AccountsContainersWorkspacesVariablesRevert = AccountsContainersWorkspacesVariablesRevert'
    { _acwvrPath :: !Text
    , _acwvrFingerprint :: !(Maybe Text)
    , _acwvrFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsContainersWorkspacesVariablesRevert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acwvrPath'
--
-- * 'acwvrFingerprint'
--
-- * 'acwvrFields'
accountsContainersWorkspacesVariablesRevert
    :: Text -- ^ 'acwvrPath'
    -> AccountsContainersWorkspacesVariablesRevert
accountsContainersWorkspacesVariablesRevert pAcwvrPath_ = 
    AccountsContainersWorkspacesVariablesRevert'
    { _acwvrPath = pAcwvrPath_
    , _acwvrFingerprint = Nothing
    , _acwvrFields = Nothing
    }

-- | GTM Variable\'s API relative path. Example:
-- accounts\/{account_id}\/containers\/{container_id}\/workspaces\/{workspace_id}\/variables\/{variable_id}
acwvrPath :: Lens' AccountsContainersWorkspacesVariablesRevert Text
acwvrPath
  = lens _acwvrPath (\ s a -> s{_acwvrPath = a})

-- | When provided, this fingerprint must match the fingerprint of the
-- variable in storage.
acwvrFingerprint :: Lens' AccountsContainersWorkspacesVariablesRevert (Maybe Text)
acwvrFingerprint
  = lens _acwvrFingerprint
      (\ s a -> s{_acwvrFingerprint = a})

-- | Selector specifying which fields to include in a partial response.
acwvrFields :: Lens' AccountsContainersWorkspacesVariablesRevert (Maybe Text)
acwvrFields
  = lens _acwvrFields (\ s a -> s{_acwvrFields = a})

instance GoogleRequest
         AccountsContainersWorkspacesVariablesRevert where
        type Rs AccountsContainersWorkspacesVariablesRevert =
             RevertVariableResponse
        type Scopes
               AccountsContainersWorkspacesVariablesRevert
             =
             '["https://www.googleapis.com/auth/tagmanager.edit.containers"]
        requestClient
          AccountsContainersWorkspacesVariablesRevert'{..}
          = go _acwvrPath _acwvrFingerprint _acwvrFields
              (Just AltJSON)
              tagManagerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           AccountsContainersWorkspacesVariablesRevertResource)
                      mempty
