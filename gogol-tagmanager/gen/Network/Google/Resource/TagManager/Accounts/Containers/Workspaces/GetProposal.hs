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
-- Module      : Network.Google.Resource.TagManager.Accounts.Containers.Workspaces.GetProposal
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a GTM Workspace Proposal.
--
-- /See:/ <https://developers.google.com/tag-manager/api/v2/ Tag Manager API Reference> for @tagmanager.accounts.containers.workspaces.getProposal@.
module Network.Google.Resource.TagManager.Accounts.Containers.Workspaces.GetProposal
    (
    -- * REST Resource
      AccountsContainersWorkspacesGetProposalResource

    -- * Creating a Request
    , accountsContainersWorkspacesGetProposal
    , AccountsContainersWorkspacesGetProposal

    -- * Request Lenses
    , acwgpPath
    , acwgpFields
    ) where

import Network.Google.Prelude
import Network.Google.TagManager.Types

-- | A resource alias for @tagmanager.accounts.containers.workspaces.getProposal@ method which the
-- 'AccountsContainersWorkspacesGetProposal' request conforms to.
type AccountsContainersWorkspacesGetProposalResource
     =
     "tagmanager" :>
       "v2" :>
         Capture "path" Text :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :>
               Get '[JSON] WorkspaceProposal

-- | Gets a GTM Workspace Proposal.
--
-- /See:/ 'accountsContainersWorkspacesGetProposal' smart constructor.
data AccountsContainersWorkspacesGetProposal = AccountsContainersWorkspacesGetProposal'
    { _acwgpPath :: !Text
    , _acwgpFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsContainersWorkspacesGetProposal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acwgpPath'
--
-- * 'acwgpFields'
accountsContainersWorkspacesGetProposal
    :: Text -- ^ 'acwgpPath'
    -> AccountsContainersWorkspacesGetProposal
accountsContainersWorkspacesGetProposal pAcwgpPath_ = 
    AccountsContainersWorkspacesGetProposal'
    { _acwgpPath = pAcwgpPath_
    , _acwgpFields = Nothing
    }

-- | GTM workspace proposal\'s relative path: Example:
-- accounts\/{aid}\/containers\/{cid}\/workspace\/{wid}\/workspace_proposal
acwgpPath :: Lens' AccountsContainersWorkspacesGetProposal Text
acwgpPath
  = lens _acwgpPath (\ s a -> s{_acwgpPath = a})

-- | Selector specifying which fields to include in a partial response.
acwgpFields :: Lens' AccountsContainersWorkspacesGetProposal (Maybe Text)
acwgpFields
  = lens _acwgpFields (\ s a -> s{_acwgpFields = a})

instance GoogleRequest
         AccountsContainersWorkspacesGetProposal where
        type Rs AccountsContainersWorkspacesGetProposal =
             WorkspaceProposal
        type Scopes AccountsContainersWorkspacesGetProposal =
             '[]
        requestClient
          AccountsContainersWorkspacesGetProposal'{..}
          = go _acwgpPath _acwgpFields (Just AltJSON)
              tagManagerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           AccountsContainersWorkspacesGetProposalResource)
                      mempty
