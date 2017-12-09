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
-- Module      : Network.Google.Resource.TagManager.Accounts.Containers.Workspaces.Triggers.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a GTM Trigger.
--
-- /See:/ <https://developers.google.com/tag-manager/api/v2/ Tag Manager API Reference> for @tagmanager.accounts.containers.workspaces.triggers.get@.
module Network.Google.Resource.TagManager.Accounts.Containers.Workspaces.Triggers.Get
    (
    -- * REST Resource
      AccountsContainersWorkspacesTriggersGetResource

    -- * Creating a Request
    , accountsContainersWorkspacesTriggersGet
    , AccountsContainersWorkspacesTriggersGet

    -- * Request Lenses
    , acwtgcPath
    , acwtgcFields
    ) where

import Network.Google.Prelude
import Network.Google.TagManager.Types

-- | A resource alias for @tagmanager.accounts.containers.workspaces.triggers.get@ method which the
-- 'AccountsContainersWorkspacesTriggersGet' request conforms to.
type AccountsContainersWorkspacesTriggersGetResource
     =
     "tagmanager" :>
       "v2" :>
         Capture "path" Text :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :> Get '[JSON] Trigger

-- | Gets a GTM Trigger.
--
-- /See:/ 'accountsContainersWorkspacesTriggersGet' smart constructor.
data AccountsContainersWorkspacesTriggersGet = AccountsContainersWorkspacesTriggersGet'
    { _acwtgcPath :: !Text
    , _acwtgcFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsContainersWorkspacesTriggersGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acwtgcPath'
--
-- * 'acwtgcFields'
accountsContainersWorkspacesTriggersGet
    :: Text -- ^ 'acwtgcPath'
    -> AccountsContainersWorkspacesTriggersGet
accountsContainersWorkspacesTriggersGet pAcwtgcPath_ = 
    AccountsContainersWorkspacesTriggersGet'
    { _acwtgcPath = pAcwtgcPath_
    , _acwtgcFields = Nothing
    }

-- | GTM Trigger\'s API relative path. Example:
-- accounts\/{account_id}\/containers\/{container_id}\/workspaces\/{workspace_id}\/triggers\/{trigger_id}
acwtgcPath :: Lens' AccountsContainersWorkspacesTriggersGet Text
acwtgcPath
  = lens _acwtgcPath (\ s a -> s{_acwtgcPath = a})

-- | Selector specifying which fields to include in a partial response.
acwtgcFields :: Lens' AccountsContainersWorkspacesTriggersGet (Maybe Text)
acwtgcFields
  = lens _acwtgcFields (\ s a -> s{_acwtgcFields = a})

instance GoogleRequest
         AccountsContainersWorkspacesTriggersGet where
        type Rs AccountsContainersWorkspacesTriggersGet =
             Trigger
        type Scopes AccountsContainersWorkspacesTriggersGet =
             '["https://www.googleapis.com/auth/tagmanager.edit.containers",
               "https://www.googleapis.com/auth/tagmanager.readonly"]
        requestClient
          AccountsContainersWorkspacesTriggersGet'{..}
          = go _acwtgcPath _acwtgcFields (Just AltJSON)
              tagManagerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           AccountsContainersWorkspacesTriggersGetResource)
                      mempty
