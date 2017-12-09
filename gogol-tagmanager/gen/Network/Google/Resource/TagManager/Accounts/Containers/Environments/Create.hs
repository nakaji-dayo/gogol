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
-- Module      : Network.Google.Resource.TagManager.Accounts.Containers.Environments.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a GTM Environment.
--
-- /See:/ <https://developers.google.com/tag-manager/api/v2/ Tag Manager API Reference> for @tagmanager.accounts.containers.environments.create@.
module Network.Google.Resource.TagManager.Accounts.Containers.Environments.Create
    (
    -- * REST Resource
      AccountsContainersEnvironmentsCreateResource

    -- * Creating a Request
    , accountsContainersEnvironmentsCreate
    , AccountsContainersEnvironmentsCreate

    -- * Request Lenses
    , acecParent
    , acecPayload
    , acecFields
    ) where

import Network.Google.Prelude
import Network.Google.TagManager.Types

-- | A resource alias for @tagmanager.accounts.containers.environments.create@ method which the
-- 'AccountsContainersEnvironmentsCreate' request conforms to.
type AccountsContainersEnvironmentsCreateResource =
     "tagmanager" :>
       "v2" :>
         Capture "parent" Text :>
           "environments" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] Environment :>
                   Post '[JSON] Environment

-- | Creates a GTM Environment.
--
-- /See:/ 'accountsContainersEnvironmentsCreate' smart constructor.
data AccountsContainersEnvironmentsCreate = AccountsContainersEnvironmentsCreate'
    { _acecParent :: !Text
    , _acecPayload :: !Environment
    , _acecFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsContainersEnvironmentsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acecParent'
--
-- * 'acecPayload'
--
-- * 'acecFields'
accountsContainersEnvironmentsCreate
    :: Text -- ^ 'acecParent'
    -> Environment -- ^ 'acecPayload'
    -> AccountsContainersEnvironmentsCreate
accountsContainersEnvironmentsCreate pAcecParent_ pAcecPayload_ = 
    AccountsContainersEnvironmentsCreate'
    { _acecParent = pAcecParent_
    , _acecPayload = pAcecPayload_
    , _acecFields = Nothing
    }

-- | GTM Container\'s API relative path. Example:
-- accounts\/{account_id}\/containers\/{container_id}
acecParent :: Lens' AccountsContainersEnvironmentsCreate Text
acecParent
  = lens _acecParent (\ s a -> s{_acecParent = a})

-- | Multipart request metadata.
acecPayload :: Lens' AccountsContainersEnvironmentsCreate Environment
acecPayload
  = lens _acecPayload (\ s a -> s{_acecPayload = a})

-- | Selector specifying which fields to include in a partial response.
acecFields :: Lens' AccountsContainersEnvironmentsCreate (Maybe Text)
acecFields
  = lens _acecFields (\ s a -> s{_acecFields = a})

instance GoogleRequest
         AccountsContainersEnvironmentsCreate where
        type Rs AccountsContainersEnvironmentsCreate =
             Environment
        type Scopes AccountsContainersEnvironmentsCreate =
             '["https://www.googleapis.com/auth/tagmanager.edit.containers"]
        requestClient
          AccountsContainersEnvironmentsCreate'{..}
          = go _acecParent _acecFields (Just AltJSON)
              _acecPayload
              tagManagerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AccountsContainersEnvironmentsCreateResource)
                      mempty
