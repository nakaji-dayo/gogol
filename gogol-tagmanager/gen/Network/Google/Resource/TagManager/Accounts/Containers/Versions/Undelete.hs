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
-- Module      : Network.Google.Resource.TagManager.Accounts.Containers.Versions.Undelete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeletes a Container Version.
--
-- /See:/ <https://developers.google.com/tag-manager/api/v2/ Tag Manager API Reference> for @tagmanager.accounts.containers.versions.undelete@.
module Network.Google.Resource.TagManager.Accounts.Containers.Versions.Undelete
    (
    -- * REST Resource
      AccountsContainersVersionsUndeleteResource

    -- * Creating a Request
    , accountsContainersVersionsUndelete
    , AccountsContainersVersionsUndelete

    -- * Request Lenses
    , acvuPath
    , acvuFields
    ) where

import Network.Google.Prelude
import Network.Google.TagManager.Types

-- | A resource alias for @tagmanager.accounts.containers.versions.undelete@ method which the
-- 'AccountsContainersVersionsUndelete' request conforms to.
type AccountsContainersVersionsUndeleteResource =
     "tagmanager" :>
       "v2" :>
         CaptureMode "path" "undelete" Text :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :>
               Post '[JSON] ContainerVersion

-- | Undeletes a Container Version.
--
-- /See:/ 'accountsContainersVersionsUndelete' smart constructor.
data AccountsContainersVersionsUndelete = AccountsContainersVersionsUndelete'
    { _acvuPath :: !Text
    , _acvuFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsContainersVersionsUndelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acvuPath'
--
-- * 'acvuFields'
accountsContainersVersionsUndelete
    :: Text -- ^ 'acvuPath'
    -> AccountsContainersVersionsUndelete
accountsContainersVersionsUndelete pAcvuPath_ = 
    AccountsContainersVersionsUndelete'
    { _acvuPath = pAcvuPath_
    , _acvuFields = Nothing
    }

-- | GTM ContainerVersion\'s API relative path. Example:
-- accounts\/{account_id}\/containers\/{container_id}\/versions\/{version_id}
acvuPath :: Lens' AccountsContainersVersionsUndelete Text
acvuPath = lens _acvuPath (\ s a -> s{_acvuPath = a})

-- | Selector specifying which fields to include in a partial response.
acvuFields :: Lens' AccountsContainersVersionsUndelete (Maybe Text)
acvuFields
  = lens _acvuFields (\ s a -> s{_acvuFields = a})

instance GoogleRequest
         AccountsContainersVersionsUndelete where
        type Rs AccountsContainersVersionsUndelete =
             ContainerVersion
        type Scopes AccountsContainersVersionsUndelete =
             '["https://www.googleapis.com/auth/tagmanager.edit.containerversions"]
        requestClient AccountsContainersVersionsUndelete'{..}
          = go _acvuPath _acvuFields (Just AltJSON)
              tagManagerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AccountsContainersVersionsUndeleteResource)
                      mempty
