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
-- Module      : Network.Google.Resource.TagManager.Accounts.Containers.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Container.
--
-- /See:/ <https://developers.google.com/tag-manager/api/v2/ Tag Manager API Reference> for @tagmanager.accounts.containers.delete@.
module Network.Google.Resource.TagManager.Accounts.Containers.Delete
    (
    -- * REST Resource
      AccountsContainersDeleteResource

    -- * Creating a Request
    , accountsContainersDelete
    , AccountsContainersDelete

    -- * Request Lenses
    , acdPath
    , acdFields
    ) where

import Network.Google.Prelude
import Network.Google.TagManager.Types

-- | A resource alias for @tagmanager.accounts.containers.delete@ method which the
-- 'AccountsContainersDelete' request conforms to.
type AccountsContainersDeleteResource =
     "tagmanager" :>
       "v2" :>
         Capture "path" Text :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Deletes a Container.
--
-- /See:/ 'accountsContainersDelete' smart constructor.
data AccountsContainersDelete = AccountsContainersDelete'
    { _acdPath :: !Text
    , _acdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsContainersDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acdPath'
--
-- * 'acdFields'
accountsContainersDelete
    :: Text -- ^ 'acdPath'
    -> AccountsContainersDelete
accountsContainersDelete pAcdPath_ = 
    AccountsContainersDelete'
    { _acdPath = pAcdPath_
    , _acdFields = Nothing
    }

-- | GTM Container\'s API relative path. Example:
-- accounts\/{account_id}\/containers\/{container_id}
acdPath :: Lens' AccountsContainersDelete Text
acdPath = lens _acdPath (\ s a -> s{_acdPath = a})

-- | Selector specifying which fields to include in a partial response.
acdFields :: Lens' AccountsContainersDelete (Maybe Text)
acdFields
  = lens _acdFields (\ s a -> s{_acdFields = a})

instance GoogleRequest AccountsContainersDelete where
        type Rs AccountsContainersDelete = ()
        type Scopes AccountsContainersDelete =
             '["https://www.googleapis.com/auth/tagmanager.delete.containers"]
        requestClient AccountsContainersDelete'{..}
          = go _acdPath _acdFields (Just AltJSON)
              tagManagerService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsContainersDeleteResource)
                      mempty
