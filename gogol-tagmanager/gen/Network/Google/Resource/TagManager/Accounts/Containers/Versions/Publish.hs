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
-- Module      : Network.Google.Resource.TagManager.Accounts.Containers.Versions.Publish
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes a Container Version.
--
-- /See:/ <https://developers.google.com/tag-manager/api/v2/ Tag Manager API Reference> for @tagmanager.accounts.containers.versions.publish@.
module Network.Google.Resource.TagManager.Accounts.Containers.Versions.Publish
    (
    -- * REST Resource
      AccountsContainersVersionsPublishResource

    -- * Creating a Request
    , accountsContainersVersionsPublish
    , AccountsContainersVersionsPublish

    -- * Request Lenses
    , acvpPath
    , acvpFingerprint
    , acvpFields
    ) where

import Network.Google.Prelude
import Network.Google.TagManager.Types

-- | A resource alias for @tagmanager.accounts.containers.versions.publish@ method which the
-- 'AccountsContainersVersionsPublish' request conforms to.
type AccountsContainersVersionsPublishResource =
     "tagmanager" :>
       "v2" :>
         CaptureMode "path" "publish" Text :>
           QueryParam "fingerprint" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 Post '[JSON] PublishContainerVersionResponse

-- | Publishes a Container Version.
--
-- /See:/ 'accountsContainersVersionsPublish' smart constructor.
data AccountsContainersVersionsPublish = AccountsContainersVersionsPublish'
    { _acvpPath :: !Text
    , _acvpFingerprint :: !(Maybe Text)
    , _acvpFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsContainersVersionsPublish' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acvpPath'
--
-- * 'acvpFingerprint'
--
-- * 'acvpFields'
accountsContainersVersionsPublish
    :: Text -- ^ 'acvpPath'
    -> AccountsContainersVersionsPublish
accountsContainersVersionsPublish pAcvpPath_ = 
    AccountsContainersVersionsPublish'
    { _acvpPath = pAcvpPath_
    , _acvpFingerprint = Nothing
    , _acvpFields = Nothing
    }

-- | GTM ContainerVersion\'s API relative path. Example:
-- accounts\/{account_id}\/containers\/{container_id}\/versions\/{version_id}
acvpPath :: Lens' AccountsContainersVersionsPublish Text
acvpPath = lens _acvpPath (\ s a -> s{_acvpPath = a})

-- | When provided, this fingerprint must match the fingerprint of the
-- container version in storage.
acvpFingerprint :: Lens' AccountsContainersVersionsPublish (Maybe Text)
acvpFingerprint
  = lens _acvpFingerprint
      (\ s a -> s{_acvpFingerprint = a})

-- | Selector specifying which fields to include in a partial response.
acvpFields :: Lens' AccountsContainersVersionsPublish (Maybe Text)
acvpFields
  = lens _acvpFields (\ s a -> s{_acvpFields = a})

instance GoogleRequest
         AccountsContainersVersionsPublish where
        type Rs AccountsContainersVersionsPublish =
             PublishContainerVersionResponse
        type Scopes AccountsContainersVersionsPublish =
             '["https://www.googleapis.com/auth/tagmanager.publish"]
        requestClient AccountsContainersVersionsPublish'{..}
          = go _acvpPath _acvpFingerprint _acvpFields
              (Just AltJSON)
              tagManagerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AccountsContainersVersionsPublishResource)
                      mempty
