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
-- Module      : Network.Google.Resource.IAM.Projects.ServiceAccounts.Keys.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a ServiceAccountKey.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.projects.serviceAccounts.keys.delete@.
module Network.Google.Resource.IAM.Projects.ServiceAccounts.Keys.Delete
    (
    -- * REST Resource
      ProjectsServiceAccountsKeysDeleteResource

    -- * Creating a Request
    , projectsServiceAccountsKeysDelete
    , ProjectsServiceAccountsKeysDelete

    -- * Request Lenses
    , psakdXgafv
    , psakdUploadProtocol
    , psakdPp
    , psakdAccessToken
    , psakdUploadType
    , psakdBearerToken
    , psakdName
    , psakdFields
    , psakdCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.projects.serviceAccounts.keys.delete@ method which the
-- 'ProjectsServiceAccountsKeysDelete' request conforms to.
type ProjectsServiceAccountsKeysDeleteResource =
     "v1" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :> Delete '[JSON] Empty

-- | Deletes a ServiceAccountKey.
--
-- /See:/ 'projectsServiceAccountsKeysDelete' smart constructor.
data ProjectsServiceAccountsKeysDelete = ProjectsServiceAccountsKeysDelete'
    { _psakdXgafv :: !(Maybe Xgafv)
    , _psakdUploadProtocol :: !(Maybe Text)
    , _psakdPp :: !Bool
    , _psakdAccessToken :: !(Maybe Text)
    , _psakdUploadType :: !(Maybe Text)
    , _psakdBearerToken :: !(Maybe Text)
    , _psakdName :: !Text
    , _psakdFields :: !(Maybe Text)
    , _psakdCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsServiceAccountsKeysDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psakdXgafv'
--
-- * 'psakdUploadProtocol'
--
-- * 'psakdPp'
--
-- * 'psakdAccessToken'
--
-- * 'psakdUploadType'
--
-- * 'psakdBearerToken'
--
-- * 'psakdName'
--
-- * 'psakdFields'
--
-- * 'psakdCallback'
projectsServiceAccountsKeysDelete
    :: Text -- ^ 'psakdName'
    -> ProjectsServiceAccountsKeysDelete
projectsServiceAccountsKeysDelete pPsakdName_ = 
    ProjectsServiceAccountsKeysDelete'
    { _psakdXgafv = Nothing
    , _psakdUploadProtocol = Nothing
    , _psakdPp = True
    , _psakdAccessToken = Nothing
    , _psakdUploadType = Nothing
    , _psakdBearerToken = Nothing
    , _psakdName = pPsakdName_
    , _psakdFields = Nothing
    , _psakdCallback = Nothing
    }

-- | V1 error format.
psakdXgafv :: Lens' ProjectsServiceAccountsKeysDelete (Maybe Xgafv)
psakdXgafv
  = lens _psakdXgafv (\ s a -> s{_psakdXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
psakdUploadProtocol :: Lens' ProjectsServiceAccountsKeysDelete (Maybe Text)
psakdUploadProtocol
  = lens _psakdUploadProtocol
      (\ s a -> s{_psakdUploadProtocol = a})

-- | Pretty-print response.
psakdPp :: Lens' ProjectsServiceAccountsKeysDelete Bool
psakdPp = lens _psakdPp (\ s a -> s{_psakdPp = a})

-- | OAuth access token.
psakdAccessToken :: Lens' ProjectsServiceAccountsKeysDelete (Maybe Text)
psakdAccessToken
  = lens _psakdAccessToken
      (\ s a -> s{_psakdAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
psakdUploadType :: Lens' ProjectsServiceAccountsKeysDelete (Maybe Text)
psakdUploadType
  = lens _psakdUploadType
      (\ s a -> s{_psakdUploadType = a})

-- | OAuth bearer token.
psakdBearerToken :: Lens' ProjectsServiceAccountsKeysDelete (Maybe Text)
psakdBearerToken
  = lens _psakdBearerToken
      (\ s a -> s{_psakdBearerToken = a})

-- | The resource name of the service account key in the following format:
-- \`projects\/{PROJECT_ID}\/serviceAccounts\/{ACCOUNT}\/keys\/{key}\`.
-- Using \`-\` as a wildcard for the \`PROJECT_ID\` will infer the project
-- from the account. The \`ACCOUNT\` value can be the \`email\` address or
-- the \`unique_id\` of the service account.
psakdName :: Lens' ProjectsServiceAccountsKeysDelete Text
psakdName
  = lens _psakdName (\ s a -> s{_psakdName = a})

-- | Selector specifying which fields to include in a partial response.
psakdFields :: Lens' ProjectsServiceAccountsKeysDelete (Maybe Text)
psakdFields
  = lens _psakdFields (\ s a -> s{_psakdFields = a})

-- | JSONP
psakdCallback :: Lens' ProjectsServiceAccountsKeysDelete (Maybe Text)
psakdCallback
  = lens _psakdCallback
      (\ s a -> s{_psakdCallback = a})

instance GoogleRequest
         ProjectsServiceAccountsKeysDelete where
        type Rs ProjectsServiceAccountsKeysDelete = Empty
        type Scopes ProjectsServiceAccountsKeysDelete =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsServiceAccountsKeysDelete'{..}
          = go _psakdName _psakdXgafv _psakdUploadProtocol
              (Just _psakdPp)
              _psakdAccessToken
              _psakdUploadType
              _psakdBearerToken
              _psakdCallback
              _psakdFields
              (Just AltJSON)
              iAMService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsServiceAccountsKeysDeleteResource)
                      mempty
