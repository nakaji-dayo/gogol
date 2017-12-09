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
-- Module      : Network.Google.Resource.IAM.Projects.ServiceAccounts.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a ServiceAccount.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.projects.serviceAccounts.get@.
module Network.Google.Resource.IAM.Projects.ServiceAccounts.Get
    (
    -- * REST Resource
      ProjectsServiceAccountsGetResource

    -- * Creating a Request
    , projectsServiceAccountsGet
    , ProjectsServiceAccountsGet

    -- * Request Lenses
    , psagXgafv
    , psagUploadProtocol
    , psagPp
    , psagAccessToken
    , psagUploadType
    , psagBearerToken
    , psagName
    , psagFields
    , psagCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.projects.serviceAccounts.get@ method which the
-- 'ProjectsServiceAccountsGet' request conforms to.
type ProjectsServiceAccountsGetResource =
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
                         QueryParam "alt" AltJSON :>
                           Get '[JSON] ServiceAccount

-- | Gets a ServiceAccount.
--
-- /See:/ 'projectsServiceAccountsGet' smart constructor.
data ProjectsServiceAccountsGet = ProjectsServiceAccountsGet'
    { _psagXgafv :: !(Maybe Xgafv)
    , _psagUploadProtocol :: !(Maybe Text)
    , _psagPp :: !Bool
    , _psagAccessToken :: !(Maybe Text)
    , _psagUploadType :: !(Maybe Text)
    , _psagBearerToken :: !(Maybe Text)
    , _psagName :: !Text
    , _psagFields :: !(Maybe Text)
    , _psagCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsServiceAccountsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psagXgafv'
--
-- * 'psagUploadProtocol'
--
-- * 'psagPp'
--
-- * 'psagAccessToken'
--
-- * 'psagUploadType'
--
-- * 'psagBearerToken'
--
-- * 'psagName'
--
-- * 'psagFields'
--
-- * 'psagCallback'
projectsServiceAccountsGet
    :: Text -- ^ 'psagName'
    -> ProjectsServiceAccountsGet
projectsServiceAccountsGet pPsagName_ = 
    ProjectsServiceAccountsGet'
    { _psagXgafv = Nothing
    , _psagUploadProtocol = Nothing
    , _psagPp = True
    , _psagAccessToken = Nothing
    , _psagUploadType = Nothing
    , _psagBearerToken = Nothing
    , _psagName = pPsagName_
    , _psagFields = Nothing
    , _psagCallback = Nothing
    }

-- | V1 error format.
psagXgafv :: Lens' ProjectsServiceAccountsGet (Maybe Xgafv)
psagXgafv
  = lens _psagXgafv (\ s a -> s{_psagXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
psagUploadProtocol :: Lens' ProjectsServiceAccountsGet (Maybe Text)
psagUploadProtocol
  = lens _psagUploadProtocol
      (\ s a -> s{_psagUploadProtocol = a})

-- | Pretty-print response.
psagPp :: Lens' ProjectsServiceAccountsGet Bool
psagPp = lens _psagPp (\ s a -> s{_psagPp = a})

-- | OAuth access token.
psagAccessToken :: Lens' ProjectsServiceAccountsGet (Maybe Text)
psagAccessToken
  = lens _psagAccessToken
      (\ s a -> s{_psagAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
psagUploadType :: Lens' ProjectsServiceAccountsGet (Maybe Text)
psagUploadType
  = lens _psagUploadType
      (\ s a -> s{_psagUploadType = a})

-- | OAuth bearer token.
psagBearerToken :: Lens' ProjectsServiceAccountsGet (Maybe Text)
psagBearerToken
  = lens _psagBearerToken
      (\ s a -> s{_psagBearerToken = a})

-- | The resource name of the service account in the following format:
-- \`projects\/{PROJECT_ID}\/serviceAccounts\/{ACCOUNT}\`. Using \`-\` as a
-- wildcard for the \`PROJECT_ID\` will infer the project from the account.
-- The \`ACCOUNT\` value can be the \`email\` address or the \`unique_id\`
-- of the service account.
psagName :: Lens' ProjectsServiceAccountsGet Text
psagName = lens _psagName (\ s a -> s{_psagName = a})

-- | Selector specifying which fields to include in a partial response.
psagFields :: Lens' ProjectsServiceAccountsGet (Maybe Text)
psagFields
  = lens _psagFields (\ s a -> s{_psagFields = a})

-- | JSONP
psagCallback :: Lens' ProjectsServiceAccountsGet (Maybe Text)
psagCallback
  = lens _psagCallback (\ s a -> s{_psagCallback = a})

instance GoogleRequest ProjectsServiceAccountsGet
         where
        type Rs ProjectsServiceAccountsGet = ServiceAccount
        type Scopes ProjectsServiceAccountsGet =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsServiceAccountsGet'{..}
          = go _psagName _psagXgafv _psagUploadProtocol
              (Just _psagPp)
              _psagAccessToken
              _psagUploadType
              _psagBearerToken
              _psagCallback
              _psagFields
              (Just AltJSON)
              iAMService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsServiceAccountsGetResource)
                      mempty
