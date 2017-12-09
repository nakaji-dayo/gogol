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
-- Module      : Network.Google.Resource.IAM.Projects.ServiceAccounts.TestIAMPermissions
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the specified permissions against the IAM access control policy
-- for a ServiceAccount.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.projects.serviceAccounts.testIamPermissions@.
module Network.Google.Resource.IAM.Projects.ServiceAccounts.TestIAMPermissions
    (
    -- * REST Resource
      ProjectsServiceAccountsTestIAMPermissionsResource

    -- * Creating a Request
    , projectsServiceAccountsTestIAMPermissions
    , ProjectsServiceAccountsTestIAMPermissions

    -- * Request Lenses
    , psatipXgafv
    , psatipUploadProtocol
    , psatipPp
    , psatipAccessToken
    , psatipUploadType
    , psatipPayload
    , psatipBearerToken
    , psatipResource
    , psatipFields
    , psatipCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.projects.serviceAccounts.testIamPermissions@ method which the
-- 'ProjectsServiceAccountsTestIAMPermissions' request conforms to.
type ProjectsServiceAccountsTestIAMPermissionsResource
     =
     "v1" :>
       CaptureMode "resource" "testIamPermissions" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] TestIAMPermissionsRequest :>
                             Post '[JSON] TestIAMPermissionsResponse

-- | Tests the specified permissions against the IAM access control policy
-- for a ServiceAccount.
--
-- /See:/ 'projectsServiceAccountsTestIAMPermissions' smart constructor.
data ProjectsServiceAccountsTestIAMPermissions = ProjectsServiceAccountsTestIAMPermissions'
    { _psatipXgafv :: !(Maybe Xgafv)
    , _psatipUploadProtocol :: !(Maybe Text)
    , _psatipPp :: !Bool
    , _psatipAccessToken :: !(Maybe Text)
    , _psatipUploadType :: !(Maybe Text)
    , _psatipPayload :: !TestIAMPermissionsRequest
    , _psatipBearerToken :: !(Maybe Text)
    , _psatipResource :: !Text
    , _psatipFields :: !(Maybe Text)
    , _psatipCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsServiceAccountsTestIAMPermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psatipXgafv'
--
-- * 'psatipUploadProtocol'
--
-- * 'psatipPp'
--
-- * 'psatipAccessToken'
--
-- * 'psatipUploadType'
--
-- * 'psatipPayload'
--
-- * 'psatipBearerToken'
--
-- * 'psatipResource'
--
-- * 'psatipFields'
--
-- * 'psatipCallback'
projectsServiceAccountsTestIAMPermissions
    :: TestIAMPermissionsRequest -- ^ 'psatipPayload'
    -> Text -- ^ 'psatipResource'
    -> ProjectsServiceAccountsTestIAMPermissions
projectsServiceAccountsTestIAMPermissions pPsatipPayload_ pPsatipResource_ = 
    ProjectsServiceAccountsTestIAMPermissions'
    { _psatipXgafv = Nothing
    , _psatipUploadProtocol = Nothing
    , _psatipPp = True
    , _psatipAccessToken = Nothing
    , _psatipUploadType = Nothing
    , _psatipPayload = pPsatipPayload_
    , _psatipBearerToken = Nothing
    , _psatipResource = pPsatipResource_
    , _psatipFields = Nothing
    , _psatipCallback = Nothing
    }

-- | V1 error format.
psatipXgafv :: Lens' ProjectsServiceAccountsTestIAMPermissions (Maybe Xgafv)
psatipXgafv
  = lens _psatipXgafv (\ s a -> s{_psatipXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
psatipUploadProtocol :: Lens' ProjectsServiceAccountsTestIAMPermissions (Maybe Text)
psatipUploadProtocol
  = lens _psatipUploadProtocol
      (\ s a -> s{_psatipUploadProtocol = a})

-- | Pretty-print response.
psatipPp :: Lens' ProjectsServiceAccountsTestIAMPermissions Bool
psatipPp = lens _psatipPp (\ s a -> s{_psatipPp = a})

-- | OAuth access token.
psatipAccessToken :: Lens' ProjectsServiceAccountsTestIAMPermissions (Maybe Text)
psatipAccessToken
  = lens _psatipAccessToken
      (\ s a -> s{_psatipAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
psatipUploadType :: Lens' ProjectsServiceAccountsTestIAMPermissions (Maybe Text)
psatipUploadType
  = lens _psatipUploadType
      (\ s a -> s{_psatipUploadType = a})

-- | Multipart request metadata.
psatipPayload :: Lens' ProjectsServiceAccountsTestIAMPermissions TestIAMPermissionsRequest
psatipPayload
  = lens _psatipPayload
      (\ s a -> s{_psatipPayload = a})

-- | OAuth bearer token.
psatipBearerToken :: Lens' ProjectsServiceAccountsTestIAMPermissions (Maybe Text)
psatipBearerToken
  = lens _psatipBearerToken
      (\ s a -> s{_psatipBearerToken = a})

-- | REQUIRED: The resource for which the policy detail is being requested.
-- See the operation documentation for the appropriate value for this
-- field.
psatipResource :: Lens' ProjectsServiceAccountsTestIAMPermissions Text
psatipResource
  = lens _psatipResource
      (\ s a -> s{_psatipResource = a})

-- | Selector specifying which fields to include in a partial response.
psatipFields :: Lens' ProjectsServiceAccountsTestIAMPermissions (Maybe Text)
psatipFields
  = lens _psatipFields (\ s a -> s{_psatipFields = a})

-- | JSONP
psatipCallback :: Lens' ProjectsServiceAccountsTestIAMPermissions (Maybe Text)
psatipCallback
  = lens _psatipCallback
      (\ s a -> s{_psatipCallback = a})

instance GoogleRequest
         ProjectsServiceAccountsTestIAMPermissions where
        type Rs ProjectsServiceAccountsTestIAMPermissions =
             TestIAMPermissionsResponse
        type Scopes ProjectsServiceAccountsTestIAMPermissions
             = '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient
          ProjectsServiceAccountsTestIAMPermissions'{..}
          = go _psatipResource _psatipXgafv
              _psatipUploadProtocol
              (Just _psatipPp)
              _psatipAccessToken
              _psatipUploadType
              _psatipBearerToken
              _psatipCallback
              _psatipFields
              (Just AltJSON)
              _psatipPayload
              iAMService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           ProjectsServiceAccountsTestIAMPermissionsResource)
                      mempty
