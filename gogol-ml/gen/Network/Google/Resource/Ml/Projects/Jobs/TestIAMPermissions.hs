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
-- Module      : Network.Google.Resource.Ml.Projects.Jobs.TestIAMPermissions
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns permissions that a caller has on the specified resource. If the
-- resource does not exist, this will return an empty set of permissions,
-- not a NOT_FOUND error. Note: This operation is designed to be used for
-- building permission-aware UIs and command-line tools, not for
-- authorization checking. This operation may \"fail open\" without
-- warning.
--
-- /See:/ <https://cloud.google.com/ml/ Google Cloud Machine Learning Engine Reference> for @ml.projects.jobs.testIamPermissions@.
module Network.Google.Resource.Ml.Projects.Jobs.TestIAMPermissions
    (
    -- * REST Resource
      ProjectsJobsTestIAMPermissionsResource

    -- * Creating a Request
    , projectsJobsTestIAMPermissions
    , ProjectsJobsTestIAMPermissions

    -- * Request Lenses
    , pjtipXgafv
    , pjtipUploadProtocol
    , pjtipPp
    , pjtipAccessToken
    , pjtipUploadType
    , pjtipPayload
    , pjtipBearerToken
    , pjtipResource
    , pjtipFields
    , pjtipCallback
    ) where

import Network.Google.MachineLearning.Types
import Network.Google.Prelude

-- | A resource alias for @ml.projects.jobs.testIamPermissions@ method which the
-- 'ProjectsJobsTestIAMPermissions' request conforms to.
type ProjectsJobsTestIAMPermissionsResource =
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
                           ReqBody '[JSON]
                             GoogleIAMV1__TestIAMPermissionsRequest
                             :>
                             Post '[JSON]
                               GoogleIAMV1__TestIAMPermissionsResponse

-- | Returns permissions that a caller has on the specified resource. If the
-- resource does not exist, this will return an empty set of permissions,
-- not a NOT_FOUND error. Note: This operation is designed to be used for
-- building permission-aware UIs and command-line tools, not for
-- authorization checking. This operation may \"fail open\" without
-- warning.
--
-- /See:/ 'projectsJobsTestIAMPermissions' smart constructor.
data ProjectsJobsTestIAMPermissions = ProjectsJobsTestIAMPermissions'
    { _pjtipXgafv :: !(Maybe Xgafv)
    , _pjtipUploadProtocol :: !(Maybe Text)
    , _pjtipPp :: !Bool
    , _pjtipAccessToken :: !(Maybe Text)
    , _pjtipUploadType :: !(Maybe Text)
    , _pjtipPayload :: !GoogleIAMV1__TestIAMPermissionsRequest
    , _pjtipBearerToken :: !(Maybe Text)
    , _pjtipResource :: !Text
    , _pjtipFields :: !(Maybe Text)
    , _pjtipCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsJobsTestIAMPermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pjtipXgafv'
--
-- * 'pjtipUploadProtocol'
--
-- * 'pjtipPp'
--
-- * 'pjtipAccessToken'
--
-- * 'pjtipUploadType'
--
-- * 'pjtipPayload'
--
-- * 'pjtipBearerToken'
--
-- * 'pjtipResource'
--
-- * 'pjtipFields'
--
-- * 'pjtipCallback'
projectsJobsTestIAMPermissions
    :: GoogleIAMV1__TestIAMPermissionsRequest -- ^ 'pjtipPayload'
    -> Text -- ^ 'pjtipResource'
    -> ProjectsJobsTestIAMPermissions
projectsJobsTestIAMPermissions pPjtipPayload_ pPjtipResource_ = 
    ProjectsJobsTestIAMPermissions'
    { _pjtipXgafv = Nothing
    , _pjtipUploadProtocol = Nothing
    , _pjtipPp = True
    , _pjtipAccessToken = Nothing
    , _pjtipUploadType = Nothing
    , _pjtipPayload = pPjtipPayload_
    , _pjtipBearerToken = Nothing
    , _pjtipResource = pPjtipResource_
    , _pjtipFields = Nothing
    , _pjtipCallback = Nothing
    }

-- | V1 error format.
pjtipXgafv :: Lens' ProjectsJobsTestIAMPermissions (Maybe Xgafv)
pjtipXgafv
  = lens _pjtipXgafv (\ s a -> s{_pjtipXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pjtipUploadProtocol :: Lens' ProjectsJobsTestIAMPermissions (Maybe Text)
pjtipUploadProtocol
  = lens _pjtipUploadProtocol
      (\ s a -> s{_pjtipUploadProtocol = a})

-- | Pretty-print response.
pjtipPp :: Lens' ProjectsJobsTestIAMPermissions Bool
pjtipPp = lens _pjtipPp (\ s a -> s{_pjtipPp = a})

-- | OAuth access token.
pjtipAccessToken :: Lens' ProjectsJobsTestIAMPermissions (Maybe Text)
pjtipAccessToken
  = lens _pjtipAccessToken
      (\ s a -> s{_pjtipAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pjtipUploadType :: Lens' ProjectsJobsTestIAMPermissions (Maybe Text)
pjtipUploadType
  = lens _pjtipUploadType
      (\ s a -> s{_pjtipUploadType = a})

-- | Multipart request metadata.
pjtipPayload :: Lens' ProjectsJobsTestIAMPermissions GoogleIAMV1__TestIAMPermissionsRequest
pjtipPayload
  = lens _pjtipPayload (\ s a -> s{_pjtipPayload = a})

-- | OAuth bearer token.
pjtipBearerToken :: Lens' ProjectsJobsTestIAMPermissions (Maybe Text)
pjtipBearerToken
  = lens _pjtipBearerToken
      (\ s a -> s{_pjtipBearerToken = a})

-- | REQUIRED: The resource for which the policy detail is being requested.
-- See the operation documentation for the appropriate value for this
-- field.
pjtipResource :: Lens' ProjectsJobsTestIAMPermissions Text
pjtipResource
  = lens _pjtipResource
      (\ s a -> s{_pjtipResource = a})

-- | Selector specifying which fields to include in a partial response.
pjtipFields :: Lens' ProjectsJobsTestIAMPermissions (Maybe Text)
pjtipFields
  = lens _pjtipFields (\ s a -> s{_pjtipFields = a})

-- | JSONP
pjtipCallback :: Lens' ProjectsJobsTestIAMPermissions (Maybe Text)
pjtipCallback
  = lens _pjtipCallback
      (\ s a -> s{_pjtipCallback = a})

instance GoogleRequest ProjectsJobsTestIAMPermissions
         where
        type Rs ProjectsJobsTestIAMPermissions =
             GoogleIAMV1__TestIAMPermissionsResponse
        type Scopes ProjectsJobsTestIAMPermissions =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsJobsTestIAMPermissions'{..}
          = go _pjtipResource _pjtipXgafv _pjtipUploadProtocol
              (Just _pjtipPp)
              _pjtipAccessToken
              _pjtipUploadType
              _pjtipBearerToken
              _pjtipCallback
              _pjtipFields
              (Just AltJSON)
              _pjtipPayload
              machineLearningService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsJobsTestIAMPermissionsResource)
                      mempty
