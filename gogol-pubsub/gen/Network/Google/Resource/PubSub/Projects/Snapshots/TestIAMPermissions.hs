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
-- Module      : Network.Google.Resource.PubSub.Projects.Snapshots.TestIAMPermissions
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
-- /See:/ <https://cloud.google.com/pubsub/docs Google Cloud Pub/Sub API Reference> for @pubsub.projects.snapshots.testIamPermissions@.
module Network.Google.Resource.PubSub.Projects.Snapshots.TestIAMPermissions
    (
    -- * REST Resource
      ProjectsSnapshotsTestIAMPermissionsResource

    -- * Creating a Request
    , projectsSnapshotsTestIAMPermissions
    , ProjectsSnapshotsTestIAMPermissions

    -- * Request Lenses
    , pstipXgafv
    , pstipUploadProtocol
    , pstipPp
    , pstipAccessToken
    , pstipUploadType
    , pstipPayload
    , pstipBearerToken
    , pstipResource
    , pstipFields
    , pstipCallback
    ) where

import Network.Google.Prelude
import Network.Google.PubSub.Types

-- | A resource alias for @pubsub.projects.snapshots.testIamPermissions@ method which the
-- 'ProjectsSnapshotsTestIAMPermissions' request conforms to.
type ProjectsSnapshotsTestIAMPermissionsResource =
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

-- | Returns permissions that a caller has on the specified resource. If the
-- resource does not exist, this will return an empty set of permissions,
-- not a NOT_FOUND error. Note: This operation is designed to be used for
-- building permission-aware UIs and command-line tools, not for
-- authorization checking. This operation may \"fail open\" without
-- warning.
--
-- /See:/ 'projectsSnapshotsTestIAMPermissions' smart constructor.
data ProjectsSnapshotsTestIAMPermissions = ProjectsSnapshotsTestIAMPermissions'
    { _pstipXgafv :: !(Maybe Xgafv)
    , _pstipUploadProtocol :: !(Maybe Text)
    , _pstipPp :: !Bool
    , _pstipAccessToken :: !(Maybe Text)
    , _pstipUploadType :: !(Maybe Text)
    , _pstipPayload :: !TestIAMPermissionsRequest
    , _pstipBearerToken :: !(Maybe Text)
    , _pstipResource :: !Text
    , _pstipFields :: !(Maybe Text)
    , _pstipCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsSnapshotsTestIAMPermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pstipXgafv'
--
-- * 'pstipUploadProtocol'
--
-- * 'pstipPp'
--
-- * 'pstipAccessToken'
--
-- * 'pstipUploadType'
--
-- * 'pstipPayload'
--
-- * 'pstipBearerToken'
--
-- * 'pstipResource'
--
-- * 'pstipFields'
--
-- * 'pstipCallback'
projectsSnapshotsTestIAMPermissions
    :: TestIAMPermissionsRequest -- ^ 'pstipPayload'
    -> Text -- ^ 'pstipResource'
    -> ProjectsSnapshotsTestIAMPermissions
projectsSnapshotsTestIAMPermissions pPstipPayload_ pPstipResource_ = 
    ProjectsSnapshotsTestIAMPermissions'
    { _pstipXgafv = Nothing
    , _pstipUploadProtocol = Nothing
    , _pstipPp = True
    , _pstipAccessToken = Nothing
    , _pstipUploadType = Nothing
    , _pstipPayload = pPstipPayload_
    , _pstipBearerToken = Nothing
    , _pstipResource = pPstipResource_
    , _pstipFields = Nothing
    , _pstipCallback = Nothing
    }

-- | V1 error format.
pstipXgafv :: Lens' ProjectsSnapshotsTestIAMPermissions (Maybe Xgafv)
pstipXgafv
  = lens _pstipXgafv (\ s a -> s{_pstipXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pstipUploadProtocol :: Lens' ProjectsSnapshotsTestIAMPermissions (Maybe Text)
pstipUploadProtocol
  = lens _pstipUploadProtocol
      (\ s a -> s{_pstipUploadProtocol = a})

-- | Pretty-print response.
pstipPp :: Lens' ProjectsSnapshotsTestIAMPermissions Bool
pstipPp = lens _pstipPp (\ s a -> s{_pstipPp = a})

-- | OAuth access token.
pstipAccessToken :: Lens' ProjectsSnapshotsTestIAMPermissions (Maybe Text)
pstipAccessToken
  = lens _pstipAccessToken
      (\ s a -> s{_pstipAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pstipUploadType :: Lens' ProjectsSnapshotsTestIAMPermissions (Maybe Text)
pstipUploadType
  = lens _pstipUploadType
      (\ s a -> s{_pstipUploadType = a})

-- | Multipart request metadata.
pstipPayload :: Lens' ProjectsSnapshotsTestIAMPermissions TestIAMPermissionsRequest
pstipPayload
  = lens _pstipPayload (\ s a -> s{_pstipPayload = a})

-- | OAuth bearer token.
pstipBearerToken :: Lens' ProjectsSnapshotsTestIAMPermissions (Maybe Text)
pstipBearerToken
  = lens _pstipBearerToken
      (\ s a -> s{_pstipBearerToken = a})

-- | REQUIRED: The resource for which the policy detail is being requested.
-- See the operation documentation for the appropriate value for this
-- field.
pstipResource :: Lens' ProjectsSnapshotsTestIAMPermissions Text
pstipResource
  = lens _pstipResource
      (\ s a -> s{_pstipResource = a})

-- | Selector specifying which fields to include in a partial response.
pstipFields :: Lens' ProjectsSnapshotsTestIAMPermissions (Maybe Text)
pstipFields
  = lens _pstipFields (\ s a -> s{_pstipFields = a})

-- | JSONP
pstipCallback :: Lens' ProjectsSnapshotsTestIAMPermissions (Maybe Text)
pstipCallback
  = lens _pstipCallback
      (\ s a -> s{_pstipCallback = a})

instance GoogleRequest
         ProjectsSnapshotsTestIAMPermissions where
        type Rs ProjectsSnapshotsTestIAMPermissions =
             TestIAMPermissionsResponse
        type Scopes ProjectsSnapshotsTestIAMPermissions =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/pubsub"]
        requestClient
          ProjectsSnapshotsTestIAMPermissions'{..}
          = go _pstipResource _pstipXgafv _pstipUploadProtocol
              (Just _pstipPp)
              _pstipAccessToken
              _pstipUploadType
              _pstipBearerToken
              _pstipCallback
              _pstipFields
              (Just AltJSON)
              _pstipPayload
              pubSubService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsSnapshotsTestIAMPermissionsResource)
                      mempty
