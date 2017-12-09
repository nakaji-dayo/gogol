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
-- Module      : Network.Google.Resource.PubSub.Projects.Snapshots.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot from the requested subscription. If the snapshot
-- already exists, returns \`ALREADY_EXISTS\`. If the requested
-- subscription doesn\'t exist, returns \`NOT_FOUND\`. If the backlog in
-- the subscription is too old -- and the resulting snapshot would expire
-- in less than 1 hour -- then \`FAILED_PRECONDITION\` is returned. See
-- also the \`Snapshot.expire_time\` field. If the name is not provided in
-- the request, the server will assign a random name for this snapshot on
-- the same project as the subscription, conforming to the [resource name
-- format](https:\/\/cloud.google.com\/pubsub\/docs\/overview#names). The
-- generated name is populated in the returned Snapshot object. Note that
-- for REST API requests, you must specify a name in the request.
--
-- /See:/ <https://cloud.google.com/pubsub/docs Google Cloud Pub/Sub API Reference> for @pubsub.projects.snapshots.create@.
module Network.Google.Resource.PubSub.Projects.Snapshots.Create
    (
    -- * REST Resource
      ProjectsSnapshotsCreateResource

    -- * Creating a Request
    , projectsSnapshotsCreate
    , ProjectsSnapshotsCreate

    -- * Request Lenses
    , pXgafv
    , pUploadProtocol
    , pPp
    , pAccessToken
    , pUploadType
    , pPayload
    , pBearerToken
    , pName
    , pFields
    , pCallback
    ) where

import Network.Google.Prelude
import Network.Google.PubSub.Types

-- | A resource alias for @pubsub.projects.snapshots.create@ method which the
-- 'ProjectsSnapshotsCreate' request conforms to.
type ProjectsSnapshotsCreateResource =
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
                           ReqBody '[JSON] CreateSnapshotRequest :>
                             Put '[JSON] Snapshot

-- | Creates a snapshot from the requested subscription. If the snapshot
-- already exists, returns \`ALREADY_EXISTS\`. If the requested
-- subscription doesn\'t exist, returns \`NOT_FOUND\`. If the backlog in
-- the subscription is too old -- and the resulting snapshot would expire
-- in less than 1 hour -- then \`FAILED_PRECONDITION\` is returned. See
-- also the \`Snapshot.expire_time\` field. If the name is not provided in
-- the request, the server will assign a random name for this snapshot on
-- the same project as the subscription, conforming to the [resource name
-- format](https:\/\/cloud.google.com\/pubsub\/docs\/overview#names). The
-- generated name is populated in the returned Snapshot object. Note that
-- for REST API requests, you must specify a name in the request.
--
-- /See:/ 'projectsSnapshotsCreate' smart constructor.
data ProjectsSnapshotsCreate = ProjectsSnapshotsCreate'
    { _pXgafv :: !(Maybe Xgafv)
    , _pUploadProtocol :: !(Maybe Text)
    , _pPp :: !Bool
    , _pAccessToken :: !(Maybe Text)
    , _pUploadType :: !(Maybe Text)
    , _pPayload :: !CreateSnapshotRequest
    , _pBearerToken :: !(Maybe Text)
    , _pName :: !Text
    , _pFields :: !(Maybe Text)
    , _pCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsSnapshotsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pXgafv'
--
-- * 'pUploadProtocol'
--
-- * 'pPp'
--
-- * 'pAccessToken'
--
-- * 'pUploadType'
--
-- * 'pPayload'
--
-- * 'pBearerToken'
--
-- * 'pName'
--
-- * 'pFields'
--
-- * 'pCallback'
projectsSnapshotsCreate
    :: CreateSnapshotRequest -- ^ 'pPayload'
    -> Text -- ^ 'pName'
    -> ProjectsSnapshotsCreate
projectsSnapshotsCreate pPPayload_ pPName_ = 
    ProjectsSnapshotsCreate'
    { _pXgafv = Nothing
    , _pUploadProtocol = Nothing
    , _pPp = True
    , _pAccessToken = Nothing
    , _pUploadType = Nothing
    , _pPayload = pPPayload_
    , _pBearerToken = Nothing
    , _pName = pPName_
    , _pFields = Nothing
    , _pCallback = Nothing
    }

-- | V1 error format.
pXgafv :: Lens' ProjectsSnapshotsCreate (Maybe Xgafv)
pXgafv = lens _pXgafv (\ s a -> s{_pXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pUploadProtocol :: Lens' ProjectsSnapshotsCreate (Maybe Text)
pUploadProtocol
  = lens _pUploadProtocol
      (\ s a -> s{_pUploadProtocol = a})

-- | Pretty-print response.
pPp :: Lens' ProjectsSnapshotsCreate Bool
pPp = lens _pPp (\ s a -> s{_pPp = a})

-- | OAuth access token.
pAccessToken :: Lens' ProjectsSnapshotsCreate (Maybe Text)
pAccessToken
  = lens _pAccessToken (\ s a -> s{_pAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pUploadType :: Lens' ProjectsSnapshotsCreate (Maybe Text)
pUploadType
  = lens _pUploadType (\ s a -> s{_pUploadType = a})

-- | Multipart request metadata.
pPayload :: Lens' ProjectsSnapshotsCreate CreateSnapshotRequest
pPayload = lens _pPayload (\ s a -> s{_pPayload = a})

-- | OAuth bearer token.
pBearerToken :: Lens' ProjectsSnapshotsCreate (Maybe Text)
pBearerToken
  = lens _pBearerToken (\ s a -> s{_pBearerToken = a})

-- | Optional user-provided name for this snapshot. If the name is not
-- provided in the request, the server will assign a random name for this
-- snapshot on the same project as the subscription. Note that for REST API
-- requests, you must specify a name. Format is
-- \`projects\/{project}\/snapshots\/{snap}\`.
pName :: Lens' ProjectsSnapshotsCreate Text
pName = lens _pName (\ s a -> s{_pName = a})

-- | Selector specifying which fields to include in a partial response.
pFields :: Lens' ProjectsSnapshotsCreate (Maybe Text)
pFields = lens _pFields (\ s a -> s{_pFields = a})

-- | JSONP
pCallback :: Lens' ProjectsSnapshotsCreate (Maybe Text)
pCallback
  = lens _pCallback (\ s a -> s{_pCallback = a})

instance GoogleRequest ProjectsSnapshotsCreate where
        type Rs ProjectsSnapshotsCreate = Snapshot
        type Scopes ProjectsSnapshotsCreate =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/pubsub"]
        requestClient ProjectsSnapshotsCreate'{..}
          = go _pName _pXgafv _pUploadProtocol (Just _pPp)
              _pAccessToken
              _pUploadType
              _pBearerToken
              _pCallback
              _pFields
              (Just AltJSON)
              _pPayload
              pubSubService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsSnapshotsCreateResource)
                      mempty
