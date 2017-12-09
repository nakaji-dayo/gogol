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
-- Module      : Network.Google.Resource.PubSub.Projects.Snapshots.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an existing snapshot. All messages retained in the snapshot are
-- immediately dropped. After a snapshot is deleted, a new one may be
-- created with the same name, but the new one has no association with the
-- old snapshot or its subscription, unless the same subscription is
-- specified.
--
-- /See:/ <https://cloud.google.com/pubsub/docs Google Cloud Pub/Sub API Reference> for @pubsub.projects.snapshots.delete@.
module Network.Google.Resource.PubSub.Projects.Snapshots.Delete
    (
    -- * REST Resource
      ProjectsSnapshotsDeleteResource

    -- * Creating a Request
    , projectsSnapshotsDelete
    , ProjectsSnapshotsDelete

    -- * Request Lenses
    , proXgafv
    , proSnapshot
    , proUploadProtocol
    , proPp
    , proAccessToken
    , proUploadType
    , proBearerToken
    , proFields
    , proCallback
    ) where

import Network.Google.Prelude
import Network.Google.PubSub.Types

-- | A resource alias for @pubsub.projects.snapshots.delete@ method which the
-- 'ProjectsSnapshotsDelete' request conforms to.
type ProjectsSnapshotsDeleteResource =
     "v1" :>
       Capture "snapshot" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :> Delete '[JSON] Empty

-- | Removes an existing snapshot. All messages retained in the snapshot are
-- immediately dropped. After a snapshot is deleted, a new one may be
-- created with the same name, but the new one has no association with the
-- old snapshot or its subscription, unless the same subscription is
-- specified.
--
-- /See:/ 'projectsSnapshotsDelete' smart constructor.
data ProjectsSnapshotsDelete = ProjectsSnapshotsDelete'
    { _proXgafv :: !(Maybe Xgafv)
    , _proSnapshot :: !Text
    , _proUploadProtocol :: !(Maybe Text)
    , _proPp :: !Bool
    , _proAccessToken :: !(Maybe Text)
    , _proUploadType :: !(Maybe Text)
    , _proBearerToken :: !(Maybe Text)
    , _proFields :: !(Maybe Text)
    , _proCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsSnapshotsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'proXgafv'
--
-- * 'proSnapshot'
--
-- * 'proUploadProtocol'
--
-- * 'proPp'
--
-- * 'proAccessToken'
--
-- * 'proUploadType'
--
-- * 'proBearerToken'
--
-- * 'proFields'
--
-- * 'proCallback'
projectsSnapshotsDelete
    :: Text -- ^ 'proSnapshot'
    -> ProjectsSnapshotsDelete
projectsSnapshotsDelete pProSnapshot_ = 
    ProjectsSnapshotsDelete'
    { _proXgafv = Nothing
    , _proSnapshot = pProSnapshot_
    , _proUploadProtocol = Nothing
    , _proPp = True
    , _proAccessToken = Nothing
    , _proUploadType = Nothing
    , _proBearerToken = Nothing
    , _proFields = Nothing
    , _proCallback = Nothing
    }

-- | V1 error format.
proXgafv :: Lens' ProjectsSnapshotsDelete (Maybe Xgafv)
proXgafv = lens _proXgafv (\ s a -> s{_proXgafv = a})

-- | The name of the snapshot to delete. Format is
-- \`projects\/{project}\/snapshots\/{snap}\`.
proSnapshot :: Lens' ProjectsSnapshotsDelete Text
proSnapshot
  = lens _proSnapshot (\ s a -> s{_proSnapshot = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
proUploadProtocol :: Lens' ProjectsSnapshotsDelete (Maybe Text)
proUploadProtocol
  = lens _proUploadProtocol
      (\ s a -> s{_proUploadProtocol = a})

-- | Pretty-print response.
proPp :: Lens' ProjectsSnapshotsDelete Bool
proPp = lens _proPp (\ s a -> s{_proPp = a})

-- | OAuth access token.
proAccessToken :: Lens' ProjectsSnapshotsDelete (Maybe Text)
proAccessToken
  = lens _proAccessToken
      (\ s a -> s{_proAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
proUploadType :: Lens' ProjectsSnapshotsDelete (Maybe Text)
proUploadType
  = lens _proUploadType
      (\ s a -> s{_proUploadType = a})

-- | OAuth bearer token.
proBearerToken :: Lens' ProjectsSnapshotsDelete (Maybe Text)
proBearerToken
  = lens _proBearerToken
      (\ s a -> s{_proBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
proFields :: Lens' ProjectsSnapshotsDelete (Maybe Text)
proFields
  = lens _proFields (\ s a -> s{_proFields = a})

-- | JSONP
proCallback :: Lens' ProjectsSnapshotsDelete (Maybe Text)
proCallback
  = lens _proCallback (\ s a -> s{_proCallback = a})

instance GoogleRequest ProjectsSnapshotsDelete where
        type Rs ProjectsSnapshotsDelete = Empty
        type Scopes ProjectsSnapshotsDelete =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/pubsub"]
        requestClient ProjectsSnapshotsDelete'{..}
          = go _proSnapshot _proXgafv _proUploadProtocol
              (Just _proPp)
              _proAccessToken
              _proUploadType
              _proBearerToken
              _proCallback
              _proFields
              (Just AltJSON)
              pubSubService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsSnapshotsDeleteResource)
                      mempty
