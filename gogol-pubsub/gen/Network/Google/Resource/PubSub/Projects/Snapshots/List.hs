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
-- Module      : Network.Google.Resource.PubSub.Projects.Snapshots.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing snapshots.
--
-- /See:/ <https://cloud.google.com/pubsub/docs Google Cloud Pub/Sub API Reference> for @pubsub.projects.snapshots.list@.
module Network.Google.Resource.PubSub.Projects.Snapshots.List
    (
    -- * REST Resource
      ProjectsSnapshotsListResource

    -- * Creating a Request
    , projectsSnapshotsList
    , ProjectsSnapshotsList

    -- * Request Lenses
    , pslsXgafv
    , pslsUploadProtocol
    , pslsProject
    , pslsPp
    , pslsAccessToken
    , pslsUploadType
    , pslsBearerToken
    , pslsPageToken
    , pslsPageSize
    , pslsFields
    , pslsCallback
    ) where

import Network.Google.Prelude
import Network.Google.PubSub.Types

-- | A resource alias for @pubsub.projects.snapshots.list@ method which the
-- 'ProjectsSnapshotsList' request conforms to.
type ProjectsSnapshotsListResource =
     "v1" :>
       Capture "project" Text :>
         "snapshots" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "pageSize" (Textual Int32) :>
                           QueryParam "callback" Text :>
                             QueryParam "fields" Text :>
                               QueryParam "alt" AltJSON :>
                                 Get '[JSON] ListSnapshotsResponse

-- | Lists the existing snapshots.
--
-- /See:/ 'projectsSnapshotsList' smart constructor.
data ProjectsSnapshotsList = ProjectsSnapshotsList'
    { _pslsXgafv :: !(Maybe Xgafv)
    , _pslsUploadProtocol :: !(Maybe Text)
    , _pslsProject :: !Text
    , _pslsPp :: !Bool
    , _pslsAccessToken :: !(Maybe Text)
    , _pslsUploadType :: !(Maybe Text)
    , _pslsBearerToken :: !(Maybe Text)
    , _pslsPageToken :: !(Maybe Text)
    , _pslsPageSize :: !(Maybe (Textual Int32))
    , _pslsFields :: !(Maybe Text)
    , _pslsCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsSnapshotsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pslsXgafv'
--
-- * 'pslsUploadProtocol'
--
-- * 'pslsProject'
--
-- * 'pslsPp'
--
-- * 'pslsAccessToken'
--
-- * 'pslsUploadType'
--
-- * 'pslsBearerToken'
--
-- * 'pslsPageToken'
--
-- * 'pslsPageSize'
--
-- * 'pslsFields'
--
-- * 'pslsCallback'
projectsSnapshotsList
    :: Text -- ^ 'pslsProject'
    -> ProjectsSnapshotsList
projectsSnapshotsList pPslsProject_ = 
    ProjectsSnapshotsList'
    { _pslsXgafv = Nothing
    , _pslsUploadProtocol = Nothing
    , _pslsProject = pPslsProject_
    , _pslsPp = True
    , _pslsAccessToken = Nothing
    , _pslsUploadType = Nothing
    , _pslsBearerToken = Nothing
    , _pslsPageToken = Nothing
    , _pslsPageSize = Nothing
    , _pslsFields = Nothing
    , _pslsCallback = Nothing
    }

-- | V1 error format.
pslsXgafv :: Lens' ProjectsSnapshotsList (Maybe Xgafv)
pslsXgafv
  = lens _pslsXgafv (\ s a -> s{_pslsXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pslsUploadProtocol :: Lens' ProjectsSnapshotsList (Maybe Text)
pslsUploadProtocol
  = lens _pslsUploadProtocol
      (\ s a -> s{_pslsUploadProtocol = a})

-- | The name of the cloud project that snapshots belong to. Format is
-- \`projects\/{project}\`.
pslsProject :: Lens' ProjectsSnapshotsList Text
pslsProject
  = lens _pslsProject (\ s a -> s{_pslsProject = a})

-- | Pretty-print response.
pslsPp :: Lens' ProjectsSnapshotsList Bool
pslsPp = lens _pslsPp (\ s a -> s{_pslsPp = a})

-- | OAuth access token.
pslsAccessToken :: Lens' ProjectsSnapshotsList (Maybe Text)
pslsAccessToken
  = lens _pslsAccessToken
      (\ s a -> s{_pslsAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pslsUploadType :: Lens' ProjectsSnapshotsList (Maybe Text)
pslsUploadType
  = lens _pslsUploadType
      (\ s a -> s{_pslsUploadType = a})

-- | OAuth bearer token.
pslsBearerToken :: Lens' ProjectsSnapshotsList (Maybe Text)
pslsBearerToken
  = lens _pslsBearerToken
      (\ s a -> s{_pslsBearerToken = a})

-- | The value returned by the last \`ListSnapshotsResponse\`; indicates that
-- this is a continuation of a prior \`ListSnapshots\` call, and that the
-- system should return the next page of data.
pslsPageToken :: Lens' ProjectsSnapshotsList (Maybe Text)
pslsPageToken
  = lens _pslsPageToken
      (\ s a -> s{_pslsPageToken = a})

-- | Maximum number of snapshots to return.
pslsPageSize :: Lens' ProjectsSnapshotsList (Maybe Int32)
pslsPageSize
  = lens _pslsPageSize (\ s a -> s{_pslsPageSize = a})
      . mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
pslsFields :: Lens' ProjectsSnapshotsList (Maybe Text)
pslsFields
  = lens _pslsFields (\ s a -> s{_pslsFields = a})

-- | JSONP
pslsCallback :: Lens' ProjectsSnapshotsList (Maybe Text)
pslsCallback
  = lens _pslsCallback (\ s a -> s{_pslsCallback = a})

instance GoogleRequest ProjectsSnapshotsList where
        type Rs ProjectsSnapshotsList = ListSnapshotsResponse
        type Scopes ProjectsSnapshotsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/pubsub"]
        requestClient ProjectsSnapshotsList'{..}
          = go _pslsProject _pslsXgafv _pslsUploadProtocol
              (Just _pslsPp)
              _pslsAccessToken
              _pslsUploadType
              _pslsBearerToken
              _pslsPageToken
              _pslsPageSize
              _pslsCallback
              _pslsFields
              (Just AltJSON)
              pubSubService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsSnapshotsListResource)
                      mempty
