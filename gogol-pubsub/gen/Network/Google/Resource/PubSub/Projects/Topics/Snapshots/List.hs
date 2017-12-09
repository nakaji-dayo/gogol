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
-- Module      : Network.Google.Resource.PubSub.Projects.Topics.Snapshots.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the snapshots on this topic.
--
-- /See:/ <https://cloud.google.com/pubsub/docs Google Cloud Pub/Sub API Reference> for @pubsub.projects.topics.snapshots.list@.
module Network.Google.Resource.PubSub.Projects.Topics.Snapshots.List
    (
    -- * REST Resource
      ProjectsTopicsSnapshotsListResource

    -- * Creating a Request
    , projectsTopicsSnapshotsList
    , ProjectsTopicsSnapshotsList

    -- * Request Lenses
    , ptsltXgafv
    , ptsltUploadProtocol
    , ptsltPp
    , ptsltAccessToken
    , ptsltUploadType
    , ptsltTopic
    , ptsltBearerToken
    , ptsltPageToken
    , ptsltPageSize
    , ptsltFields
    , ptsltCallback
    ) where

import Network.Google.Prelude
import Network.Google.PubSub.Types

-- | A resource alias for @pubsub.projects.topics.snapshots.list@ method which the
-- 'ProjectsTopicsSnapshotsList' request conforms to.
type ProjectsTopicsSnapshotsListResource =
     "v1" :>
       Capture "topic" Text :>
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
                                 Get '[JSON] ListTopicSnapshotsResponse

-- | Lists the names of the snapshots on this topic.
--
-- /See:/ 'projectsTopicsSnapshotsList' smart constructor.
data ProjectsTopicsSnapshotsList = ProjectsTopicsSnapshotsList'
    { _ptsltXgafv :: !(Maybe Xgafv)
    , _ptsltUploadProtocol :: !(Maybe Text)
    , _ptsltPp :: !Bool
    , _ptsltAccessToken :: !(Maybe Text)
    , _ptsltUploadType :: !(Maybe Text)
    , _ptsltTopic :: !Text
    , _ptsltBearerToken :: !(Maybe Text)
    , _ptsltPageToken :: !(Maybe Text)
    , _ptsltPageSize :: !(Maybe (Textual Int32))
    , _ptsltFields :: !(Maybe Text)
    , _ptsltCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsTopicsSnapshotsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptsltXgafv'
--
-- * 'ptsltUploadProtocol'
--
-- * 'ptsltPp'
--
-- * 'ptsltAccessToken'
--
-- * 'ptsltUploadType'
--
-- * 'ptsltTopic'
--
-- * 'ptsltBearerToken'
--
-- * 'ptsltPageToken'
--
-- * 'ptsltPageSize'
--
-- * 'ptsltFields'
--
-- * 'ptsltCallback'
projectsTopicsSnapshotsList
    :: Text -- ^ 'ptsltTopic'
    -> ProjectsTopicsSnapshotsList
projectsTopicsSnapshotsList pPtsltTopic_ = 
    ProjectsTopicsSnapshotsList'
    { _ptsltXgafv = Nothing
    , _ptsltUploadProtocol = Nothing
    , _ptsltPp = True
    , _ptsltAccessToken = Nothing
    , _ptsltUploadType = Nothing
    , _ptsltTopic = pPtsltTopic_
    , _ptsltBearerToken = Nothing
    , _ptsltPageToken = Nothing
    , _ptsltPageSize = Nothing
    , _ptsltFields = Nothing
    , _ptsltCallback = Nothing
    }

-- | V1 error format.
ptsltXgafv :: Lens' ProjectsTopicsSnapshotsList (Maybe Xgafv)
ptsltXgafv
  = lens _ptsltXgafv (\ s a -> s{_ptsltXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
ptsltUploadProtocol :: Lens' ProjectsTopicsSnapshotsList (Maybe Text)
ptsltUploadProtocol
  = lens _ptsltUploadProtocol
      (\ s a -> s{_ptsltUploadProtocol = a})

-- | Pretty-print response.
ptsltPp :: Lens' ProjectsTopicsSnapshotsList Bool
ptsltPp = lens _ptsltPp (\ s a -> s{_ptsltPp = a})

-- | OAuth access token.
ptsltAccessToken :: Lens' ProjectsTopicsSnapshotsList (Maybe Text)
ptsltAccessToken
  = lens _ptsltAccessToken
      (\ s a -> s{_ptsltAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
ptsltUploadType :: Lens' ProjectsTopicsSnapshotsList (Maybe Text)
ptsltUploadType
  = lens _ptsltUploadType
      (\ s a -> s{_ptsltUploadType = a})

-- | The name of the topic that snapshots are attached to. Format is
-- \`projects\/{project}\/topics\/{topic}\`.
ptsltTopic :: Lens' ProjectsTopicsSnapshotsList Text
ptsltTopic
  = lens _ptsltTopic (\ s a -> s{_ptsltTopic = a})

-- | OAuth bearer token.
ptsltBearerToken :: Lens' ProjectsTopicsSnapshotsList (Maybe Text)
ptsltBearerToken
  = lens _ptsltBearerToken
      (\ s a -> s{_ptsltBearerToken = a})

-- | The value returned by the last \`ListTopicSnapshotsResponse\`; indicates
-- that this is a continuation of a prior \`ListTopicSnapshots\` call, and
-- that the system should return the next page of data.
ptsltPageToken :: Lens' ProjectsTopicsSnapshotsList (Maybe Text)
ptsltPageToken
  = lens _ptsltPageToken
      (\ s a -> s{_ptsltPageToken = a})

-- | Maximum number of snapshot names to return.
ptsltPageSize :: Lens' ProjectsTopicsSnapshotsList (Maybe Int32)
ptsltPageSize
  = lens _ptsltPageSize
      (\ s a -> s{_ptsltPageSize = a})
      . mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
ptsltFields :: Lens' ProjectsTopicsSnapshotsList (Maybe Text)
ptsltFields
  = lens _ptsltFields (\ s a -> s{_ptsltFields = a})

-- | JSONP
ptsltCallback :: Lens' ProjectsTopicsSnapshotsList (Maybe Text)
ptsltCallback
  = lens _ptsltCallback
      (\ s a -> s{_ptsltCallback = a})

instance GoogleRequest ProjectsTopicsSnapshotsList
         where
        type Rs ProjectsTopicsSnapshotsList =
             ListTopicSnapshotsResponse
        type Scopes ProjectsTopicsSnapshotsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/pubsub"]
        requestClient ProjectsTopicsSnapshotsList'{..}
          = go _ptsltTopic _ptsltXgafv _ptsltUploadProtocol
              (Just _ptsltPp)
              _ptsltAccessToken
              _ptsltUploadType
              _ptsltBearerToken
              _ptsltPageToken
              _ptsltPageSize
              _ptsltCallback
              _ptsltFields
              (Just AltJSON)
              pubSubService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsTopicsSnapshotsListResource)
                      mempty
