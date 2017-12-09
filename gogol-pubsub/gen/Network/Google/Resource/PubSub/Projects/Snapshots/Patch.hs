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
-- Module      : Network.Google.Resource.PubSub.Projects.Snapshots.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing snapshot. Note that certain properties of a snapshot
-- are not modifiable.
--
-- /See:/ <https://cloud.google.com/pubsub/docs Google Cloud Pub/Sub API Reference> for @pubsub.projects.snapshots.patch@.
module Network.Google.Resource.PubSub.Projects.Snapshots.Patch
    (
    -- * REST Resource
      ProjectsSnapshotsPatchResource

    -- * Creating a Request
    , projectsSnapshotsPatch
    , ProjectsSnapshotsPatch

    -- * Request Lenses
    , ppXgafv
    , ppUploadProtocol
    , ppPp
    , ppAccessToken
    , ppUploadType
    , ppPayload
    , ppBearerToken
    , ppName
    , ppFields
    , ppCallback
    ) where

import Network.Google.Prelude
import Network.Google.PubSub.Types

-- | A resource alias for @pubsub.projects.snapshots.patch@ method which the
-- 'ProjectsSnapshotsPatch' request conforms to.
type ProjectsSnapshotsPatchResource =
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
                           ReqBody '[JSON] UpdateSnapshotRequest :>
                             Patch '[JSON] Snapshot

-- | Updates an existing snapshot. Note that certain properties of a snapshot
-- are not modifiable.
--
-- /See:/ 'projectsSnapshotsPatch' smart constructor.
data ProjectsSnapshotsPatch = ProjectsSnapshotsPatch'
    { _ppXgafv :: !(Maybe Xgafv)
    , _ppUploadProtocol :: !(Maybe Text)
    , _ppPp :: !Bool
    , _ppAccessToken :: !(Maybe Text)
    , _ppUploadType :: !(Maybe Text)
    , _ppPayload :: !UpdateSnapshotRequest
    , _ppBearerToken :: !(Maybe Text)
    , _ppName :: !Text
    , _ppFields :: !(Maybe Text)
    , _ppCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsSnapshotsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppXgafv'
--
-- * 'ppUploadProtocol'
--
-- * 'ppPp'
--
-- * 'ppAccessToken'
--
-- * 'ppUploadType'
--
-- * 'ppPayload'
--
-- * 'ppBearerToken'
--
-- * 'ppName'
--
-- * 'ppFields'
--
-- * 'ppCallback'
projectsSnapshotsPatch
    :: UpdateSnapshotRequest -- ^ 'ppPayload'
    -> Text -- ^ 'ppName'
    -> ProjectsSnapshotsPatch
projectsSnapshotsPatch pPpPayload_ pPpName_ = 
    ProjectsSnapshotsPatch'
    { _ppXgafv = Nothing
    , _ppUploadProtocol = Nothing
    , _ppPp = True
    , _ppAccessToken = Nothing
    , _ppUploadType = Nothing
    , _ppPayload = pPpPayload_
    , _ppBearerToken = Nothing
    , _ppName = pPpName_
    , _ppFields = Nothing
    , _ppCallback = Nothing
    }

-- | V1 error format.
ppXgafv :: Lens' ProjectsSnapshotsPatch (Maybe Xgafv)
ppXgafv = lens _ppXgafv (\ s a -> s{_ppXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
ppUploadProtocol :: Lens' ProjectsSnapshotsPatch (Maybe Text)
ppUploadProtocol
  = lens _ppUploadProtocol
      (\ s a -> s{_ppUploadProtocol = a})

-- | Pretty-print response.
ppPp :: Lens' ProjectsSnapshotsPatch Bool
ppPp = lens _ppPp (\ s a -> s{_ppPp = a})

-- | OAuth access token.
ppAccessToken :: Lens' ProjectsSnapshotsPatch (Maybe Text)
ppAccessToken
  = lens _ppAccessToken
      (\ s a -> s{_ppAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
ppUploadType :: Lens' ProjectsSnapshotsPatch (Maybe Text)
ppUploadType
  = lens _ppUploadType (\ s a -> s{_ppUploadType = a})

-- | Multipart request metadata.
ppPayload :: Lens' ProjectsSnapshotsPatch UpdateSnapshotRequest
ppPayload
  = lens _ppPayload (\ s a -> s{_ppPayload = a})

-- | OAuth bearer token.
ppBearerToken :: Lens' ProjectsSnapshotsPatch (Maybe Text)
ppBearerToken
  = lens _ppBearerToken
      (\ s a -> s{_ppBearerToken = a})

-- | The name of the snapshot.
ppName :: Lens' ProjectsSnapshotsPatch Text
ppName = lens _ppName (\ s a -> s{_ppName = a})

-- | Selector specifying which fields to include in a partial response.
ppFields :: Lens' ProjectsSnapshotsPatch (Maybe Text)
ppFields = lens _ppFields (\ s a -> s{_ppFields = a})

-- | JSONP
ppCallback :: Lens' ProjectsSnapshotsPatch (Maybe Text)
ppCallback
  = lens _ppCallback (\ s a -> s{_ppCallback = a})

instance GoogleRequest ProjectsSnapshotsPatch where
        type Rs ProjectsSnapshotsPatch = Snapshot
        type Scopes ProjectsSnapshotsPatch =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/pubsub"]
        requestClient ProjectsSnapshotsPatch'{..}
          = go _ppName _ppXgafv _ppUploadProtocol (Just _ppPp)
              _ppAccessToken
              _ppUploadType
              _ppBearerToken
              _ppCallback
              _ppFields
              (Just AltJSON)
              _ppPayload
              pubSubService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsSnapshotsPatchResource)
                      mempty
