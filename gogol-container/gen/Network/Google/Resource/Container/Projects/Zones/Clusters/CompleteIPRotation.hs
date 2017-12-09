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
-- Module      : Network.Google.Resource.Container.Projects.Zones.Clusters.CompleteIPRotation
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Completes master IP rotation.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference> for @container.projects.zones.clusters.completeIpRotation@.
module Network.Google.Resource.Container.Projects.Zones.Clusters.CompleteIPRotation
    (
    -- * REST Resource
      ProjectsZonesClustersCompleteIPRotationResource

    -- * Creating a Request
    , projectsZonesClustersCompleteIPRotation
    , ProjectsZonesClustersCompleteIPRotation

    -- * Request Lenses
    , pzccirXgafv
    , pzccirUploadProtocol
    , pzccirPp
    , pzccirAccessToken
    , pzccirUploadType
    , pzccirZone
    , pzccirPayload
    , pzccirBearerToken
    , pzccirClusterId
    , pzccirProjectId
    , pzccirFields
    , pzccirCallback
    ) where

import Network.Google.Container.Types
import Network.Google.Prelude

-- | A resource alias for @container.projects.zones.clusters.completeIpRotation@ method which the
-- 'ProjectsZonesClustersCompleteIPRotation' request conforms to.
type ProjectsZonesClustersCompleteIPRotationResource
     =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "zones" :>
             Capture "zone" Text :>
               "clusters" :>
                 CaptureMode "clusterId" "completeIpRotation" Text :>
                   QueryParam "$.xgafv" Xgafv :>
                     QueryParam "upload_protocol" Text :>
                       QueryParam "pp" Bool :>
                         QueryParam "access_token" Text :>
                           QueryParam "uploadType" Text :>
                             QueryParam "bearer_token" Text :>
                               QueryParam "callback" Text :>
                                 QueryParam "fields" Text :>
                                   QueryParam "alt" AltJSON :>
                                     ReqBody '[JSON] CompleteIPRotationRequest
                                       :> Post '[JSON] Operation

-- | Completes master IP rotation.
--
-- /See:/ 'projectsZonesClustersCompleteIPRotation' smart constructor.
data ProjectsZonesClustersCompleteIPRotation = ProjectsZonesClustersCompleteIPRotation'
    { _pzccirXgafv :: !(Maybe Xgafv)
    , _pzccirUploadProtocol :: !(Maybe Text)
    , _pzccirPp :: !Bool
    , _pzccirAccessToken :: !(Maybe Text)
    , _pzccirUploadType :: !(Maybe Text)
    , _pzccirZone :: !Text
    , _pzccirPayload :: !CompleteIPRotationRequest
    , _pzccirBearerToken :: !(Maybe Text)
    , _pzccirClusterId :: !Text
    , _pzccirProjectId :: !Text
    , _pzccirFields :: !(Maybe Text)
    , _pzccirCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsZonesClustersCompleteIPRotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pzccirXgafv'
--
-- * 'pzccirUploadProtocol'
--
-- * 'pzccirPp'
--
-- * 'pzccirAccessToken'
--
-- * 'pzccirUploadType'
--
-- * 'pzccirZone'
--
-- * 'pzccirPayload'
--
-- * 'pzccirBearerToken'
--
-- * 'pzccirClusterId'
--
-- * 'pzccirProjectId'
--
-- * 'pzccirFields'
--
-- * 'pzccirCallback'
projectsZonesClustersCompleteIPRotation
    :: Text -- ^ 'pzccirZone'
    -> CompleteIPRotationRequest -- ^ 'pzccirPayload'
    -> Text -- ^ 'pzccirClusterId'
    -> Text -- ^ 'pzccirProjectId'
    -> ProjectsZonesClustersCompleteIPRotation
projectsZonesClustersCompleteIPRotation pPzccirZone_ pPzccirPayload_ pPzccirClusterId_ pPzccirProjectId_ = 
    ProjectsZonesClustersCompleteIPRotation'
    { _pzccirXgafv = Nothing
    , _pzccirUploadProtocol = Nothing
    , _pzccirPp = True
    , _pzccirAccessToken = Nothing
    , _pzccirUploadType = Nothing
    , _pzccirZone = pPzccirZone_
    , _pzccirPayload = pPzccirPayload_
    , _pzccirBearerToken = Nothing
    , _pzccirClusterId = pPzccirClusterId_
    , _pzccirProjectId = pPzccirProjectId_
    , _pzccirFields = Nothing
    , _pzccirCallback = Nothing
    }

-- | V1 error format.
pzccirXgafv :: Lens' ProjectsZonesClustersCompleteIPRotation (Maybe Xgafv)
pzccirXgafv
  = lens _pzccirXgafv (\ s a -> s{_pzccirXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pzccirUploadProtocol :: Lens' ProjectsZonesClustersCompleteIPRotation (Maybe Text)
pzccirUploadProtocol
  = lens _pzccirUploadProtocol
      (\ s a -> s{_pzccirUploadProtocol = a})

-- | Pretty-print response.
pzccirPp :: Lens' ProjectsZonesClustersCompleteIPRotation Bool
pzccirPp = lens _pzccirPp (\ s a -> s{_pzccirPp = a})

-- | OAuth access token.
pzccirAccessToken :: Lens' ProjectsZonesClustersCompleteIPRotation (Maybe Text)
pzccirAccessToken
  = lens _pzccirAccessToken
      (\ s a -> s{_pzccirAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pzccirUploadType :: Lens' ProjectsZonesClustersCompleteIPRotation (Maybe Text)
pzccirUploadType
  = lens _pzccirUploadType
      (\ s a -> s{_pzccirUploadType = a})

-- | The name of the Google Compute Engine
-- [zone](\/compute\/docs\/zones#available) in which the cluster resides.
pzccirZone :: Lens' ProjectsZonesClustersCompleteIPRotation Text
pzccirZone
  = lens _pzccirZone (\ s a -> s{_pzccirZone = a})

-- | Multipart request metadata.
pzccirPayload :: Lens' ProjectsZonesClustersCompleteIPRotation CompleteIPRotationRequest
pzccirPayload
  = lens _pzccirPayload
      (\ s a -> s{_pzccirPayload = a})

-- | OAuth bearer token.
pzccirBearerToken :: Lens' ProjectsZonesClustersCompleteIPRotation (Maybe Text)
pzccirBearerToken
  = lens _pzccirBearerToken
      (\ s a -> s{_pzccirBearerToken = a})

-- | The name of the cluster.
pzccirClusterId :: Lens' ProjectsZonesClustersCompleteIPRotation Text
pzccirClusterId
  = lens _pzccirClusterId
      (\ s a -> s{_pzccirClusterId = a})

-- | The Google Developers Console [project ID or project
-- number](https:\/\/developers.google.com\/console\/help\/new\/#projectnumber).
pzccirProjectId :: Lens' ProjectsZonesClustersCompleteIPRotation Text
pzccirProjectId
  = lens _pzccirProjectId
      (\ s a -> s{_pzccirProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pzccirFields :: Lens' ProjectsZonesClustersCompleteIPRotation (Maybe Text)
pzccirFields
  = lens _pzccirFields (\ s a -> s{_pzccirFields = a})

-- | JSONP
pzccirCallback :: Lens' ProjectsZonesClustersCompleteIPRotation (Maybe Text)
pzccirCallback
  = lens _pzccirCallback
      (\ s a -> s{_pzccirCallback = a})

instance GoogleRequest
         ProjectsZonesClustersCompleteIPRotation where
        type Rs ProjectsZonesClustersCompleteIPRotation =
             Operation
        type Scopes ProjectsZonesClustersCompleteIPRotation =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient
          ProjectsZonesClustersCompleteIPRotation'{..}
          = go _pzccirProjectId _pzccirZone _pzccirClusterId
              _pzccirXgafv
              _pzccirUploadProtocol
              (Just _pzccirPp)
              _pzccirAccessToken
              _pzccirUploadType
              _pzccirBearerToken
              _pzccirCallback
              _pzccirFields
              (Just AltJSON)
              _pzccirPayload
              containerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           ProjectsZonesClustersCompleteIPRotationResource)
                      mempty
