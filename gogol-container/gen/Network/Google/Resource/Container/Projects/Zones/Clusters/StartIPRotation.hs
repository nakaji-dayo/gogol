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
-- Module      : Network.Google.Resource.Container.Projects.Zones.Clusters.StartIPRotation
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start master IP rotation.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference> for @container.projects.zones.clusters.startIpRotation@.
module Network.Google.Resource.Container.Projects.Zones.Clusters.StartIPRotation
    (
    -- * REST Resource
      ProjectsZonesClustersStartIPRotationResource

    -- * Creating a Request
    , projectsZonesClustersStartIPRotation
    , ProjectsZonesClustersStartIPRotation

    -- * Request Lenses
    , pzcsirXgafv
    , pzcsirUploadProtocol
    , pzcsirPp
    , pzcsirAccessToken
    , pzcsirUploadType
    , pzcsirZone
    , pzcsirPayload
    , pzcsirBearerToken
    , pzcsirClusterId
    , pzcsirProjectId
    , pzcsirFields
    , pzcsirCallback
    ) where

import Network.Google.Container.Types
import Network.Google.Prelude

-- | A resource alias for @container.projects.zones.clusters.startIpRotation@ method which the
-- 'ProjectsZonesClustersStartIPRotation' request conforms to.
type ProjectsZonesClustersStartIPRotationResource =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "zones" :>
             Capture "zone" Text :>
               "clusters" :>
                 CaptureMode "clusterId" "startIpRotation" Text :>
                   QueryParam "$.xgafv" Xgafv :>
                     QueryParam "upload_protocol" Text :>
                       QueryParam "pp" Bool :>
                         QueryParam "access_token" Text :>
                           QueryParam "uploadType" Text :>
                             QueryParam "bearer_token" Text :>
                               QueryParam "callback" Text :>
                                 QueryParam "fields" Text :>
                                   QueryParam "alt" AltJSON :>
                                     ReqBody '[JSON] StartIPRotationRequest :>
                                       Post '[JSON] Operation

-- | Start master IP rotation.
--
-- /See:/ 'projectsZonesClustersStartIPRotation' smart constructor.
data ProjectsZonesClustersStartIPRotation = ProjectsZonesClustersStartIPRotation'
    { _pzcsirXgafv :: !(Maybe Xgafv)
    , _pzcsirUploadProtocol :: !(Maybe Text)
    , _pzcsirPp :: !Bool
    , _pzcsirAccessToken :: !(Maybe Text)
    , _pzcsirUploadType :: !(Maybe Text)
    , _pzcsirZone :: !Text
    , _pzcsirPayload :: !StartIPRotationRequest
    , _pzcsirBearerToken :: !(Maybe Text)
    , _pzcsirClusterId :: !Text
    , _pzcsirProjectId :: !Text
    , _pzcsirFields :: !(Maybe Text)
    , _pzcsirCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsZonesClustersStartIPRotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pzcsirXgafv'
--
-- * 'pzcsirUploadProtocol'
--
-- * 'pzcsirPp'
--
-- * 'pzcsirAccessToken'
--
-- * 'pzcsirUploadType'
--
-- * 'pzcsirZone'
--
-- * 'pzcsirPayload'
--
-- * 'pzcsirBearerToken'
--
-- * 'pzcsirClusterId'
--
-- * 'pzcsirProjectId'
--
-- * 'pzcsirFields'
--
-- * 'pzcsirCallback'
projectsZonesClustersStartIPRotation
    :: Text -- ^ 'pzcsirZone'
    -> StartIPRotationRequest -- ^ 'pzcsirPayload'
    -> Text -- ^ 'pzcsirClusterId'
    -> Text -- ^ 'pzcsirProjectId'
    -> ProjectsZonesClustersStartIPRotation
projectsZonesClustersStartIPRotation pPzcsirZone_ pPzcsirPayload_ pPzcsirClusterId_ pPzcsirProjectId_ = 
    ProjectsZonesClustersStartIPRotation'
    { _pzcsirXgafv = Nothing
    , _pzcsirUploadProtocol = Nothing
    , _pzcsirPp = True
    , _pzcsirAccessToken = Nothing
    , _pzcsirUploadType = Nothing
    , _pzcsirZone = pPzcsirZone_
    , _pzcsirPayload = pPzcsirPayload_
    , _pzcsirBearerToken = Nothing
    , _pzcsirClusterId = pPzcsirClusterId_
    , _pzcsirProjectId = pPzcsirProjectId_
    , _pzcsirFields = Nothing
    , _pzcsirCallback = Nothing
    }

-- | V1 error format.
pzcsirXgafv :: Lens' ProjectsZonesClustersStartIPRotation (Maybe Xgafv)
pzcsirXgafv
  = lens _pzcsirXgafv (\ s a -> s{_pzcsirXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pzcsirUploadProtocol :: Lens' ProjectsZonesClustersStartIPRotation (Maybe Text)
pzcsirUploadProtocol
  = lens _pzcsirUploadProtocol
      (\ s a -> s{_pzcsirUploadProtocol = a})

-- | Pretty-print response.
pzcsirPp :: Lens' ProjectsZonesClustersStartIPRotation Bool
pzcsirPp = lens _pzcsirPp (\ s a -> s{_pzcsirPp = a})

-- | OAuth access token.
pzcsirAccessToken :: Lens' ProjectsZonesClustersStartIPRotation (Maybe Text)
pzcsirAccessToken
  = lens _pzcsirAccessToken
      (\ s a -> s{_pzcsirAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pzcsirUploadType :: Lens' ProjectsZonesClustersStartIPRotation (Maybe Text)
pzcsirUploadType
  = lens _pzcsirUploadType
      (\ s a -> s{_pzcsirUploadType = a})

-- | The name of the Google Compute Engine
-- [zone](\/compute\/docs\/zones#available) in which the cluster resides.
pzcsirZone :: Lens' ProjectsZonesClustersStartIPRotation Text
pzcsirZone
  = lens _pzcsirZone (\ s a -> s{_pzcsirZone = a})

-- | Multipart request metadata.
pzcsirPayload :: Lens' ProjectsZonesClustersStartIPRotation StartIPRotationRequest
pzcsirPayload
  = lens _pzcsirPayload
      (\ s a -> s{_pzcsirPayload = a})

-- | OAuth bearer token.
pzcsirBearerToken :: Lens' ProjectsZonesClustersStartIPRotation (Maybe Text)
pzcsirBearerToken
  = lens _pzcsirBearerToken
      (\ s a -> s{_pzcsirBearerToken = a})

-- | The name of the cluster.
pzcsirClusterId :: Lens' ProjectsZonesClustersStartIPRotation Text
pzcsirClusterId
  = lens _pzcsirClusterId
      (\ s a -> s{_pzcsirClusterId = a})

-- | The Google Developers Console [project ID or project
-- number](https:\/\/developers.google.com\/console\/help\/new\/#projectnumber).
pzcsirProjectId :: Lens' ProjectsZonesClustersStartIPRotation Text
pzcsirProjectId
  = lens _pzcsirProjectId
      (\ s a -> s{_pzcsirProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pzcsirFields :: Lens' ProjectsZonesClustersStartIPRotation (Maybe Text)
pzcsirFields
  = lens _pzcsirFields (\ s a -> s{_pzcsirFields = a})

-- | JSONP
pzcsirCallback :: Lens' ProjectsZonesClustersStartIPRotation (Maybe Text)
pzcsirCallback
  = lens _pzcsirCallback
      (\ s a -> s{_pzcsirCallback = a})

instance GoogleRequest
         ProjectsZonesClustersStartIPRotation where
        type Rs ProjectsZonesClustersStartIPRotation =
             Operation
        type Scopes ProjectsZonesClustersStartIPRotation =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient
          ProjectsZonesClustersStartIPRotation'{..}
          = go _pzcsirProjectId _pzcsirZone _pzcsirClusterId
              _pzcsirXgafv
              _pzcsirUploadProtocol
              (Just _pzcsirPp)
              _pzcsirAccessToken
              _pzcsirUploadType
              _pzcsirBearerToken
              _pzcsirCallback
              _pzcsirFields
              (Just AltJSON)
              _pzcsirPayload
              containerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsZonesClustersStartIPRotationResource)
                      mempty
