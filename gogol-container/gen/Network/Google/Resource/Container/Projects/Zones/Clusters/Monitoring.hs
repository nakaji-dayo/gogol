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
-- Module      : Network.Google.Resource.Container.Projects.Zones.Clusters.Monitoring
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the monitoring service of a specific cluster.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference> for @container.projects.zones.clusters.monitoring@.
module Network.Google.Resource.Container.Projects.Zones.Clusters.Monitoring
    (
    -- * REST Resource
      ProjectsZonesClustersMonitoringResource

    -- * Creating a Request
    , projectsZonesClustersMonitoring
    , ProjectsZonesClustersMonitoring

    -- * Request Lenses
    , pzcmzXgafv
    , pzcmzUploadProtocol
    , pzcmzPp
    , pzcmzAccessToken
    , pzcmzUploadType
    , pzcmzZone
    , pzcmzPayload
    , pzcmzBearerToken
    , pzcmzClusterId
    , pzcmzProjectId
    , pzcmzFields
    , pzcmzCallback
    ) where

import Network.Google.Container.Types
import Network.Google.Prelude

-- | A resource alias for @container.projects.zones.clusters.monitoring@ method which the
-- 'ProjectsZonesClustersMonitoring' request conforms to.
type ProjectsZonesClustersMonitoringResource =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "zones" :>
             Capture "zone" Text :>
               "clusters" :>
                 Capture "clusterId" Text :>
                   "monitoring" :>
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
                                         SetMonitoringServiceRequest
                                         :> Post '[JSON] Operation

-- | Sets the monitoring service of a specific cluster.
--
-- /See:/ 'projectsZonesClustersMonitoring' smart constructor.
data ProjectsZonesClustersMonitoring = ProjectsZonesClustersMonitoring'
    { _pzcmzXgafv :: !(Maybe Xgafv)
    , _pzcmzUploadProtocol :: !(Maybe Text)
    , _pzcmzPp :: !Bool
    , _pzcmzAccessToken :: !(Maybe Text)
    , _pzcmzUploadType :: !(Maybe Text)
    , _pzcmzZone :: !Text
    , _pzcmzPayload :: !SetMonitoringServiceRequest
    , _pzcmzBearerToken :: !(Maybe Text)
    , _pzcmzClusterId :: !Text
    , _pzcmzProjectId :: !Text
    , _pzcmzFields :: !(Maybe Text)
    , _pzcmzCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsZonesClustersMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pzcmzXgafv'
--
-- * 'pzcmzUploadProtocol'
--
-- * 'pzcmzPp'
--
-- * 'pzcmzAccessToken'
--
-- * 'pzcmzUploadType'
--
-- * 'pzcmzZone'
--
-- * 'pzcmzPayload'
--
-- * 'pzcmzBearerToken'
--
-- * 'pzcmzClusterId'
--
-- * 'pzcmzProjectId'
--
-- * 'pzcmzFields'
--
-- * 'pzcmzCallback'
projectsZonesClustersMonitoring
    :: Text -- ^ 'pzcmzZone'
    -> SetMonitoringServiceRequest -- ^ 'pzcmzPayload'
    -> Text -- ^ 'pzcmzClusterId'
    -> Text -- ^ 'pzcmzProjectId'
    -> ProjectsZonesClustersMonitoring
projectsZonesClustersMonitoring pPzcmzZone_ pPzcmzPayload_ pPzcmzClusterId_ pPzcmzProjectId_ = 
    ProjectsZonesClustersMonitoring'
    { _pzcmzXgafv = Nothing
    , _pzcmzUploadProtocol = Nothing
    , _pzcmzPp = True
    , _pzcmzAccessToken = Nothing
    , _pzcmzUploadType = Nothing
    , _pzcmzZone = pPzcmzZone_
    , _pzcmzPayload = pPzcmzPayload_
    , _pzcmzBearerToken = Nothing
    , _pzcmzClusterId = pPzcmzClusterId_
    , _pzcmzProjectId = pPzcmzProjectId_
    , _pzcmzFields = Nothing
    , _pzcmzCallback = Nothing
    }

-- | V1 error format.
pzcmzXgafv :: Lens' ProjectsZonesClustersMonitoring (Maybe Xgafv)
pzcmzXgafv
  = lens _pzcmzXgafv (\ s a -> s{_pzcmzXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pzcmzUploadProtocol :: Lens' ProjectsZonesClustersMonitoring (Maybe Text)
pzcmzUploadProtocol
  = lens _pzcmzUploadProtocol
      (\ s a -> s{_pzcmzUploadProtocol = a})

-- | Pretty-print response.
pzcmzPp :: Lens' ProjectsZonesClustersMonitoring Bool
pzcmzPp = lens _pzcmzPp (\ s a -> s{_pzcmzPp = a})

-- | OAuth access token.
pzcmzAccessToken :: Lens' ProjectsZonesClustersMonitoring (Maybe Text)
pzcmzAccessToken
  = lens _pzcmzAccessToken
      (\ s a -> s{_pzcmzAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pzcmzUploadType :: Lens' ProjectsZonesClustersMonitoring (Maybe Text)
pzcmzUploadType
  = lens _pzcmzUploadType
      (\ s a -> s{_pzcmzUploadType = a})

-- | The name of the Google Compute Engine
-- [zone](\/compute\/docs\/zones#available) in which the cluster resides.
pzcmzZone :: Lens' ProjectsZonesClustersMonitoring Text
pzcmzZone
  = lens _pzcmzZone (\ s a -> s{_pzcmzZone = a})

-- | Multipart request metadata.
pzcmzPayload :: Lens' ProjectsZonesClustersMonitoring SetMonitoringServiceRequest
pzcmzPayload
  = lens _pzcmzPayload (\ s a -> s{_pzcmzPayload = a})

-- | OAuth bearer token.
pzcmzBearerToken :: Lens' ProjectsZonesClustersMonitoring (Maybe Text)
pzcmzBearerToken
  = lens _pzcmzBearerToken
      (\ s a -> s{_pzcmzBearerToken = a})

-- | The name of the cluster to upgrade.
pzcmzClusterId :: Lens' ProjectsZonesClustersMonitoring Text
pzcmzClusterId
  = lens _pzcmzClusterId
      (\ s a -> s{_pzcmzClusterId = a})

-- | The Google Developers Console [project ID or project
-- number](https:\/\/support.google.com\/cloud\/answer\/6158840).
pzcmzProjectId :: Lens' ProjectsZonesClustersMonitoring Text
pzcmzProjectId
  = lens _pzcmzProjectId
      (\ s a -> s{_pzcmzProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pzcmzFields :: Lens' ProjectsZonesClustersMonitoring (Maybe Text)
pzcmzFields
  = lens _pzcmzFields (\ s a -> s{_pzcmzFields = a})

-- | JSONP
pzcmzCallback :: Lens' ProjectsZonesClustersMonitoring (Maybe Text)
pzcmzCallback
  = lens _pzcmzCallback
      (\ s a -> s{_pzcmzCallback = a})

instance GoogleRequest
         ProjectsZonesClustersMonitoring where
        type Rs ProjectsZonesClustersMonitoring = Operation
        type Scopes ProjectsZonesClustersMonitoring =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsZonesClustersMonitoring'{..}
          = go _pzcmzProjectId _pzcmzZone _pzcmzClusterId
              _pzcmzXgafv
              _pzcmzUploadProtocol
              (Just _pzcmzPp)
              _pzcmzAccessToken
              _pzcmzUploadType
              _pzcmzBearerToken
              _pzcmzCallback
              _pzcmzFields
              (Just AltJSON)
              _pzcmzPayload
              containerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsZonesClustersMonitoringResource)
                      mempty
