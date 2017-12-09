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
-- Module      : Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the version and\/or image type of a specific node pool.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference> for @container.projects.zones.clusters.nodePools.update@.
module Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Update
    (
    -- * REST Resource
      ProjectsZonesClustersNodePoolsUpdateResource

    -- * Creating a Request
    , projectsZonesClustersNodePoolsUpdate
    , ProjectsZonesClustersNodePoolsUpdate

    -- * Request Lenses
    , pzcnpuXgafv
    , pzcnpuUploadProtocol
    , pzcnpuPp
    , pzcnpuAccessToken
    , pzcnpuUploadType
    , pzcnpuZone
    , pzcnpuPayload
    , pzcnpuNodePoolId
    , pzcnpuBearerToken
    , pzcnpuClusterId
    , pzcnpuProjectId
    , pzcnpuFields
    , pzcnpuCallback
    ) where

import Network.Google.Container.Types
import Network.Google.Prelude

-- | A resource alias for @container.projects.zones.clusters.nodePools.update@ method which the
-- 'ProjectsZonesClustersNodePoolsUpdate' request conforms to.
type ProjectsZonesClustersNodePoolsUpdateResource =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "zones" :>
             Capture "zone" Text :>
               "clusters" :>
                 Capture "clusterId" Text :>
                   "nodePools" :>
                     Capture "nodePoolId" Text :>
                       "update" :>
                         QueryParam "$.xgafv" Xgafv :>
                           QueryParam "upload_protocol" Text :>
                             QueryParam "pp" Bool :>
                               QueryParam "access_token" Text :>
                                 QueryParam "uploadType" Text :>
                                   QueryParam "bearer_token" Text :>
                                     QueryParam "callback" Text :>
                                       QueryParam "fields" Text :>
                                         QueryParam "alt" AltJSON :>
                                           ReqBody '[JSON] UpdateNodePoolRequest
                                             :> Post '[JSON] Operation

-- | Updates the version and\/or image type of a specific node pool.
--
-- /See:/ 'projectsZonesClustersNodePoolsUpdate' smart constructor.
data ProjectsZonesClustersNodePoolsUpdate = ProjectsZonesClustersNodePoolsUpdate'
    { _pzcnpuXgafv :: !(Maybe Xgafv)
    , _pzcnpuUploadProtocol :: !(Maybe Text)
    , _pzcnpuPp :: !Bool
    , _pzcnpuAccessToken :: !(Maybe Text)
    , _pzcnpuUploadType :: !(Maybe Text)
    , _pzcnpuZone :: !Text
    , _pzcnpuPayload :: !UpdateNodePoolRequest
    , _pzcnpuNodePoolId :: !Text
    , _pzcnpuBearerToken :: !(Maybe Text)
    , _pzcnpuClusterId :: !Text
    , _pzcnpuProjectId :: !Text
    , _pzcnpuFields :: !(Maybe Text)
    , _pzcnpuCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsZonesClustersNodePoolsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pzcnpuXgafv'
--
-- * 'pzcnpuUploadProtocol'
--
-- * 'pzcnpuPp'
--
-- * 'pzcnpuAccessToken'
--
-- * 'pzcnpuUploadType'
--
-- * 'pzcnpuZone'
--
-- * 'pzcnpuPayload'
--
-- * 'pzcnpuNodePoolId'
--
-- * 'pzcnpuBearerToken'
--
-- * 'pzcnpuClusterId'
--
-- * 'pzcnpuProjectId'
--
-- * 'pzcnpuFields'
--
-- * 'pzcnpuCallback'
projectsZonesClustersNodePoolsUpdate
    :: Text -- ^ 'pzcnpuZone'
    -> UpdateNodePoolRequest -- ^ 'pzcnpuPayload'
    -> Text -- ^ 'pzcnpuNodePoolId'
    -> Text -- ^ 'pzcnpuClusterId'
    -> Text -- ^ 'pzcnpuProjectId'
    -> ProjectsZonesClustersNodePoolsUpdate
projectsZonesClustersNodePoolsUpdate pPzcnpuZone_ pPzcnpuPayload_ pPzcnpuNodePoolId_ pPzcnpuClusterId_ pPzcnpuProjectId_ = 
    ProjectsZonesClustersNodePoolsUpdate'
    { _pzcnpuXgafv = Nothing
    , _pzcnpuUploadProtocol = Nothing
    , _pzcnpuPp = True
    , _pzcnpuAccessToken = Nothing
    , _pzcnpuUploadType = Nothing
    , _pzcnpuZone = pPzcnpuZone_
    , _pzcnpuPayload = pPzcnpuPayload_
    , _pzcnpuNodePoolId = pPzcnpuNodePoolId_
    , _pzcnpuBearerToken = Nothing
    , _pzcnpuClusterId = pPzcnpuClusterId_
    , _pzcnpuProjectId = pPzcnpuProjectId_
    , _pzcnpuFields = Nothing
    , _pzcnpuCallback = Nothing
    }

-- | V1 error format.
pzcnpuXgafv :: Lens' ProjectsZonesClustersNodePoolsUpdate (Maybe Xgafv)
pzcnpuXgafv
  = lens _pzcnpuXgafv (\ s a -> s{_pzcnpuXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pzcnpuUploadProtocol :: Lens' ProjectsZonesClustersNodePoolsUpdate (Maybe Text)
pzcnpuUploadProtocol
  = lens _pzcnpuUploadProtocol
      (\ s a -> s{_pzcnpuUploadProtocol = a})

-- | Pretty-print response.
pzcnpuPp :: Lens' ProjectsZonesClustersNodePoolsUpdate Bool
pzcnpuPp = lens _pzcnpuPp (\ s a -> s{_pzcnpuPp = a})

-- | OAuth access token.
pzcnpuAccessToken :: Lens' ProjectsZonesClustersNodePoolsUpdate (Maybe Text)
pzcnpuAccessToken
  = lens _pzcnpuAccessToken
      (\ s a -> s{_pzcnpuAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pzcnpuUploadType :: Lens' ProjectsZonesClustersNodePoolsUpdate (Maybe Text)
pzcnpuUploadType
  = lens _pzcnpuUploadType
      (\ s a -> s{_pzcnpuUploadType = a})

-- | The name of the Google Compute Engine
-- [zone](\/compute\/docs\/zones#available) in which the cluster resides.
pzcnpuZone :: Lens' ProjectsZonesClustersNodePoolsUpdate Text
pzcnpuZone
  = lens _pzcnpuZone (\ s a -> s{_pzcnpuZone = a})

-- | Multipart request metadata.
pzcnpuPayload :: Lens' ProjectsZonesClustersNodePoolsUpdate UpdateNodePoolRequest
pzcnpuPayload
  = lens _pzcnpuPayload
      (\ s a -> s{_pzcnpuPayload = a})

-- | The name of the node pool to upgrade.
pzcnpuNodePoolId :: Lens' ProjectsZonesClustersNodePoolsUpdate Text
pzcnpuNodePoolId
  = lens _pzcnpuNodePoolId
      (\ s a -> s{_pzcnpuNodePoolId = a})

-- | OAuth bearer token.
pzcnpuBearerToken :: Lens' ProjectsZonesClustersNodePoolsUpdate (Maybe Text)
pzcnpuBearerToken
  = lens _pzcnpuBearerToken
      (\ s a -> s{_pzcnpuBearerToken = a})

-- | The name of the cluster to upgrade.
pzcnpuClusterId :: Lens' ProjectsZonesClustersNodePoolsUpdate Text
pzcnpuClusterId
  = lens _pzcnpuClusterId
      (\ s a -> s{_pzcnpuClusterId = a})

-- | The Google Developers Console [project ID or project
-- number](https:\/\/support.google.com\/cloud\/answer\/6158840).
pzcnpuProjectId :: Lens' ProjectsZonesClustersNodePoolsUpdate Text
pzcnpuProjectId
  = lens _pzcnpuProjectId
      (\ s a -> s{_pzcnpuProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pzcnpuFields :: Lens' ProjectsZonesClustersNodePoolsUpdate (Maybe Text)
pzcnpuFields
  = lens _pzcnpuFields (\ s a -> s{_pzcnpuFields = a})

-- | JSONP
pzcnpuCallback :: Lens' ProjectsZonesClustersNodePoolsUpdate (Maybe Text)
pzcnpuCallback
  = lens _pzcnpuCallback
      (\ s a -> s{_pzcnpuCallback = a})

instance GoogleRequest
         ProjectsZonesClustersNodePoolsUpdate where
        type Rs ProjectsZonesClustersNodePoolsUpdate =
             Operation
        type Scopes ProjectsZonesClustersNodePoolsUpdate =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient
          ProjectsZonesClustersNodePoolsUpdate'{..}
          = go _pzcnpuProjectId _pzcnpuZone _pzcnpuClusterId
              _pzcnpuNodePoolId
              _pzcnpuXgafv
              _pzcnpuUploadProtocol
              (Just _pzcnpuPp)
              _pzcnpuAccessToken
              _pzcnpuUploadType
              _pzcnpuBearerToken
              _pzcnpuCallback
              _pzcnpuFields
              (Just AltJSON)
              _pzcnpuPayload
              containerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsZonesClustersNodePoolsUpdateResource)
                      mempty
