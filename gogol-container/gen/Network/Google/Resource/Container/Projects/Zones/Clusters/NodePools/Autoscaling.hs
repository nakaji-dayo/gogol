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
-- Module      : Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Autoscaling
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the autoscaling settings of a specific node pool.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference> for @container.projects.zones.clusters.nodePools.autoscaling@.
module Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Autoscaling
    (
    -- * REST Resource
      ProjectsZonesClustersNodePoolsAutoscalingResource

    -- * Creating a Request
    , projectsZonesClustersNodePoolsAutoscaling
    , ProjectsZonesClustersNodePoolsAutoscaling

    -- * Request Lenses
    , pzcnpaXgafv
    , pzcnpaUploadProtocol
    , pzcnpaPp
    , pzcnpaAccessToken
    , pzcnpaUploadType
    , pzcnpaZone
    , pzcnpaPayload
    , pzcnpaNodePoolId
    , pzcnpaBearerToken
    , pzcnpaClusterId
    , pzcnpaProjectId
    , pzcnpaFields
    , pzcnpaCallback
    ) where

import Network.Google.Container.Types
import Network.Google.Prelude

-- | A resource alias for @container.projects.zones.clusters.nodePools.autoscaling@ method which the
-- 'ProjectsZonesClustersNodePoolsAutoscaling' request conforms to.
type ProjectsZonesClustersNodePoolsAutoscalingResource
     =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "zones" :>
             Capture "zone" Text :>
               "clusters" :>
                 Capture "clusterId" Text :>
                   "nodePools" :>
                     Capture "nodePoolId" Text :>
                       "autoscaling" :>
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
                                             SetNodePoolAutoscalingRequest
                                             :> Post '[JSON] Operation

-- | Sets the autoscaling settings of a specific node pool.
--
-- /See:/ 'projectsZonesClustersNodePoolsAutoscaling' smart constructor.
data ProjectsZonesClustersNodePoolsAutoscaling = ProjectsZonesClustersNodePoolsAutoscaling'
    { _pzcnpaXgafv :: !(Maybe Xgafv)
    , _pzcnpaUploadProtocol :: !(Maybe Text)
    , _pzcnpaPp :: !Bool
    , _pzcnpaAccessToken :: !(Maybe Text)
    , _pzcnpaUploadType :: !(Maybe Text)
    , _pzcnpaZone :: !Text
    , _pzcnpaPayload :: !SetNodePoolAutoscalingRequest
    , _pzcnpaNodePoolId :: !Text
    , _pzcnpaBearerToken :: !(Maybe Text)
    , _pzcnpaClusterId :: !Text
    , _pzcnpaProjectId :: !Text
    , _pzcnpaFields :: !(Maybe Text)
    , _pzcnpaCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsZonesClustersNodePoolsAutoscaling' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pzcnpaXgafv'
--
-- * 'pzcnpaUploadProtocol'
--
-- * 'pzcnpaPp'
--
-- * 'pzcnpaAccessToken'
--
-- * 'pzcnpaUploadType'
--
-- * 'pzcnpaZone'
--
-- * 'pzcnpaPayload'
--
-- * 'pzcnpaNodePoolId'
--
-- * 'pzcnpaBearerToken'
--
-- * 'pzcnpaClusterId'
--
-- * 'pzcnpaProjectId'
--
-- * 'pzcnpaFields'
--
-- * 'pzcnpaCallback'
projectsZonesClustersNodePoolsAutoscaling
    :: Text -- ^ 'pzcnpaZone'
    -> SetNodePoolAutoscalingRequest -- ^ 'pzcnpaPayload'
    -> Text -- ^ 'pzcnpaNodePoolId'
    -> Text -- ^ 'pzcnpaClusterId'
    -> Text -- ^ 'pzcnpaProjectId'
    -> ProjectsZonesClustersNodePoolsAutoscaling
projectsZonesClustersNodePoolsAutoscaling pPzcnpaZone_ pPzcnpaPayload_ pPzcnpaNodePoolId_ pPzcnpaClusterId_ pPzcnpaProjectId_ = 
    ProjectsZonesClustersNodePoolsAutoscaling'
    { _pzcnpaXgafv = Nothing
    , _pzcnpaUploadProtocol = Nothing
    , _pzcnpaPp = True
    , _pzcnpaAccessToken = Nothing
    , _pzcnpaUploadType = Nothing
    , _pzcnpaZone = pPzcnpaZone_
    , _pzcnpaPayload = pPzcnpaPayload_
    , _pzcnpaNodePoolId = pPzcnpaNodePoolId_
    , _pzcnpaBearerToken = Nothing
    , _pzcnpaClusterId = pPzcnpaClusterId_
    , _pzcnpaProjectId = pPzcnpaProjectId_
    , _pzcnpaFields = Nothing
    , _pzcnpaCallback = Nothing
    }

-- | V1 error format.
pzcnpaXgafv :: Lens' ProjectsZonesClustersNodePoolsAutoscaling (Maybe Xgafv)
pzcnpaXgafv
  = lens _pzcnpaXgafv (\ s a -> s{_pzcnpaXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pzcnpaUploadProtocol :: Lens' ProjectsZonesClustersNodePoolsAutoscaling (Maybe Text)
pzcnpaUploadProtocol
  = lens _pzcnpaUploadProtocol
      (\ s a -> s{_pzcnpaUploadProtocol = a})

-- | Pretty-print response.
pzcnpaPp :: Lens' ProjectsZonesClustersNodePoolsAutoscaling Bool
pzcnpaPp = lens _pzcnpaPp (\ s a -> s{_pzcnpaPp = a})

-- | OAuth access token.
pzcnpaAccessToken :: Lens' ProjectsZonesClustersNodePoolsAutoscaling (Maybe Text)
pzcnpaAccessToken
  = lens _pzcnpaAccessToken
      (\ s a -> s{_pzcnpaAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pzcnpaUploadType :: Lens' ProjectsZonesClustersNodePoolsAutoscaling (Maybe Text)
pzcnpaUploadType
  = lens _pzcnpaUploadType
      (\ s a -> s{_pzcnpaUploadType = a})

-- | The name of the Google Compute Engine
-- [zone](\/compute\/docs\/zones#available) in which the cluster resides.
pzcnpaZone :: Lens' ProjectsZonesClustersNodePoolsAutoscaling Text
pzcnpaZone
  = lens _pzcnpaZone (\ s a -> s{_pzcnpaZone = a})

-- | Multipart request metadata.
pzcnpaPayload :: Lens' ProjectsZonesClustersNodePoolsAutoscaling SetNodePoolAutoscalingRequest
pzcnpaPayload
  = lens _pzcnpaPayload
      (\ s a -> s{_pzcnpaPayload = a})

-- | The name of the node pool to upgrade.
pzcnpaNodePoolId :: Lens' ProjectsZonesClustersNodePoolsAutoscaling Text
pzcnpaNodePoolId
  = lens _pzcnpaNodePoolId
      (\ s a -> s{_pzcnpaNodePoolId = a})

-- | OAuth bearer token.
pzcnpaBearerToken :: Lens' ProjectsZonesClustersNodePoolsAutoscaling (Maybe Text)
pzcnpaBearerToken
  = lens _pzcnpaBearerToken
      (\ s a -> s{_pzcnpaBearerToken = a})

-- | The name of the cluster to upgrade.
pzcnpaClusterId :: Lens' ProjectsZonesClustersNodePoolsAutoscaling Text
pzcnpaClusterId
  = lens _pzcnpaClusterId
      (\ s a -> s{_pzcnpaClusterId = a})

-- | The Google Developers Console [project ID or project
-- number](https:\/\/support.google.com\/cloud\/answer\/6158840).
pzcnpaProjectId :: Lens' ProjectsZonesClustersNodePoolsAutoscaling Text
pzcnpaProjectId
  = lens _pzcnpaProjectId
      (\ s a -> s{_pzcnpaProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pzcnpaFields :: Lens' ProjectsZonesClustersNodePoolsAutoscaling (Maybe Text)
pzcnpaFields
  = lens _pzcnpaFields (\ s a -> s{_pzcnpaFields = a})

-- | JSONP
pzcnpaCallback :: Lens' ProjectsZonesClustersNodePoolsAutoscaling (Maybe Text)
pzcnpaCallback
  = lens _pzcnpaCallback
      (\ s a -> s{_pzcnpaCallback = a})

instance GoogleRequest
         ProjectsZonesClustersNodePoolsAutoscaling where
        type Rs ProjectsZonesClustersNodePoolsAutoscaling =
             Operation
        type Scopes ProjectsZonesClustersNodePoolsAutoscaling
             = '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient
          ProjectsZonesClustersNodePoolsAutoscaling'{..}
          = go _pzcnpaProjectId _pzcnpaZone _pzcnpaClusterId
              _pzcnpaNodePoolId
              _pzcnpaXgafv
              _pzcnpaUploadProtocol
              (Just _pzcnpaPp)
              _pzcnpaAccessToken
              _pzcnpaUploadType
              _pzcnpaBearerToken
              _pzcnpaCallback
              _pzcnpaFields
              (Just AltJSON)
              _pzcnpaPayload
              containerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           ProjectsZonesClustersNodePoolsAutoscalingResource)
                      mempty
