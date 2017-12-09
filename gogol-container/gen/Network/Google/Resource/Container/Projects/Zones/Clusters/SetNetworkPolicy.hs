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
-- Module      : Network.Google.Resource.Container.Projects.Zones.Clusters.SetNetworkPolicy
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables\/Disables Network Policy for a cluster.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference> for @container.projects.zones.clusters.setNetworkPolicy@.
module Network.Google.Resource.Container.Projects.Zones.Clusters.SetNetworkPolicy
    (
    -- * REST Resource
      ProjectsZonesClustersSetNetworkPolicyResource

    -- * Creating a Request
    , projectsZonesClustersSetNetworkPolicy
    , ProjectsZonesClustersSetNetworkPolicy

    -- * Request Lenses
    , pzcsnpXgafv
    , pzcsnpUploadProtocol
    , pzcsnpPp
    , pzcsnpAccessToken
    , pzcsnpUploadType
    , pzcsnpZone
    , pzcsnpPayload
    , pzcsnpBearerToken
    , pzcsnpClusterId
    , pzcsnpProjectId
    , pzcsnpFields
    , pzcsnpCallback
    ) where

import Network.Google.Container.Types
import Network.Google.Prelude

-- | A resource alias for @container.projects.zones.clusters.setNetworkPolicy@ method which the
-- 'ProjectsZonesClustersSetNetworkPolicy' request conforms to.
type ProjectsZonesClustersSetNetworkPolicyResource =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "zones" :>
             Capture "zone" Text :>
               "clusters" :>
                 CaptureMode "clusterId" "setNetworkPolicy" Text :>
                   QueryParam "$.xgafv" Xgafv :>
                     QueryParam "upload_protocol" Text :>
                       QueryParam "pp" Bool :>
                         QueryParam "access_token" Text :>
                           QueryParam "uploadType" Text :>
                             QueryParam "bearer_token" Text :>
                               QueryParam "callback" Text :>
                                 QueryParam "fields" Text :>
                                   QueryParam "alt" AltJSON :>
                                     ReqBody '[JSON] SetNetworkPolicyRequest :>
                                       Post '[JSON] Operation

-- | Enables\/Disables Network Policy for a cluster.
--
-- /See:/ 'projectsZonesClustersSetNetworkPolicy' smart constructor.
data ProjectsZonesClustersSetNetworkPolicy = ProjectsZonesClustersSetNetworkPolicy'
    { _pzcsnpXgafv :: !(Maybe Xgafv)
    , _pzcsnpUploadProtocol :: !(Maybe Text)
    , _pzcsnpPp :: !Bool
    , _pzcsnpAccessToken :: !(Maybe Text)
    , _pzcsnpUploadType :: !(Maybe Text)
    , _pzcsnpZone :: !Text
    , _pzcsnpPayload :: !SetNetworkPolicyRequest
    , _pzcsnpBearerToken :: !(Maybe Text)
    , _pzcsnpClusterId :: !Text
    , _pzcsnpProjectId :: !Text
    , _pzcsnpFields :: !(Maybe Text)
    , _pzcsnpCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsZonesClustersSetNetworkPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pzcsnpXgafv'
--
-- * 'pzcsnpUploadProtocol'
--
-- * 'pzcsnpPp'
--
-- * 'pzcsnpAccessToken'
--
-- * 'pzcsnpUploadType'
--
-- * 'pzcsnpZone'
--
-- * 'pzcsnpPayload'
--
-- * 'pzcsnpBearerToken'
--
-- * 'pzcsnpClusterId'
--
-- * 'pzcsnpProjectId'
--
-- * 'pzcsnpFields'
--
-- * 'pzcsnpCallback'
projectsZonesClustersSetNetworkPolicy
    :: Text -- ^ 'pzcsnpZone'
    -> SetNetworkPolicyRequest -- ^ 'pzcsnpPayload'
    -> Text -- ^ 'pzcsnpClusterId'
    -> Text -- ^ 'pzcsnpProjectId'
    -> ProjectsZonesClustersSetNetworkPolicy
projectsZonesClustersSetNetworkPolicy pPzcsnpZone_ pPzcsnpPayload_ pPzcsnpClusterId_ pPzcsnpProjectId_ = 
    ProjectsZonesClustersSetNetworkPolicy'
    { _pzcsnpXgafv = Nothing
    , _pzcsnpUploadProtocol = Nothing
    , _pzcsnpPp = True
    , _pzcsnpAccessToken = Nothing
    , _pzcsnpUploadType = Nothing
    , _pzcsnpZone = pPzcsnpZone_
    , _pzcsnpPayload = pPzcsnpPayload_
    , _pzcsnpBearerToken = Nothing
    , _pzcsnpClusterId = pPzcsnpClusterId_
    , _pzcsnpProjectId = pPzcsnpProjectId_
    , _pzcsnpFields = Nothing
    , _pzcsnpCallback = Nothing
    }

-- | V1 error format.
pzcsnpXgafv :: Lens' ProjectsZonesClustersSetNetworkPolicy (Maybe Xgafv)
pzcsnpXgafv
  = lens _pzcsnpXgafv (\ s a -> s{_pzcsnpXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pzcsnpUploadProtocol :: Lens' ProjectsZonesClustersSetNetworkPolicy (Maybe Text)
pzcsnpUploadProtocol
  = lens _pzcsnpUploadProtocol
      (\ s a -> s{_pzcsnpUploadProtocol = a})

-- | Pretty-print response.
pzcsnpPp :: Lens' ProjectsZonesClustersSetNetworkPolicy Bool
pzcsnpPp = lens _pzcsnpPp (\ s a -> s{_pzcsnpPp = a})

-- | OAuth access token.
pzcsnpAccessToken :: Lens' ProjectsZonesClustersSetNetworkPolicy (Maybe Text)
pzcsnpAccessToken
  = lens _pzcsnpAccessToken
      (\ s a -> s{_pzcsnpAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pzcsnpUploadType :: Lens' ProjectsZonesClustersSetNetworkPolicy (Maybe Text)
pzcsnpUploadType
  = lens _pzcsnpUploadType
      (\ s a -> s{_pzcsnpUploadType = a})

-- | The name of the Google Compute Engine
-- [zone](\/compute\/docs\/zones#available) in which the cluster resides.
pzcsnpZone :: Lens' ProjectsZonesClustersSetNetworkPolicy Text
pzcsnpZone
  = lens _pzcsnpZone (\ s a -> s{_pzcsnpZone = a})

-- | Multipart request metadata.
pzcsnpPayload :: Lens' ProjectsZonesClustersSetNetworkPolicy SetNetworkPolicyRequest
pzcsnpPayload
  = lens _pzcsnpPayload
      (\ s a -> s{_pzcsnpPayload = a})

-- | OAuth bearer token.
pzcsnpBearerToken :: Lens' ProjectsZonesClustersSetNetworkPolicy (Maybe Text)
pzcsnpBearerToken
  = lens _pzcsnpBearerToken
      (\ s a -> s{_pzcsnpBearerToken = a})

-- | The name of the cluster.
pzcsnpClusterId :: Lens' ProjectsZonesClustersSetNetworkPolicy Text
pzcsnpClusterId
  = lens _pzcsnpClusterId
      (\ s a -> s{_pzcsnpClusterId = a})

-- | The Google Developers Console [project ID or project
-- number](https:\/\/developers.google.com\/console\/help\/new\/#projectnumber).
pzcsnpProjectId :: Lens' ProjectsZonesClustersSetNetworkPolicy Text
pzcsnpProjectId
  = lens _pzcsnpProjectId
      (\ s a -> s{_pzcsnpProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pzcsnpFields :: Lens' ProjectsZonesClustersSetNetworkPolicy (Maybe Text)
pzcsnpFields
  = lens _pzcsnpFields (\ s a -> s{_pzcsnpFields = a})

-- | JSONP
pzcsnpCallback :: Lens' ProjectsZonesClustersSetNetworkPolicy (Maybe Text)
pzcsnpCallback
  = lens _pzcsnpCallback
      (\ s a -> s{_pzcsnpCallback = a})

instance GoogleRequest
         ProjectsZonesClustersSetNetworkPolicy where
        type Rs ProjectsZonesClustersSetNetworkPolicy =
             Operation
        type Scopes ProjectsZonesClustersSetNetworkPolicy =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient
          ProjectsZonesClustersSetNetworkPolicy'{..}
          = go _pzcsnpProjectId _pzcsnpZone _pzcsnpClusterId
              _pzcsnpXgafv
              _pzcsnpUploadProtocol
              (Just _pzcsnpPp)
              _pzcsnpAccessToken
              _pzcsnpUploadType
              _pzcsnpBearerToken
              _pzcsnpCallback
              _pzcsnpFields
              (Just AltJSON)
              _pzcsnpPayload
              containerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsZonesClustersSetNetworkPolicyResource)
                      mempty
