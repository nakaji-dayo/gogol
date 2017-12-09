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
-- Module      : Network.Google.Resource.Container.Projects.Zones.Clusters.Addons
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the addons of a specific cluster.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference> for @container.projects.zones.clusters.addons@.
module Network.Google.Resource.Container.Projects.Zones.Clusters.Addons
    (
    -- * REST Resource
      ProjectsZonesClustersAddonsResource

    -- * Creating a Request
    , projectsZonesClustersAddons
    , ProjectsZonesClustersAddons

    -- * Request Lenses
    , pzcaXgafv
    , pzcaUploadProtocol
    , pzcaPp
    , pzcaAccessToken
    , pzcaUploadType
    , pzcaZone
    , pzcaPayload
    , pzcaBearerToken
    , pzcaClusterId
    , pzcaProjectId
    , pzcaFields
    , pzcaCallback
    ) where

import Network.Google.Container.Types
import Network.Google.Prelude

-- | A resource alias for @container.projects.zones.clusters.addons@ method which the
-- 'ProjectsZonesClustersAddons' request conforms to.
type ProjectsZonesClustersAddonsResource =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "zones" :>
             Capture "zone" Text :>
               "clusters" :>
                 Capture "clusterId" Text :>
                   "addons" :>
                     QueryParam "$.xgafv" Xgafv :>
                       QueryParam "upload_protocol" Text :>
                         QueryParam "pp" Bool :>
                           QueryParam "access_token" Text :>
                             QueryParam "uploadType" Text :>
                               QueryParam "bearer_token" Text :>
                                 QueryParam "callback" Text :>
                                   QueryParam "fields" Text :>
                                     QueryParam "alt" AltJSON :>
                                       ReqBody '[JSON] SetAddonsConfigRequest :>
                                         Post '[JSON] Operation

-- | Sets the addons of a specific cluster.
--
-- /See:/ 'projectsZonesClustersAddons' smart constructor.
data ProjectsZonesClustersAddons = ProjectsZonesClustersAddons'
    { _pzcaXgafv :: !(Maybe Xgafv)
    , _pzcaUploadProtocol :: !(Maybe Text)
    , _pzcaPp :: !Bool
    , _pzcaAccessToken :: !(Maybe Text)
    , _pzcaUploadType :: !(Maybe Text)
    , _pzcaZone :: !Text
    , _pzcaPayload :: !SetAddonsConfigRequest
    , _pzcaBearerToken :: !(Maybe Text)
    , _pzcaClusterId :: !Text
    , _pzcaProjectId :: !Text
    , _pzcaFields :: !(Maybe Text)
    , _pzcaCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsZonesClustersAddons' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pzcaXgafv'
--
-- * 'pzcaUploadProtocol'
--
-- * 'pzcaPp'
--
-- * 'pzcaAccessToken'
--
-- * 'pzcaUploadType'
--
-- * 'pzcaZone'
--
-- * 'pzcaPayload'
--
-- * 'pzcaBearerToken'
--
-- * 'pzcaClusterId'
--
-- * 'pzcaProjectId'
--
-- * 'pzcaFields'
--
-- * 'pzcaCallback'
projectsZonesClustersAddons
    :: Text -- ^ 'pzcaZone'
    -> SetAddonsConfigRequest -- ^ 'pzcaPayload'
    -> Text -- ^ 'pzcaClusterId'
    -> Text -- ^ 'pzcaProjectId'
    -> ProjectsZonesClustersAddons
projectsZonesClustersAddons pPzcaZone_ pPzcaPayload_ pPzcaClusterId_ pPzcaProjectId_ = 
    ProjectsZonesClustersAddons'
    { _pzcaXgafv = Nothing
    , _pzcaUploadProtocol = Nothing
    , _pzcaPp = True
    , _pzcaAccessToken = Nothing
    , _pzcaUploadType = Nothing
    , _pzcaZone = pPzcaZone_
    , _pzcaPayload = pPzcaPayload_
    , _pzcaBearerToken = Nothing
    , _pzcaClusterId = pPzcaClusterId_
    , _pzcaProjectId = pPzcaProjectId_
    , _pzcaFields = Nothing
    , _pzcaCallback = Nothing
    }

-- | V1 error format.
pzcaXgafv :: Lens' ProjectsZonesClustersAddons (Maybe Xgafv)
pzcaXgafv
  = lens _pzcaXgafv (\ s a -> s{_pzcaXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pzcaUploadProtocol :: Lens' ProjectsZonesClustersAddons (Maybe Text)
pzcaUploadProtocol
  = lens _pzcaUploadProtocol
      (\ s a -> s{_pzcaUploadProtocol = a})

-- | Pretty-print response.
pzcaPp :: Lens' ProjectsZonesClustersAddons Bool
pzcaPp = lens _pzcaPp (\ s a -> s{_pzcaPp = a})

-- | OAuth access token.
pzcaAccessToken :: Lens' ProjectsZonesClustersAddons (Maybe Text)
pzcaAccessToken
  = lens _pzcaAccessToken
      (\ s a -> s{_pzcaAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pzcaUploadType :: Lens' ProjectsZonesClustersAddons (Maybe Text)
pzcaUploadType
  = lens _pzcaUploadType
      (\ s a -> s{_pzcaUploadType = a})

-- | The name of the Google Compute Engine
-- [zone](\/compute\/docs\/zones#available) in which the cluster resides.
pzcaZone :: Lens' ProjectsZonesClustersAddons Text
pzcaZone = lens _pzcaZone (\ s a -> s{_pzcaZone = a})

-- | Multipart request metadata.
pzcaPayload :: Lens' ProjectsZonesClustersAddons SetAddonsConfigRequest
pzcaPayload
  = lens _pzcaPayload (\ s a -> s{_pzcaPayload = a})

-- | OAuth bearer token.
pzcaBearerToken :: Lens' ProjectsZonesClustersAddons (Maybe Text)
pzcaBearerToken
  = lens _pzcaBearerToken
      (\ s a -> s{_pzcaBearerToken = a})

-- | The name of the cluster to upgrade.
pzcaClusterId :: Lens' ProjectsZonesClustersAddons Text
pzcaClusterId
  = lens _pzcaClusterId
      (\ s a -> s{_pzcaClusterId = a})

-- | The Google Developers Console [project ID or project
-- number](https:\/\/support.google.com\/cloud\/answer\/6158840).
pzcaProjectId :: Lens' ProjectsZonesClustersAddons Text
pzcaProjectId
  = lens _pzcaProjectId
      (\ s a -> s{_pzcaProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pzcaFields :: Lens' ProjectsZonesClustersAddons (Maybe Text)
pzcaFields
  = lens _pzcaFields (\ s a -> s{_pzcaFields = a})

-- | JSONP
pzcaCallback :: Lens' ProjectsZonesClustersAddons (Maybe Text)
pzcaCallback
  = lens _pzcaCallback (\ s a -> s{_pzcaCallback = a})

instance GoogleRequest ProjectsZonesClustersAddons
         where
        type Rs ProjectsZonesClustersAddons = Operation
        type Scopes ProjectsZonesClustersAddons =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsZonesClustersAddons'{..}
          = go _pzcaProjectId _pzcaZone _pzcaClusterId
              _pzcaXgafv
              _pzcaUploadProtocol
              (Just _pzcaPp)
              _pzcaAccessToken
              _pzcaUploadType
              _pzcaBearerToken
              _pzcaCallback
              _pzcaFields
              (Just AltJSON)
              _pzcaPayload
              containerService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsZonesClustersAddonsResource)
                      mempty
