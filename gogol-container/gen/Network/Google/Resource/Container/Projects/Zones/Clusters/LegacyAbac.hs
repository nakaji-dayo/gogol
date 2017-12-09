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
-- Module      : Network.Google.Resource.Container.Projects.Zones.Clusters.LegacyAbac
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables the ABAC authorization mechanism on a cluster.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference> for @container.projects.zones.clusters.legacyAbac@.
module Network.Google.Resource.Container.Projects.Zones.Clusters.LegacyAbac
    (
    -- * REST Resource
      ProjectsZonesClustersLegacyAbacResource

    -- * Creating a Request
    , projectsZonesClustersLegacyAbac
    , ProjectsZonesClustersLegacyAbac

    -- * Request Lenses
    , pzclaXgafv
    , pzclaUploadProtocol
    , pzclaPp
    , pzclaAccessToken
    , pzclaUploadType
    , pzclaZone
    , pzclaPayload
    , pzclaBearerToken
    , pzclaClusterId
    , pzclaProjectId
    , pzclaFields
    , pzclaCallback
    ) where

import Network.Google.Container.Types
import Network.Google.Prelude

-- | A resource alias for @container.projects.zones.clusters.legacyAbac@ method which the
-- 'ProjectsZonesClustersLegacyAbac' request conforms to.
type ProjectsZonesClustersLegacyAbacResource =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "zones" :>
             Capture "zone" Text :>
               "clusters" :>
                 Capture "clusterId" Text :>
                   "legacyAbac" :>
                     QueryParam "$.xgafv" Xgafv :>
                       QueryParam "upload_protocol" Text :>
                         QueryParam "pp" Bool :>
                           QueryParam "access_token" Text :>
                             QueryParam "uploadType" Text :>
                               QueryParam "bearer_token" Text :>
                                 QueryParam "callback" Text :>
                                   QueryParam "fields" Text :>
                                     QueryParam "alt" AltJSON :>
                                       ReqBody '[JSON] SetLegacyAbacRequest :>
                                         Post '[JSON] Operation

-- | Enables or disables the ABAC authorization mechanism on a cluster.
--
-- /See:/ 'projectsZonesClustersLegacyAbac' smart constructor.
data ProjectsZonesClustersLegacyAbac = ProjectsZonesClustersLegacyAbac'
    { _pzclaXgafv :: !(Maybe Xgafv)
    , _pzclaUploadProtocol :: !(Maybe Text)
    , _pzclaPp :: !Bool
    , _pzclaAccessToken :: !(Maybe Text)
    , _pzclaUploadType :: !(Maybe Text)
    , _pzclaZone :: !Text
    , _pzclaPayload :: !SetLegacyAbacRequest
    , _pzclaBearerToken :: !(Maybe Text)
    , _pzclaClusterId :: !Text
    , _pzclaProjectId :: !Text
    , _pzclaFields :: !(Maybe Text)
    , _pzclaCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsZonesClustersLegacyAbac' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pzclaXgafv'
--
-- * 'pzclaUploadProtocol'
--
-- * 'pzclaPp'
--
-- * 'pzclaAccessToken'
--
-- * 'pzclaUploadType'
--
-- * 'pzclaZone'
--
-- * 'pzclaPayload'
--
-- * 'pzclaBearerToken'
--
-- * 'pzclaClusterId'
--
-- * 'pzclaProjectId'
--
-- * 'pzclaFields'
--
-- * 'pzclaCallback'
projectsZonesClustersLegacyAbac
    :: Text -- ^ 'pzclaZone'
    -> SetLegacyAbacRequest -- ^ 'pzclaPayload'
    -> Text -- ^ 'pzclaClusterId'
    -> Text -- ^ 'pzclaProjectId'
    -> ProjectsZonesClustersLegacyAbac
projectsZonesClustersLegacyAbac pPzclaZone_ pPzclaPayload_ pPzclaClusterId_ pPzclaProjectId_ = 
    ProjectsZonesClustersLegacyAbac'
    { _pzclaXgafv = Nothing
    , _pzclaUploadProtocol = Nothing
    , _pzclaPp = True
    , _pzclaAccessToken = Nothing
    , _pzclaUploadType = Nothing
    , _pzclaZone = pPzclaZone_
    , _pzclaPayload = pPzclaPayload_
    , _pzclaBearerToken = Nothing
    , _pzclaClusterId = pPzclaClusterId_
    , _pzclaProjectId = pPzclaProjectId_
    , _pzclaFields = Nothing
    , _pzclaCallback = Nothing
    }

-- | V1 error format.
pzclaXgafv :: Lens' ProjectsZonesClustersLegacyAbac (Maybe Xgafv)
pzclaXgafv
  = lens _pzclaXgafv (\ s a -> s{_pzclaXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pzclaUploadProtocol :: Lens' ProjectsZonesClustersLegacyAbac (Maybe Text)
pzclaUploadProtocol
  = lens _pzclaUploadProtocol
      (\ s a -> s{_pzclaUploadProtocol = a})

-- | Pretty-print response.
pzclaPp :: Lens' ProjectsZonesClustersLegacyAbac Bool
pzclaPp = lens _pzclaPp (\ s a -> s{_pzclaPp = a})

-- | OAuth access token.
pzclaAccessToken :: Lens' ProjectsZonesClustersLegacyAbac (Maybe Text)
pzclaAccessToken
  = lens _pzclaAccessToken
      (\ s a -> s{_pzclaAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pzclaUploadType :: Lens' ProjectsZonesClustersLegacyAbac (Maybe Text)
pzclaUploadType
  = lens _pzclaUploadType
      (\ s a -> s{_pzclaUploadType = a})

-- | The name of the Google Compute Engine
-- [zone](\/compute\/docs\/zones#available) in which the cluster resides.
pzclaZone :: Lens' ProjectsZonesClustersLegacyAbac Text
pzclaZone
  = lens _pzclaZone (\ s a -> s{_pzclaZone = a})

-- | Multipart request metadata.
pzclaPayload :: Lens' ProjectsZonesClustersLegacyAbac SetLegacyAbacRequest
pzclaPayload
  = lens _pzclaPayload (\ s a -> s{_pzclaPayload = a})

-- | OAuth bearer token.
pzclaBearerToken :: Lens' ProjectsZonesClustersLegacyAbac (Maybe Text)
pzclaBearerToken
  = lens _pzclaBearerToken
      (\ s a -> s{_pzclaBearerToken = a})

-- | The name of the cluster to update.
pzclaClusterId :: Lens' ProjectsZonesClustersLegacyAbac Text
pzclaClusterId
  = lens _pzclaClusterId
      (\ s a -> s{_pzclaClusterId = a})

-- | The Google Developers Console [project ID or project
-- number](https:\/\/support.google.com\/cloud\/answer\/6158840).
pzclaProjectId :: Lens' ProjectsZonesClustersLegacyAbac Text
pzclaProjectId
  = lens _pzclaProjectId
      (\ s a -> s{_pzclaProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pzclaFields :: Lens' ProjectsZonesClustersLegacyAbac (Maybe Text)
pzclaFields
  = lens _pzclaFields (\ s a -> s{_pzclaFields = a})

-- | JSONP
pzclaCallback :: Lens' ProjectsZonesClustersLegacyAbac (Maybe Text)
pzclaCallback
  = lens _pzclaCallback
      (\ s a -> s{_pzclaCallback = a})

instance GoogleRequest
         ProjectsZonesClustersLegacyAbac where
        type Rs ProjectsZonesClustersLegacyAbac = Operation
        type Scopes ProjectsZonesClustersLegacyAbac =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsZonesClustersLegacyAbac'{..}
          = go _pzclaProjectId _pzclaZone _pzclaClusterId
              _pzclaXgafv
              _pzclaUploadProtocol
              (Just _pzclaPp)
              _pzclaAccessToken
              _pzclaUploadType
              _pzclaBearerToken
              _pzclaCallback
              _pzclaFields
              (Just AltJSON)
              _pzclaPayload
              containerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsZonesClustersLegacyAbacResource)
                      mempty
