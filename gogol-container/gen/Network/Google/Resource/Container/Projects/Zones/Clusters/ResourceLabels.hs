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
-- Module      : Network.Google.Resource.Container.Projects.Zones.Clusters.ResourceLabels
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets labels on a cluster.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference> for @container.projects.zones.clusters.resourceLabels@.
module Network.Google.Resource.Container.Projects.Zones.Clusters.ResourceLabels
    (
    -- * REST Resource
      ProjectsZonesClustersResourceLabelsResource

    -- * Creating a Request
    , projectsZonesClustersResourceLabels
    , ProjectsZonesClustersResourceLabels

    -- * Request Lenses
    , pzcrlXgafv
    , pzcrlUploadProtocol
    , pzcrlPp
    , pzcrlAccessToken
    , pzcrlUploadType
    , pzcrlZone
    , pzcrlPayload
    , pzcrlBearerToken
    , pzcrlClusterId
    , pzcrlProjectId
    , pzcrlFields
    , pzcrlCallback
    ) where

import Network.Google.Container.Types
import Network.Google.Prelude

-- | A resource alias for @container.projects.zones.clusters.resourceLabels@ method which the
-- 'ProjectsZonesClustersResourceLabels' request conforms to.
type ProjectsZonesClustersResourceLabelsResource =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "zones" :>
             Capture "zone" Text :>
               "clusters" :>
                 Capture "clusterId" Text :>
                   "resourceLabels" :>
                     QueryParam "$.xgafv" Xgafv :>
                       QueryParam "upload_protocol" Text :>
                         QueryParam "pp" Bool :>
                           QueryParam "access_token" Text :>
                             QueryParam "uploadType" Text :>
                               QueryParam "bearer_token" Text :>
                                 QueryParam "callback" Text :>
                                   QueryParam "fields" Text :>
                                     QueryParam "alt" AltJSON :>
                                       ReqBody '[JSON] SetLabelsRequest :>
                                         Post '[JSON] Operation

-- | Sets labels on a cluster.
--
-- /See:/ 'projectsZonesClustersResourceLabels' smart constructor.
data ProjectsZonesClustersResourceLabels = ProjectsZonesClustersResourceLabels'
    { _pzcrlXgafv :: !(Maybe Xgafv)
    , _pzcrlUploadProtocol :: !(Maybe Text)
    , _pzcrlPp :: !Bool
    , _pzcrlAccessToken :: !(Maybe Text)
    , _pzcrlUploadType :: !(Maybe Text)
    , _pzcrlZone :: !Text
    , _pzcrlPayload :: !SetLabelsRequest
    , _pzcrlBearerToken :: !(Maybe Text)
    , _pzcrlClusterId :: !Text
    , _pzcrlProjectId :: !Text
    , _pzcrlFields :: !(Maybe Text)
    , _pzcrlCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsZonesClustersResourceLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pzcrlXgafv'
--
-- * 'pzcrlUploadProtocol'
--
-- * 'pzcrlPp'
--
-- * 'pzcrlAccessToken'
--
-- * 'pzcrlUploadType'
--
-- * 'pzcrlZone'
--
-- * 'pzcrlPayload'
--
-- * 'pzcrlBearerToken'
--
-- * 'pzcrlClusterId'
--
-- * 'pzcrlProjectId'
--
-- * 'pzcrlFields'
--
-- * 'pzcrlCallback'
projectsZonesClustersResourceLabels
    :: Text -- ^ 'pzcrlZone'
    -> SetLabelsRequest -- ^ 'pzcrlPayload'
    -> Text -- ^ 'pzcrlClusterId'
    -> Text -- ^ 'pzcrlProjectId'
    -> ProjectsZonesClustersResourceLabels
projectsZonesClustersResourceLabels pPzcrlZone_ pPzcrlPayload_ pPzcrlClusterId_ pPzcrlProjectId_ = 
    ProjectsZonesClustersResourceLabels'
    { _pzcrlXgafv = Nothing
    , _pzcrlUploadProtocol = Nothing
    , _pzcrlPp = True
    , _pzcrlAccessToken = Nothing
    , _pzcrlUploadType = Nothing
    , _pzcrlZone = pPzcrlZone_
    , _pzcrlPayload = pPzcrlPayload_
    , _pzcrlBearerToken = Nothing
    , _pzcrlClusterId = pPzcrlClusterId_
    , _pzcrlProjectId = pPzcrlProjectId_
    , _pzcrlFields = Nothing
    , _pzcrlCallback = Nothing
    }

-- | V1 error format.
pzcrlXgafv :: Lens' ProjectsZonesClustersResourceLabels (Maybe Xgafv)
pzcrlXgafv
  = lens _pzcrlXgafv (\ s a -> s{_pzcrlXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pzcrlUploadProtocol :: Lens' ProjectsZonesClustersResourceLabels (Maybe Text)
pzcrlUploadProtocol
  = lens _pzcrlUploadProtocol
      (\ s a -> s{_pzcrlUploadProtocol = a})

-- | Pretty-print response.
pzcrlPp :: Lens' ProjectsZonesClustersResourceLabels Bool
pzcrlPp = lens _pzcrlPp (\ s a -> s{_pzcrlPp = a})

-- | OAuth access token.
pzcrlAccessToken :: Lens' ProjectsZonesClustersResourceLabels (Maybe Text)
pzcrlAccessToken
  = lens _pzcrlAccessToken
      (\ s a -> s{_pzcrlAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pzcrlUploadType :: Lens' ProjectsZonesClustersResourceLabels (Maybe Text)
pzcrlUploadType
  = lens _pzcrlUploadType
      (\ s a -> s{_pzcrlUploadType = a})

-- | The name of the Google Compute Engine
-- [zone](\/compute\/docs\/zones#available) in which the cluster resides.
pzcrlZone :: Lens' ProjectsZonesClustersResourceLabels Text
pzcrlZone
  = lens _pzcrlZone (\ s a -> s{_pzcrlZone = a})

-- | Multipart request metadata.
pzcrlPayload :: Lens' ProjectsZonesClustersResourceLabels SetLabelsRequest
pzcrlPayload
  = lens _pzcrlPayload (\ s a -> s{_pzcrlPayload = a})

-- | OAuth bearer token.
pzcrlBearerToken :: Lens' ProjectsZonesClustersResourceLabels (Maybe Text)
pzcrlBearerToken
  = lens _pzcrlBearerToken
      (\ s a -> s{_pzcrlBearerToken = a})

-- | The name of the cluster.
pzcrlClusterId :: Lens' ProjectsZonesClustersResourceLabels Text
pzcrlClusterId
  = lens _pzcrlClusterId
      (\ s a -> s{_pzcrlClusterId = a})

-- | The Google Developers Console [project ID or project
-- number](https:\/\/developers.google.com\/console\/help\/new\/#projectnumber).
pzcrlProjectId :: Lens' ProjectsZonesClustersResourceLabels Text
pzcrlProjectId
  = lens _pzcrlProjectId
      (\ s a -> s{_pzcrlProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pzcrlFields :: Lens' ProjectsZonesClustersResourceLabels (Maybe Text)
pzcrlFields
  = lens _pzcrlFields (\ s a -> s{_pzcrlFields = a})

-- | JSONP
pzcrlCallback :: Lens' ProjectsZonesClustersResourceLabels (Maybe Text)
pzcrlCallback
  = lens _pzcrlCallback
      (\ s a -> s{_pzcrlCallback = a})

instance GoogleRequest
         ProjectsZonesClustersResourceLabels where
        type Rs ProjectsZonesClustersResourceLabels =
             Operation
        type Scopes ProjectsZonesClustersResourceLabels =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient
          ProjectsZonesClustersResourceLabels'{..}
          = go _pzcrlProjectId _pzcrlZone _pzcrlClusterId
              _pzcrlXgafv
              _pzcrlUploadProtocol
              (Just _pzcrlPp)
              _pzcrlAccessToken
              _pzcrlUploadType
              _pzcrlBearerToken
              _pzcrlCallback
              _pzcrlFields
              (Just AltJSON)
              _pzcrlPayload
              containerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsZonesClustersResourceLabelsResource)
                      mempty
