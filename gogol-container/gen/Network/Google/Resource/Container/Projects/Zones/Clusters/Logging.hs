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
-- Module      : Network.Google.Resource.Container.Projects.Zones.Clusters.Logging
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging service of a specific cluster.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference> for @container.projects.zones.clusters.logging@.
module Network.Google.Resource.Container.Projects.Zones.Clusters.Logging
    (
    -- * REST Resource
      ProjectsZonesClustersLoggingResource

    -- * Creating a Request
    , projectsZonesClustersLogging
    , ProjectsZonesClustersLogging

    -- * Request Lenses
    , pzclXgafv
    , pzclUploadProtocol
    , pzclPp
    , pzclAccessToken
    , pzclUploadType
    , pzclZone
    , pzclPayload
    , pzclBearerToken
    , pzclClusterId
    , pzclProjectId
    , pzclFields
    , pzclCallback
    ) where

import Network.Google.Container.Types
import Network.Google.Prelude

-- | A resource alias for @container.projects.zones.clusters.logging@ method which the
-- 'ProjectsZonesClustersLogging' request conforms to.
type ProjectsZonesClustersLoggingResource =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "zones" :>
             Capture "zone" Text :>
               "clusters" :>
                 Capture "clusterId" Text :>
                   "logging" :>
                     QueryParam "$.xgafv" Xgafv :>
                       QueryParam "upload_protocol" Text :>
                         QueryParam "pp" Bool :>
                           QueryParam "access_token" Text :>
                             QueryParam "uploadType" Text :>
                               QueryParam "bearer_token" Text :>
                                 QueryParam "callback" Text :>
                                   QueryParam "fields" Text :>
                                     QueryParam "alt" AltJSON :>
                                       ReqBody '[JSON] SetLoggingServiceRequest
                                         :> Post '[JSON] Operation

-- | Sets the logging service of a specific cluster.
--
-- /See:/ 'projectsZonesClustersLogging' smart constructor.
data ProjectsZonesClustersLogging = ProjectsZonesClustersLogging'
    { _pzclXgafv :: !(Maybe Xgafv)
    , _pzclUploadProtocol :: !(Maybe Text)
    , _pzclPp :: !Bool
    , _pzclAccessToken :: !(Maybe Text)
    , _pzclUploadType :: !(Maybe Text)
    , _pzclZone :: !Text
    , _pzclPayload :: !SetLoggingServiceRequest
    , _pzclBearerToken :: !(Maybe Text)
    , _pzclClusterId :: !Text
    , _pzclProjectId :: !Text
    , _pzclFields :: !(Maybe Text)
    , _pzclCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsZonesClustersLogging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pzclXgafv'
--
-- * 'pzclUploadProtocol'
--
-- * 'pzclPp'
--
-- * 'pzclAccessToken'
--
-- * 'pzclUploadType'
--
-- * 'pzclZone'
--
-- * 'pzclPayload'
--
-- * 'pzclBearerToken'
--
-- * 'pzclClusterId'
--
-- * 'pzclProjectId'
--
-- * 'pzclFields'
--
-- * 'pzclCallback'
projectsZonesClustersLogging
    :: Text -- ^ 'pzclZone'
    -> SetLoggingServiceRequest -- ^ 'pzclPayload'
    -> Text -- ^ 'pzclClusterId'
    -> Text -- ^ 'pzclProjectId'
    -> ProjectsZonesClustersLogging
projectsZonesClustersLogging pPzclZone_ pPzclPayload_ pPzclClusterId_ pPzclProjectId_ = 
    ProjectsZonesClustersLogging'
    { _pzclXgafv = Nothing
    , _pzclUploadProtocol = Nothing
    , _pzclPp = True
    , _pzclAccessToken = Nothing
    , _pzclUploadType = Nothing
    , _pzclZone = pPzclZone_
    , _pzclPayload = pPzclPayload_
    , _pzclBearerToken = Nothing
    , _pzclClusterId = pPzclClusterId_
    , _pzclProjectId = pPzclProjectId_
    , _pzclFields = Nothing
    , _pzclCallback = Nothing
    }

-- | V1 error format.
pzclXgafv :: Lens' ProjectsZonesClustersLogging (Maybe Xgafv)
pzclXgafv
  = lens _pzclXgafv (\ s a -> s{_pzclXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pzclUploadProtocol :: Lens' ProjectsZonesClustersLogging (Maybe Text)
pzclUploadProtocol
  = lens _pzclUploadProtocol
      (\ s a -> s{_pzclUploadProtocol = a})

-- | Pretty-print response.
pzclPp :: Lens' ProjectsZonesClustersLogging Bool
pzclPp = lens _pzclPp (\ s a -> s{_pzclPp = a})

-- | OAuth access token.
pzclAccessToken :: Lens' ProjectsZonesClustersLogging (Maybe Text)
pzclAccessToken
  = lens _pzclAccessToken
      (\ s a -> s{_pzclAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pzclUploadType :: Lens' ProjectsZonesClustersLogging (Maybe Text)
pzclUploadType
  = lens _pzclUploadType
      (\ s a -> s{_pzclUploadType = a})

-- | The name of the Google Compute Engine
-- [zone](\/compute\/docs\/zones#available) in which the cluster resides.
pzclZone :: Lens' ProjectsZonesClustersLogging Text
pzclZone = lens _pzclZone (\ s a -> s{_pzclZone = a})

-- | Multipart request metadata.
pzclPayload :: Lens' ProjectsZonesClustersLogging SetLoggingServiceRequest
pzclPayload
  = lens _pzclPayload (\ s a -> s{_pzclPayload = a})

-- | OAuth bearer token.
pzclBearerToken :: Lens' ProjectsZonesClustersLogging (Maybe Text)
pzclBearerToken
  = lens _pzclBearerToken
      (\ s a -> s{_pzclBearerToken = a})

-- | The name of the cluster to upgrade.
pzclClusterId :: Lens' ProjectsZonesClustersLogging Text
pzclClusterId
  = lens _pzclClusterId
      (\ s a -> s{_pzclClusterId = a})

-- | The Google Developers Console [project ID or project
-- number](https:\/\/support.google.com\/cloud\/answer\/6158840).
pzclProjectId :: Lens' ProjectsZonesClustersLogging Text
pzclProjectId
  = lens _pzclProjectId
      (\ s a -> s{_pzclProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pzclFields :: Lens' ProjectsZonesClustersLogging (Maybe Text)
pzclFields
  = lens _pzclFields (\ s a -> s{_pzclFields = a})

-- | JSONP
pzclCallback :: Lens' ProjectsZonesClustersLogging (Maybe Text)
pzclCallback
  = lens _pzclCallback (\ s a -> s{_pzclCallback = a})

instance GoogleRequest ProjectsZonesClustersLogging
         where
        type Rs ProjectsZonesClustersLogging = Operation
        type Scopes ProjectsZonesClustersLogging =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsZonesClustersLogging'{..}
          = go _pzclProjectId _pzclZone _pzclClusterId
              _pzclXgafv
              _pzclUploadProtocol
              (Just _pzclPp)
              _pzclAccessToken
              _pzclUploadType
              _pzclBearerToken
              _pzclCallback
              _pzclFields
              (Just AltJSON)
              _pzclPayload
              containerService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsZonesClustersLoggingResource)
                      mempty
