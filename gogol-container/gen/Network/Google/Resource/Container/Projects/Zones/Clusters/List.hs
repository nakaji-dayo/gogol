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
-- Module      : Network.Google.Resource.Container.Projects.Zones.Clusters.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all clusters owned by a project in either the specified zone or
-- all zones.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference> for @container.projects.zones.clusters.list@.
module Network.Google.Resource.Container.Projects.Zones.Clusters.List
    (
    -- * REST Resource
      ProjectsZonesClustersListResource

    -- * Creating a Request
    , projectsZonesClustersList
    , ProjectsZonesClustersList

    -- * Request Lenses
    , proXgafv
    , proUploadProtocol
    , proPp
    , proAccessToken
    , proUploadType
    , proZone
    , proBearerToken
    , proProjectId
    , proFields
    , proCallback
    ) where

import Network.Google.Container.Types
import Network.Google.Prelude

-- | A resource alias for @container.projects.zones.clusters.list@ method which the
-- 'ProjectsZonesClustersList' request conforms to.
type ProjectsZonesClustersListResource =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "zones" :>
             Capture "zone" Text :>
               "clusters" :>
                 QueryParam "$.xgafv" Xgafv :>
                   QueryParam "upload_protocol" Text :>
                     QueryParam "pp" Bool :>
                       QueryParam "access_token" Text :>
                         QueryParam "uploadType" Text :>
                           QueryParam "bearer_token" Text :>
                             QueryParam "callback" Text :>
                               QueryParam "fields" Text :>
                                 QueryParam "alt" AltJSON :>
                                   Get '[JSON] ListClustersResponse

-- | Lists all clusters owned by a project in either the specified zone or
-- all zones.
--
-- /See:/ 'projectsZonesClustersList' smart constructor.
data ProjectsZonesClustersList = ProjectsZonesClustersList'
    { _proXgafv :: !(Maybe Xgafv)
    , _proUploadProtocol :: !(Maybe Text)
    , _proPp :: !Bool
    , _proAccessToken :: !(Maybe Text)
    , _proUploadType :: !(Maybe Text)
    , _proZone :: !Text
    , _proBearerToken :: !(Maybe Text)
    , _proProjectId :: !Text
    , _proFields :: !(Maybe Text)
    , _proCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsZonesClustersList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'proXgafv'
--
-- * 'proUploadProtocol'
--
-- * 'proPp'
--
-- * 'proAccessToken'
--
-- * 'proUploadType'
--
-- * 'proZone'
--
-- * 'proBearerToken'
--
-- * 'proProjectId'
--
-- * 'proFields'
--
-- * 'proCallback'
projectsZonesClustersList
    :: Text -- ^ 'proZone'
    -> Text -- ^ 'proProjectId'
    -> ProjectsZonesClustersList
projectsZonesClustersList pProZone_ pProProjectId_ = 
    ProjectsZonesClustersList'
    { _proXgafv = Nothing
    , _proUploadProtocol = Nothing
    , _proPp = True
    , _proAccessToken = Nothing
    , _proUploadType = Nothing
    , _proZone = pProZone_
    , _proBearerToken = Nothing
    , _proProjectId = pProProjectId_
    , _proFields = Nothing
    , _proCallback = Nothing
    }

-- | V1 error format.
proXgafv :: Lens' ProjectsZonesClustersList (Maybe Xgafv)
proXgafv = lens _proXgafv (\ s a -> s{_proXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
proUploadProtocol :: Lens' ProjectsZonesClustersList (Maybe Text)
proUploadProtocol
  = lens _proUploadProtocol
      (\ s a -> s{_proUploadProtocol = a})

-- | Pretty-print response.
proPp :: Lens' ProjectsZonesClustersList Bool
proPp = lens _proPp (\ s a -> s{_proPp = a})

-- | OAuth access token.
proAccessToken :: Lens' ProjectsZonesClustersList (Maybe Text)
proAccessToken
  = lens _proAccessToken
      (\ s a -> s{_proAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
proUploadType :: Lens' ProjectsZonesClustersList (Maybe Text)
proUploadType
  = lens _proUploadType
      (\ s a -> s{_proUploadType = a})

-- | The name of the Google Compute Engine
-- [zone](\/compute\/docs\/zones#available) in which the cluster resides,
-- or \"-\" for all zones.
proZone :: Lens' ProjectsZonesClustersList Text
proZone = lens _proZone (\ s a -> s{_proZone = a})

-- | OAuth bearer token.
proBearerToken :: Lens' ProjectsZonesClustersList (Maybe Text)
proBearerToken
  = lens _proBearerToken
      (\ s a -> s{_proBearerToken = a})

-- | The Google Developers Console [project ID or project
-- number](https:\/\/support.google.com\/cloud\/answer\/6158840).
proProjectId :: Lens' ProjectsZonesClustersList Text
proProjectId
  = lens _proProjectId (\ s a -> s{_proProjectId = a})

-- | Selector specifying which fields to include in a partial response.
proFields :: Lens' ProjectsZonesClustersList (Maybe Text)
proFields
  = lens _proFields (\ s a -> s{_proFields = a})

-- | JSONP
proCallback :: Lens' ProjectsZonesClustersList (Maybe Text)
proCallback
  = lens _proCallback (\ s a -> s{_proCallback = a})

instance GoogleRequest ProjectsZonesClustersList
         where
        type Rs ProjectsZonesClustersList =
             ListClustersResponse
        type Scopes ProjectsZonesClustersList =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsZonesClustersList'{..}
          = go _proProjectId _proZone _proXgafv
              _proUploadProtocol
              (Just _proPp)
              _proAccessToken
              _proUploadType
              _proBearerToken
              _proCallback
              _proFields
              (Just AltJSON)
              containerService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsZonesClustersListResource)
                      mempty
