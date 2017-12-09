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
-- Module      : Network.Google.Resource.Dataflow.Projects.Locations.WorkerMessages
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Send a worker_message to the service.
--
-- /See:/ <https://cloud.google.com/dataflow Google Dataflow API Reference> for @dataflow.projects.locations.workerMessages@.
module Network.Google.Resource.Dataflow.Projects.Locations.WorkerMessages
    (
    -- * REST Resource
      ProjectsLocationsWorkerMessagesResource

    -- * Creating a Request
    , projectsLocationsWorkerMessages
    , ProjectsLocationsWorkerMessages

    -- * Request Lenses
    , plwmXgafv
    , plwmUploadProtocol
    , plwmLocation
    , plwmPp
    , plwmAccessToken
    , plwmUploadType
    , plwmPayload
    , plwmBearerToken
    , plwmProjectId
    , plwmFields
    , plwmCallback
    ) where

import Network.Google.Dataflow.Types
import Network.Google.Prelude

-- | A resource alias for @dataflow.projects.locations.workerMessages@ method which the
-- 'ProjectsLocationsWorkerMessages' request conforms to.
type ProjectsLocationsWorkerMessagesResource =
     "v1b3" :>
       "projects" :>
         Capture "projectId" Text :>
           "locations" :>
             Capture "location" Text :>
               "WorkerMessages" :>
                 QueryParam "$.xgafv" Xgafv :>
                   QueryParam "upload_protocol" Text :>
                     QueryParam "pp" Bool :>
                       QueryParam "access_token" Text :>
                         QueryParam "uploadType" Text :>
                           QueryParam "bearer_token" Text :>
                             QueryParam "callback" Text :>
                               QueryParam "fields" Text :>
                                 QueryParam "alt" AltJSON :>
                                   ReqBody '[JSON] SendWorkerMessagesRequest :>
                                     Post '[JSON] SendWorkerMessagesResponse

-- | Send a worker_message to the service.
--
-- /See:/ 'projectsLocationsWorkerMessages' smart constructor.
data ProjectsLocationsWorkerMessages = ProjectsLocationsWorkerMessages'
    { _plwmXgafv :: !(Maybe Xgafv)
    , _plwmUploadProtocol :: !(Maybe Text)
    , _plwmLocation :: !Text
    , _plwmPp :: !Bool
    , _plwmAccessToken :: !(Maybe Text)
    , _plwmUploadType :: !(Maybe Text)
    , _plwmPayload :: !SendWorkerMessagesRequest
    , _plwmBearerToken :: !(Maybe Text)
    , _plwmProjectId :: !Text
    , _plwmFields :: !(Maybe Text)
    , _plwmCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsLocationsWorkerMessages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plwmXgafv'
--
-- * 'plwmUploadProtocol'
--
-- * 'plwmLocation'
--
-- * 'plwmPp'
--
-- * 'plwmAccessToken'
--
-- * 'plwmUploadType'
--
-- * 'plwmPayload'
--
-- * 'plwmBearerToken'
--
-- * 'plwmProjectId'
--
-- * 'plwmFields'
--
-- * 'plwmCallback'
projectsLocationsWorkerMessages
    :: Text -- ^ 'plwmLocation'
    -> SendWorkerMessagesRequest -- ^ 'plwmPayload'
    -> Text -- ^ 'plwmProjectId'
    -> ProjectsLocationsWorkerMessages
projectsLocationsWorkerMessages pPlwmLocation_ pPlwmPayload_ pPlwmProjectId_ = 
    ProjectsLocationsWorkerMessages'
    { _plwmXgafv = Nothing
    , _plwmUploadProtocol = Nothing
    , _plwmLocation = pPlwmLocation_
    , _plwmPp = True
    , _plwmAccessToken = Nothing
    , _plwmUploadType = Nothing
    , _plwmPayload = pPlwmPayload_
    , _plwmBearerToken = Nothing
    , _plwmProjectId = pPlwmProjectId_
    , _plwmFields = Nothing
    , _plwmCallback = Nothing
    }

-- | V1 error format.
plwmXgafv :: Lens' ProjectsLocationsWorkerMessages (Maybe Xgafv)
plwmXgafv
  = lens _plwmXgafv (\ s a -> s{_plwmXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
plwmUploadProtocol :: Lens' ProjectsLocationsWorkerMessages (Maybe Text)
plwmUploadProtocol
  = lens _plwmUploadProtocol
      (\ s a -> s{_plwmUploadProtocol = a})

-- | The location which contains the job
plwmLocation :: Lens' ProjectsLocationsWorkerMessages Text
plwmLocation
  = lens _plwmLocation (\ s a -> s{_plwmLocation = a})

-- | Pretty-print response.
plwmPp :: Lens' ProjectsLocationsWorkerMessages Bool
plwmPp = lens _plwmPp (\ s a -> s{_plwmPp = a})

-- | OAuth access token.
plwmAccessToken :: Lens' ProjectsLocationsWorkerMessages (Maybe Text)
plwmAccessToken
  = lens _plwmAccessToken
      (\ s a -> s{_plwmAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
plwmUploadType :: Lens' ProjectsLocationsWorkerMessages (Maybe Text)
plwmUploadType
  = lens _plwmUploadType
      (\ s a -> s{_plwmUploadType = a})

-- | Multipart request metadata.
plwmPayload :: Lens' ProjectsLocationsWorkerMessages SendWorkerMessagesRequest
plwmPayload
  = lens _plwmPayload (\ s a -> s{_plwmPayload = a})

-- | OAuth bearer token.
plwmBearerToken :: Lens' ProjectsLocationsWorkerMessages (Maybe Text)
plwmBearerToken
  = lens _plwmBearerToken
      (\ s a -> s{_plwmBearerToken = a})

-- | The project to send the WorkerMessages to.
plwmProjectId :: Lens' ProjectsLocationsWorkerMessages Text
plwmProjectId
  = lens _plwmProjectId
      (\ s a -> s{_plwmProjectId = a})

-- | Selector specifying which fields to include in a partial response.
plwmFields :: Lens' ProjectsLocationsWorkerMessages (Maybe Text)
plwmFields
  = lens _plwmFields (\ s a -> s{_plwmFields = a})

-- | JSONP
plwmCallback :: Lens' ProjectsLocationsWorkerMessages (Maybe Text)
plwmCallback
  = lens _plwmCallback (\ s a -> s{_plwmCallback = a})

instance GoogleRequest
         ProjectsLocationsWorkerMessages where
        type Rs ProjectsLocationsWorkerMessages =
             SendWorkerMessagesResponse
        type Scopes ProjectsLocationsWorkerMessages =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly",
               "https://www.googleapis.com/auth/userinfo.email"]
        requestClient ProjectsLocationsWorkerMessages'{..}
          = go _plwmProjectId _plwmLocation _plwmXgafv
              _plwmUploadProtocol
              (Just _plwmPp)
              _plwmAccessToken
              _plwmUploadType
              _plwmBearerToken
              _plwmCallback
              _plwmFields
              (Just AltJSON)
              _plwmPayload
              dataflowService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsLocationsWorkerMessagesResource)
                      mempty
