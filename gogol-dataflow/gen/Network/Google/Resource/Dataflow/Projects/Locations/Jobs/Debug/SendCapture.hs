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
-- Module      : Network.Google.Resource.Dataflow.Projects.Locations.Jobs.Debug.SendCapture
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Send encoded debug capture data for component.
--
-- /See:/ <https://cloud.google.com/dataflow Google Dataflow API Reference> for @dataflow.projects.locations.jobs.debug.sendCapture@.
module Network.Google.Resource.Dataflow.Projects.Locations.Jobs.Debug.SendCapture
    (
    -- * REST Resource
      ProjectsLocationsJobsDebugSendCaptureResource

    -- * Creating a Request
    , projectsLocationsJobsDebugSendCapture
    , ProjectsLocationsJobsDebugSendCapture

    -- * Request Lenses
    , pljdscXgafv
    , pljdscJobId
    , pljdscUploadProtocol
    , pljdscLocation
    , pljdscPp
    , pljdscAccessToken
    , pljdscUploadType
    , pljdscPayload
    , pljdscBearerToken
    , pljdscProjectId
    , pljdscFields
    , pljdscCallback
    ) where

import Network.Google.Dataflow.Types
import Network.Google.Prelude

-- | A resource alias for @dataflow.projects.locations.jobs.debug.sendCapture@ method which the
-- 'ProjectsLocationsJobsDebugSendCapture' request conforms to.
type ProjectsLocationsJobsDebugSendCaptureResource =
     "v1b3" :>
       "projects" :>
         Capture "projectId" Text :>
           "locations" :>
             Capture "location" Text :>
               "jobs" :>
                 Capture "jobId" Text :>
                   "debug" :>
                     "sendCapture" :>
                       QueryParam "$.xgafv" Xgafv :>
                         QueryParam "upload_protocol" Text :>
                           QueryParam "pp" Bool :>
                             QueryParam "access_token" Text :>
                               QueryParam "uploadType" Text :>
                                 QueryParam "bearer_token" Text :>
                                   QueryParam "callback" Text :>
                                     QueryParam "fields" Text :>
                                       QueryParam "alt" AltJSON :>
                                         ReqBody '[JSON] SendDebugCaptureRequest
                                           :>
                                           Post '[JSON] SendDebugCaptureResponse

-- | Send encoded debug capture data for component.
--
-- /See:/ 'projectsLocationsJobsDebugSendCapture' smart constructor.
data ProjectsLocationsJobsDebugSendCapture = ProjectsLocationsJobsDebugSendCapture'
    { _pljdscXgafv :: !(Maybe Xgafv)
    , _pljdscJobId :: !Text
    , _pljdscUploadProtocol :: !(Maybe Text)
    , _pljdscLocation :: !Text
    , _pljdscPp :: !Bool
    , _pljdscAccessToken :: !(Maybe Text)
    , _pljdscUploadType :: !(Maybe Text)
    , _pljdscPayload :: !SendDebugCaptureRequest
    , _pljdscBearerToken :: !(Maybe Text)
    , _pljdscProjectId :: !Text
    , _pljdscFields :: !(Maybe Text)
    , _pljdscCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsLocationsJobsDebugSendCapture' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pljdscXgafv'
--
-- * 'pljdscJobId'
--
-- * 'pljdscUploadProtocol'
--
-- * 'pljdscLocation'
--
-- * 'pljdscPp'
--
-- * 'pljdscAccessToken'
--
-- * 'pljdscUploadType'
--
-- * 'pljdscPayload'
--
-- * 'pljdscBearerToken'
--
-- * 'pljdscProjectId'
--
-- * 'pljdscFields'
--
-- * 'pljdscCallback'
projectsLocationsJobsDebugSendCapture
    :: Text -- ^ 'pljdscJobId'
    -> Text -- ^ 'pljdscLocation'
    -> SendDebugCaptureRequest -- ^ 'pljdscPayload'
    -> Text -- ^ 'pljdscProjectId'
    -> ProjectsLocationsJobsDebugSendCapture
projectsLocationsJobsDebugSendCapture pPljdscJobId_ pPljdscLocation_ pPljdscPayload_ pPljdscProjectId_ = 
    ProjectsLocationsJobsDebugSendCapture'
    { _pljdscXgafv = Nothing
    , _pljdscJobId = pPljdscJobId_
    , _pljdscUploadProtocol = Nothing
    , _pljdscLocation = pPljdscLocation_
    , _pljdscPp = True
    , _pljdscAccessToken = Nothing
    , _pljdscUploadType = Nothing
    , _pljdscPayload = pPljdscPayload_
    , _pljdscBearerToken = Nothing
    , _pljdscProjectId = pPljdscProjectId_
    , _pljdscFields = Nothing
    , _pljdscCallback = Nothing
    }

-- | V1 error format.
pljdscXgafv :: Lens' ProjectsLocationsJobsDebugSendCapture (Maybe Xgafv)
pljdscXgafv
  = lens _pljdscXgafv (\ s a -> s{_pljdscXgafv = a})

-- | The job id.
pljdscJobId :: Lens' ProjectsLocationsJobsDebugSendCapture Text
pljdscJobId
  = lens _pljdscJobId (\ s a -> s{_pljdscJobId = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pljdscUploadProtocol :: Lens' ProjectsLocationsJobsDebugSendCapture (Maybe Text)
pljdscUploadProtocol
  = lens _pljdscUploadProtocol
      (\ s a -> s{_pljdscUploadProtocol = a})

-- | The location which contains the job specified by job_id.
pljdscLocation :: Lens' ProjectsLocationsJobsDebugSendCapture Text
pljdscLocation
  = lens _pljdscLocation
      (\ s a -> s{_pljdscLocation = a})

-- | Pretty-print response.
pljdscPp :: Lens' ProjectsLocationsJobsDebugSendCapture Bool
pljdscPp = lens _pljdscPp (\ s a -> s{_pljdscPp = a})

-- | OAuth access token.
pljdscAccessToken :: Lens' ProjectsLocationsJobsDebugSendCapture (Maybe Text)
pljdscAccessToken
  = lens _pljdscAccessToken
      (\ s a -> s{_pljdscAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pljdscUploadType :: Lens' ProjectsLocationsJobsDebugSendCapture (Maybe Text)
pljdscUploadType
  = lens _pljdscUploadType
      (\ s a -> s{_pljdscUploadType = a})

-- | Multipart request metadata.
pljdscPayload :: Lens' ProjectsLocationsJobsDebugSendCapture SendDebugCaptureRequest
pljdscPayload
  = lens _pljdscPayload
      (\ s a -> s{_pljdscPayload = a})

-- | OAuth bearer token.
pljdscBearerToken :: Lens' ProjectsLocationsJobsDebugSendCapture (Maybe Text)
pljdscBearerToken
  = lens _pljdscBearerToken
      (\ s a -> s{_pljdscBearerToken = a})

-- | The project id.
pljdscProjectId :: Lens' ProjectsLocationsJobsDebugSendCapture Text
pljdscProjectId
  = lens _pljdscProjectId
      (\ s a -> s{_pljdscProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pljdscFields :: Lens' ProjectsLocationsJobsDebugSendCapture (Maybe Text)
pljdscFields
  = lens _pljdscFields (\ s a -> s{_pljdscFields = a})

-- | JSONP
pljdscCallback :: Lens' ProjectsLocationsJobsDebugSendCapture (Maybe Text)
pljdscCallback
  = lens _pljdscCallback
      (\ s a -> s{_pljdscCallback = a})

instance GoogleRequest
         ProjectsLocationsJobsDebugSendCapture where
        type Rs ProjectsLocationsJobsDebugSendCapture =
             SendDebugCaptureResponse
        type Scopes ProjectsLocationsJobsDebugSendCapture =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly",
               "https://www.googleapis.com/auth/userinfo.email"]
        requestClient
          ProjectsLocationsJobsDebugSendCapture'{..}
          = go _pljdscProjectId _pljdscLocation _pljdscJobId
              _pljdscXgafv
              _pljdscUploadProtocol
              (Just _pljdscPp)
              _pljdscAccessToken
              _pljdscUploadType
              _pljdscBearerToken
              _pljdscCallback
              _pljdscFields
              (Just AltJSON)
              _pljdscPayload
              dataflowService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsLocationsJobsDebugSendCaptureResource)
                      mempty
