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
-- Module      : Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Cancel
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a job cancellation request. To access the job resource after
-- cancellation, call regions\/{region}\/jobs.list or
-- regions\/{region}\/jobs.get.
--
-- /See:/ <https://cloud.google.com/dataproc/ Google Cloud Dataproc API Reference> for @dataproc.projects.regions.jobs.cancel@.
module Network.Google.Resource.Dataproc.Projects.Regions.Jobs.Cancel
    (
    -- * REST Resource
      ProjectsRegionsJobsCancelResource

    -- * Creating a Request
    , projectsRegionsJobsCancel
    , ProjectsRegionsJobsCancel

    -- * Request Lenses
    , prjcXgafv
    , prjcJobId
    , prjcUploadProtocol
    , prjcPp
    , prjcAccessToken
    , prjcUploadType
    , prjcPayload
    , prjcBearerToken
    , prjcRegion
    , prjcProjectId
    , prjcFields
    , prjcCallback
    ) where

import Network.Google.Dataproc.Types
import Network.Google.Prelude

-- | A resource alias for @dataproc.projects.regions.jobs.cancel@ method which the
-- 'ProjectsRegionsJobsCancel' request conforms to.
type ProjectsRegionsJobsCancelResource =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "regions" :>
             Capture "region" Text :>
               "jobs" :>
                 CaptureMode "jobId" "cancel" Text :>
                   QueryParam "$.xgafv" Xgafv :>
                     QueryParam "upload_protocol" Text :>
                       QueryParam "pp" Bool :>
                         QueryParam "access_token" Text :>
                           QueryParam "uploadType" Text :>
                             QueryParam "bearer_token" Text :>
                               QueryParam "callback" Text :>
                                 QueryParam "fields" Text :>
                                   QueryParam "alt" AltJSON :>
                                     ReqBody '[JSON] CancelJobRequest :>
                                       Post '[JSON] Job

-- | Starts a job cancellation request. To access the job resource after
-- cancellation, call regions\/{region}\/jobs.list or
-- regions\/{region}\/jobs.get.
--
-- /See:/ 'projectsRegionsJobsCancel' smart constructor.
data ProjectsRegionsJobsCancel = ProjectsRegionsJobsCancel'
    { _prjcXgafv :: !(Maybe Xgafv)
    , _prjcJobId :: !Text
    , _prjcUploadProtocol :: !(Maybe Text)
    , _prjcPp :: !Bool
    , _prjcAccessToken :: !(Maybe Text)
    , _prjcUploadType :: !(Maybe Text)
    , _prjcPayload :: !CancelJobRequest
    , _prjcBearerToken :: !(Maybe Text)
    , _prjcRegion :: !Text
    , _prjcProjectId :: !Text
    , _prjcFields :: !(Maybe Text)
    , _prjcCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsRegionsJobsCancel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prjcXgafv'
--
-- * 'prjcJobId'
--
-- * 'prjcUploadProtocol'
--
-- * 'prjcPp'
--
-- * 'prjcAccessToken'
--
-- * 'prjcUploadType'
--
-- * 'prjcPayload'
--
-- * 'prjcBearerToken'
--
-- * 'prjcRegion'
--
-- * 'prjcProjectId'
--
-- * 'prjcFields'
--
-- * 'prjcCallback'
projectsRegionsJobsCancel
    :: Text -- ^ 'prjcJobId'
    -> CancelJobRequest -- ^ 'prjcPayload'
    -> Text -- ^ 'prjcRegion'
    -> Text -- ^ 'prjcProjectId'
    -> ProjectsRegionsJobsCancel
projectsRegionsJobsCancel pPrjcJobId_ pPrjcPayload_ pPrjcRegion_ pPrjcProjectId_ = 
    ProjectsRegionsJobsCancel'
    { _prjcXgafv = Nothing
    , _prjcJobId = pPrjcJobId_
    , _prjcUploadProtocol = Nothing
    , _prjcPp = True
    , _prjcAccessToken = Nothing
    , _prjcUploadType = Nothing
    , _prjcPayload = pPrjcPayload_
    , _prjcBearerToken = Nothing
    , _prjcRegion = pPrjcRegion_
    , _prjcProjectId = pPrjcProjectId_
    , _prjcFields = Nothing
    , _prjcCallback = Nothing
    }

-- | V1 error format.
prjcXgafv :: Lens' ProjectsRegionsJobsCancel (Maybe Xgafv)
prjcXgafv
  = lens _prjcXgafv (\ s a -> s{_prjcXgafv = a})

-- | Required. The job ID.
prjcJobId :: Lens' ProjectsRegionsJobsCancel Text
prjcJobId
  = lens _prjcJobId (\ s a -> s{_prjcJobId = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
prjcUploadProtocol :: Lens' ProjectsRegionsJobsCancel (Maybe Text)
prjcUploadProtocol
  = lens _prjcUploadProtocol
      (\ s a -> s{_prjcUploadProtocol = a})

-- | Pretty-print response.
prjcPp :: Lens' ProjectsRegionsJobsCancel Bool
prjcPp = lens _prjcPp (\ s a -> s{_prjcPp = a})

-- | OAuth access token.
prjcAccessToken :: Lens' ProjectsRegionsJobsCancel (Maybe Text)
prjcAccessToken
  = lens _prjcAccessToken
      (\ s a -> s{_prjcAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
prjcUploadType :: Lens' ProjectsRegionsJobsCancel (Maybe Text)
prjcUploadType
  = lens _prjcUploadType
      (\ s a -> s{_prjcUploadType = a})

-- | Multipart request metadata.
prjcPayload :: Lens' ProjectsRegionsJobsCancel CancelJobRequest
prjcPayload
  = lens _prjcPayload (\ s a -> s{_prjcPayload = a})

-- | OAuth bearer token.
prjcBearerToken :: Lens' ProjectsRegionsJobsCancel (Maybe Text)
prjcBearerToken
  = lens _prjcBearerToken
      (\ s a -> s{_prjcBearerToken = a})

-- | Required. The Cloud Dataproc region in which to handle the request.
prjcRegion :: Lens' ProjectsRegionsJobsCancel Text
prjcRegion
  = lens _prjcRegion (\ s a -> s{_prjcRegion = a})

-- | Required. The ID of the Google Cloud Platform project that the job
-- belongs to.
prjcProjectId :: Lens' ProjectsRegionsJobsCancel Text
prjcProjectId
  = lens _prjcProjectId
      (\ s a -> s{_prjcProjectId = a})

-- | Selector specifying which fields to include in a partial response.
prjcFields :: Lens' ProjectsRegionsJobsCancel (Maybe Text)
prjcFields
  = lens _prjcFields (\ s a -> s{_prjcFields = a})

-- | JSONP
prjcCallback :: Lens' ProjectsRegionsJobsCancel (Maybe Text)
prjcCallback
  = lens _prjcCallback (\ s a -> s{_prjcCallback = a})

instance GoogleRequest ProjectsRegionsJobsCancel
         where
        type Rs ProjectsRegionsJobsCancel = Job
        type Scopes ProjectsRegionsJobsCancel =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsRegionsJobsCancel'{..}
          = go _prjcProjectId _prjcRegion _prjcJobId _prjcXgafv
              _prjcUploadProtocol
              (Just _prjcPp)
              _prjcAccessToken
              _prjcUploadType
              _prjcBearerToken
              _prjcCallback
              _prjcFields
              (Just AltJSON)
              _prjcPayload
              dataprocService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsRegionsJobsCancelResource)
                      mempty
