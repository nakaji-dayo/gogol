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
-- Module      : Network.Google.Resource.Dataflow.Projects.Locations.Templates.Launch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launch a template.
--
-- /See:/ <https://cloud.google.com/dataflow Google Dataflow API Reference> for @dataflow.projects.locations.templates.launch@.
module Network.Google.Resource.Dataflow.Projects.Locations.Templates.Launch
    (
    -- * REST Resource
      ProjectsLocationsTemplatesLaunchResource

    -- * Creating a Request
    , projectsLocationsTemplatesLaunch
    , ProjectsLocationsTemplatesLaunch

    -- * Request Lenses
    , pltlXgafv
    , pltlValidateOnly
    , pltlUploadProtocol
    , pltlLocation
    , pltlPp
    , pltlAccessToken
    , pltlUploadType
    , pltlPayload
    , pltlBearerToken
    , pltlGcsPath
    , pltlProjectId
    , pltlFields
    , pltlCallback
    ) where

import Network.Google.Dataflow.Types
import Network.Google.Prelude

-- | A resource alias for @dataflow.projects.locations.templates.launch@ method which the
-- 'ProjectsLocationsTemplatesLaunch' request conforms to.
type ProjectsLocationsTemplatesLaunchResource =
     "v1b3" :>
       "projects" :>
         Capture "projectId" Text :>
           "locations" :>
             Capture "location" Text :>
               "templates:launch" :>
                 QueryParam "$.xgafv" Xgafv :>
                   QueryParam "validateOnly" Bool :>
                     QueryParam "upload_protocol" Text :>
                       QueryParam "pp" Bool :>
                         QueryParam "access_token" Text :>
                           QueryParam "uploadType" Text :>
                             QueryParam "bearer_token" Text :>
                               QueryParam "gcsPath" Text :>
                                 QueryParam "callback" Text :>
                                   QueryParam "fields" Text :>
                                     QueryParam "alt" AltJSON :>
                                       ReqBody '[JSON] LaunchTemplateParameters
                                         :> Post '[JSON] LaunchTemplateResponse

-- | Launch a template.
--
-- /See:/ 'projectsLocationsTemplatesLaunch' smart constructor.
data ProjectsLocationsTemplatesLaunch = ProjectsLocationsTemplatesLaunch'
    { _pltlXgafv :: !(Maybe Xgafv)
    , _pltlValidateOnly :: !(Maybe Bool)
    , _pltlUploadProtocol :: !(Maybe Text)
    , _pltlLocation :: !Text
    , _pltlPp :: !Bool
    , _pltlAccessToken :: !(Maybe Text)
    , _pltlUploadType :: !(Maybe Text)
    , _pltlPayload :: !LaunchTemplateParameters
    , _pltlBearerToken :: !(Maybe Text)
    , _pltlGcsPath :: !(Maybe Text)
    , _pltlProjectId :: !Text
    , _pltlFields :: !(Maybe Text)
    , _pltlCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsLocationsTemplatesLaunch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pltlXgafv'
--
-- * 'pltlValidateOnly'
--
-- * 'pltlUploadProtocol'
--
-- * 'pltlLocation'
--
-- * 'pltlPp'
--
-- * 'pltlAccessToken'
--
-- * 'pltlUploadType'
--
-- * 'pltlPayload'
--
-- * 'pltlBearerToken'
--
-- * 'pltlGcsPath'
--
-- * 'pltlProjectId'
--
-- * 'pltlFields'
--
-- * 'pltlCallback'
projectsLocationsTemplatesLaunch
    :: Text -- ^ 'pltlLocation'
    -> LaunchTemplateParameters -- ^ 'pltlPayload'
    -> Text -- ^ 'pltlProjectId'
    -> ProjectsLocationsTemplatesLaunch
projectsLocationsTemplatesLaunch pPltlLocation_ pPltlPayload_ pPltlProjectId_ = 
    ProjectsLocationsTemplatesLaunch'
    { _pltlXgafv = Nothing
    , _pltlValidateOnly = Nothing
    , _pltlUploadProtocol = Nothing
    , _pltlLocation = pPltlLocation_
    , _pltlPp = True
    , _pltlAccessToken = Nothing
    , _pltlUploadType = Nothing
    , _pltlPayload = pPltlPayload_
    , _pltlBearerToken = Nothing
    , _pltlGcsPath = Nothing
    , _pltlProjectId = pPltlProjectId_
    , _pltlFields = Nothing
    , _pltlCallback = Nothing
    }

-- | V1 error format.
pltlXgafv :: Lens' ProjectsLocationsTemplatesLaunch (Maybe Xgafv)
pltlXgafv
  = lens _pltlXgafv (\ s a -> s{_pltlXgafv = a})

-- | If true, the request is validated but not actually executed. Defaults to
-- false.
pltlValidateOnly :: Lens' ProjectsLocationsTemplatesLaunch (Maybe Bool)
pltlValidateOnly
  = lens _pltlValidateOnly
      (\ s a -> s{_pltlValidateOnly = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pltlUploadProtocol :: Lens' ProjectsLocationsTemplatesLaunch (Maybe Text)
pltlUploadProtocol
  = lens _pltlUploadProtocol
      (\ s a -> s{_pltlUploadProtocol = a})

-- | The location to which to direct the request.
pltlLocation :: Lens' ProjectsLocationsTemplatesLaunch Text
pltlLocation
  = lens _pltlLocation (\ s a -> s{_pltlLocation = a})

-- | Pretty-print response.
pltlPp :: Lens' ProjectsLocationsTemplatesLaunch Bool
pltlPp = lens _pltlPp (\ s a -> s{_pltlPp = a})

-- | OAuth access token.
pltlAccessToken :: Lens' ProjectsLocationsTemplatesLaunch (Maybe Text)
pltlAccessToken
  = lens _pltlAccessToken
      (\ s a -> s{_pltlAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pltlUploadType :: Lens' ProjectsLocationsTemplatesLaunch (Maybe Text)
pltlUploadType
  = lens _pltlUploadType
      (\ s a -> s{_pltlUploadType = a})

-- | Multipart request metadata.
pltlPayload :: Lens' ProjectsLocationsTemplatesLaunch LaunchTemplateParameters
pltlPayload
  = lens _pltlPayload (\ s a -> s{_pltlPayload = a})

-- | OAuth bearer token.
pltlBearerToken :: Lens' ProjectsLocationsTemplatesLaunch (Maybe Text)
pltlBearerToken
  = lens _pltlBearerToken
      (\ s a -> s{_pltlBearerToken = a})

-- | Required. A Cloud Storage path to the template from which to create the
-- job. Must be valid Cloud Storage URL, beginning with \'gs:\/\/\'.
pltlGcsPath :: Lens' ProjectsLocationsTemplatesLaunch (Maybe Text)
pltlGcsPath
  = lens _pltlGcsPath (\ s a -> s{_pltlGcsPath = a})

-- | Required. The ID of the Cloud Platform project that the job belongs to.
pltlProjectId :: Lens' ProjectsLocationsTemplatesLaunch Text
pltlProjectId
  = lens _pltlProjectId
      (\ s a -> s{_pltlProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pltlFields :: Lens' ProjectsLocationsTemplatesLaunch (Maybe Text)
pltlFields
  = lens _pltlFields (\ s a -> s{_pltlFields = a})

-- | JSONP
pltlCallback :: Lens' ProjectsLocationsTemplatesLaunch (Maybe Text)
pltlCallback
  = lens _pltlCallback (\ s a -> s{_pltlCallback = a})

instance GoogleRequest
         ProjectsLocationsTemplatesLaunch where
        type Rs ProjectsLocationsTemplatesLaunch =
             LaunchTemplateResponse
        type Scopes ProjectsLocationsTemplatesLaunch =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly",
               "https://www.googleapis.com/auth/userinfo.email"]
        requestClient ProjectsLocationsTemplatesLaunch'{..}
          = go _pltlProjectId _pltlLocation _pltlXgafv
              _pltlValidateOnly
              _pltlUploadProtocol
              (Just _pltlPp)
              _pltlAccessToken
              _pltlUploadType
              _pltlBearerToken
              _pltlGcsPath
              _pltlCallback
              _pltlFields
              (Just AltJSON)
              _pltlPayload
              dataflowService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsLocationsTemplatesLaunchResource)
                      mempty
