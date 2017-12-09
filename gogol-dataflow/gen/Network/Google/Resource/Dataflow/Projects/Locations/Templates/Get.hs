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
-- Module      : Network.Google.Resource.Dataflow.Projects.Locations.Templates.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the template associated with a template.
--
-- /See:/ <https://cloud.google.com/dataflow Google Dataflow API Reference> for @dataflow.projects.locations.templates.get@.
module Network.Google.Resource.Dataflow.Projects.Locations.Templates.Get
    (
    -- * REST Resource
      ProjectsLocationsTemplatesGetResource

    -- * Creating a Request
    , projectsLocationsTemplatesGet
    , ProjectsLocationsTemplatesGet

    -- * Request Lenses
    , pltgXgafv
    , pltgUploadProtocol
    , pltgLocation
    , pltgPp
    , pltgAccessToken
    , pltgUploadType
    , pltgBearerToken
    , pltgGcsPath
    , pltgView
    , pltgProjectId
    , pltgFields
    , pltgCallback
    ) where

import Network.Google.Dataflow.Types
import Network.Google.Prelude

-- | A resource alias for @dataflow.projects.locations.templates.get@ method which the
-- 'ProjectsLocationsTemplatesGet' request conforms to.
type ProjectsLocationsTemplatesGetResource =
     "v1b3" :>
       "projects" :>
         Capture "projectId" Text :>
           "locations" :>
             Capture "location" Text :>
               "templates:get" :>
                 QueryParam "$.xgafv" Xgafv :>
                   QueryParam "upload_protocol" Text :>
                     QueryParam "pp" Bool :>
                       QueryParam "access_token" Text :>
                         QueryParam "uploadType" Text :>
                           QueryParam "bearer_token" Text :>
                             QueryParam "gcsPath" Text :>
                               QueryParam "view" Text :>
                                 QueryParam "callback" Text :>
                                   QueryParam "fields" Text :>
                                     QueryParam "alt" AltJSON :>
                                       Get '[JSON] GetTemplateResponse

-- | Get the template associated with a template.
--
-- /See:/ 'projectsLocationsTemplatesGet' smart constructor.
data ProjectsLocationsTemplatesGet = ProjectsLocationsTemplatesGet'
    { _pltgXgafv :: !(Maybe Xgafv)
    , _pltgUploadProtocol :: !(Maybe Text)
    , _pltgLocation :: !Text
    , _pltgPp :: !Bool
    , _pltgAccessToken :: !(Maybe Text)
    , _pltgUploadType :: !(Maybe Text)
    , _pltgBearerToken :: !(Maybe Text)
    , _pltgGcsPath :: !(Maybe Text)
    , _pltgView :: !(Maybe Text)
    , _pltgProjectId :: !Text
    , _pltgFields :: !(Maybe Text)
    , _pltgCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsLocationsTemplatesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pltgXgafv'
--
-- * 'pltgUploadProtocol'
--
-- * 'pltgLocation'
--
-- * 'pltgPp'
--
-- * 'pltgAccessToken'
--
-- * 'pltgUploadType'
--
-- * 'pltgBearerToken'
--
-- * 'pltgGcsPath'
--
-- * 'pltgView'
--
-- * 'pltgProjectId'
--
-- * 'pltgFields'
--
-- * 'pltgCallback'
projectsLocationsTemplatesGet
    :: Text -- ^ 'pltgLocation'
    -> Text -- ^ 'pltgProjectId'
    -> ProjectsLocationsTemplatesGet
projectsLocationsTemplatesGet pPltgLocation_ pPltgProjectId_ = 
    ProjectsLocationsTemplatesGet'
    { _pltgXgafv = Nothing
    , _pltgUploadProtocol = Nothing
    , _pltgLocation = pPltgLocation_
    , _pltgPp = True
    , _pltgAccessToken = Nothing
    , _pltgUploadType = Nothing
    , _pltgBearerToken = Nothing
    , _pltgGcsPath = Nothing
    , _pltgView = Nothing
    , _pltgProjectId = pPltgProjectId_
    , _pltgFields = Nothing
    , _pltgCallback = Nothing
    }

-- | V1 error format.
pltgXgafv :: Lens' ProjectsLocationsTemplatesGet (Maybe Xgafv)
pltgXgafv
  = lens _pltgXgafv (\ s a -> s{_pltgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pltgUploadProtocol :: Lens' ProjectsLocationsTemplatesGet (Maybe Text)
pltgUploadProtocol
  = lens _pltgUploadProtocol
      (\ s a -> s{_pltgUploadProtocol = a})

-- | The location to which to direct the request.
pltgLocation :: Lens' ProjectsLocationsTemplatesGet Text
pltgLocation
  = lens _pltgLocation (\ s a -> s{_pltgLocation = a})

-- | Pretty-print response.
pltgPp :: Lens' ProjectsLocationsTemplatesGet Bool
pltgPp = lens _pltgPp (\ s a -> s{_pltgPp = a})

-- | OAuth access token.
pltgAccessToken :: Lens' ProjectsLocationsTemplatesGet (Maybe Text)
pltgAccessToken
  = lens _pltgAccessToken
      (\ s a -> s{_pltgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pltgUploadType :: Lens' ProjectsLocationsTemplatesGet (Maybe Text)
pltgUploadType
  = lens _pltgUploadType
      (\ s a -> s{_pltgUploadType = a})

-- | OAuth bearer token.
pltgBearerToken :: Lens' ProjectsLocationsTemplatesGet (Maybe Text)
pltgBearerToken
  = lens _pltgBearerToken
      (\ s a -> s{_pltgBearerToken = a})

-- | Required. A Cloud Storage path to the template from which to create the
-- job. Must be a valid Cloud Storage URL, beginning with \`gs:\/\/\`.
pltgGcsPath :: Lens' ProjectsLocationsTemplatesGet (Maybe Text)
pltgGcsPath
  = lens _pltgGcsPath (\ s a -> s{_pltgGcsPath = a})

-- | The view to retrieve. Defaults to METADATA_ONLY.
pltgView :: Lens' ProjectsLocationsTemplatesGet (Maybe Text)
pltgView = lens _pltgView (\ s a -> s{_pltgView = a})

-- | Required. The ID of the Cloud Platform project that the job belongs to.
pltgProjectId :: Lens' ProjectsLocationsTemplatesGet Text
pltgProjectId
  = lens _pltgProjectId
      (\ s a -> s{_pltgProjectId = a})

-- | Selector specifying which fields to include in a partial response.
pltgFields :: Lens' ProjectsLocationsTemplatesGet (Maybe Text)
pltgFields
  = lens _pltgFields (\ s a -> s{_pltgFields = a})

-- | JSONP
pltgCallback :: Lens' ProjectsLocationsTemplatesGet (Maybe Text)
pltgCallback
  = lens _pltgCallback (\ s a -> s{_pltgCallback = a})

instance GoogleRequest ProjectsLocationsTemplatesGet
         where
        type Rs ProjectsLocationsTemplatesGet =
             GetTemplateResponse
        type Scopes ProjectsLocationsTemplatesGet =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly",
               "https://www.googleapis.com/auth/userinfo.email"]
        requestClient ProjectsLocationsTemplatesGet'{..}
          = go _pltgProjectId _pltgLocation _pltgXgafv
              _pltgUploadProtocol
              (Just _pltgPp)
              _pltgAccessToken
              _pltgUploadType
              _pltgBearerToken
              _pltgGcsPath
              _pltgView
              _pltgCallback
              _pltgFields
              (Just AltJSON)
              dataflowService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsLocationsTemplatesGetResource)
                      mempty
