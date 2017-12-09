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
-- Module      : Network.Google.Resource.Ml.Projects.Locations.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the complete list of CMLE capabilities in a location, along with
-- their location-specific properties.
--
-- /See:/ <https://cloud.google.com/ml/ Google Cloud Machine Learning Engine Reference> for @ml.projects.locations.get@.
module Network.Google.Resource.Ml.Projects.Locations.Get
    (
    -- * REST Resource
      ProjectsLocationsGetResource

    -- * Creating a Request
    , projectsLocationsGet
    , ProjectsLocationsGet

    -- * Request Lenses
    , plgXgafv
    , plgUploadProtocol
    , plgPp
    , plgAccessToken
    , plgUploadType
    , plgBearerToken
    , plgName
    , plgFields
    , plgCallback
    ) where

import Network.Google.MachineLearning.Types
import Network.Google.Prelude

-- | A resource alias for @ml.projects.locations.get@ method which the
-- 'ProjectsLocationsGet' request conforms to.
type ProjectsLocationsGetResource =
     "v1" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON] GoogleCloudMlV1__Location

-- | Get the complete list of CMLE capabilities in a location, along with
-- their location-specific properties.
--
-- /See:/ 'projectsLocationsGet' smart constructor.
data ProjectsLocationsGet = ProjectsLocationsGet'
    { _plgXgafv :: !(Maybe Xgafv)
    , _plgUploadProtocol :: !(Maybe Text)
    , _plgPp :: !Bool
    , _plgAccessToken :: !(Maybe Text)
    , _plgUploadType :: !(Maybe Text)
    , _plgBearerToken :: !(Maybe Text)
    , _plgName :: !Text
    , _plgFields :: !(Maybe Text)
    , _plgCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsLocationsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plgXgafv'
--
-- * 'plgUploadProtocol'
--
-- * 'plgPp'
--
-- * 'plgAccessToken'
--
-- * 'plgUploadType'
--
-- * 'plgBearerToken'
--
-- * 'plgName'
--
-- * 'plgFields'
--
-- * 'plgCallback'
projectsLocationsGet
    :: Text -- ^ 'plgName'
    -> ProjectsLocationsGet
projectsLocationsGet pPlgName_ = 
    ProjectsLocationsGet'
    { _plgXgafv = Nothing
    , _plgUploadProtocol = Nothing
    , _plgPp = True
    , _plgAccessToken = Nothing
    , _plgUploadType = Nothing
    , _plgBearerToken = Nothing
    , _plgName = pPlgName_
    , _plgFields = Nothing
    , _plgCallback = Nothing
    }

-- | V1 error format.
plgXgafv :: Lens' ProjectsLocationsGet (Maybe Xgafv)
plgXgafv = lens _plgXgafv (\ s a -> s{_plgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
plgUploadProtocol :: Lens' ProjectsLocationsGet (Maybe Text)
plgUploadProtocol
  = lens _plgUploadProtocol
      (\ s a -> s{_plgUploadProtocol = a})

-- | Pretty-print response.
plgPp :: Lens' ProjectsLocationsGet Bool
plgPp = lens _plgPp (\ s a -> s{_plgPp = a})

-- | OAuth access token.
plgAccessToken :: Lens' ProjectsLocationsGet (Maybe Text)
plgAccessToken
  = lens _plgAccessToken
      (\ s a -> s{_plgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
plgUploadType :: Lens' ProjectsLocationsGet (Maybe Text)
plgUploadType
  = lens _plgUploadType
      (\ s a -> s{_plgUploadType = a})

-- | OAuth bearer token.
plgBearerToken :: Lens' ProjectsLocationsGet (Maybe Text)
plgBearerToken
  = lens _plgBearerToken
      (\ s a -> s{_plgBearerToken = a})

-- | Required. The name of the location.
plgName :: Lens' ProjectsLocationsGet Text
plgName = lens _plgName (\ s a -> s{_plgName = a})

-- | Selector specifying which fields to include in a partial response.
plgFields :: Lens' ProjectsLocationsGet (Maybe Text)
plgFields
  = lens _plgFields (\ s a -> s{_plgFields = a})

-- | JSONP
plgCallback :: Lens' ProjectsLocationsGet (Maybe Text)
plgCallback
  = lens _plgCallback (\ s a -> s{_plgCallback = a})

instance GoogleRequest ProjectsLocationsGet where
        type Rs ProjectsLocationsGet =
             GoogleCloudMlV1__Location
        type Scopes ProjectsLocationsGet =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsLocationsGet'{..}
          = go _plgName _plgXgafv _plgUploadProtocol
              (Just _plgPp)
              _plgAccessToken
              _plgUploadType
              _plgBearerToken
              _plgCallback
              _plgFields
              (Just AltJSON)
              machineLearningService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsLocationsGetResource)
                      mempty
