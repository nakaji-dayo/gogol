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
-- Module      : Network.Google.Resource.Ml.Projects.Locations.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all locations that provides at least one type of CMLE capability.
--
-- /See:/ <https://cloud.google.com/ml/ Google Cloud Machine Learning Engine Reference> for @ml.projects.locations.list@.
module Network.Google.Resource.Ml.Projects.Locations.List
    (
    -- * REST Resource
      ProjectsLocationsListResource

    -- * Creating a Request
    , projectsLocationsList
    , ProjectsLocationsList

    -- * Request Lenses
    , pllParent
    , pllXgafv
    , pllUploadProtocol
    , pllPp
    , pllAccessToken
    , pllUploadType
    , pllBearerToken
    , pllPageToken
    , pllPageSize
    , pllFields
    , pllCallback
    ) where

import Network.Google.MachineLearning.Types
import Network.Google.Prelude

-- | A resource alias for @ml.projects.locations.list@ method which the
-- 'ProjectsLocationsList' request conforms to.
type ProjectsLocationsListResource =
     "v1" :>
       Capture "parent" Text :>
         "locations" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "pageSize" (Textual Int32) :>
                           QueryParam "callback" Text :>
                             QueryParam "fields" Text :>
                               QueryParam "alt" AltJSON :>
                                 Get '[JSON]
                                   GoogleCloudMlV1__ListLocationsResponse

-- | List all locations that provides at least one type of CMLE capability.
--
-- /See:/ 'projectsLocationsList' smart constructor.
data ProjectsLocationsList = ProjectsLocationsList'
    { _pllParent :: !Text
    , _pllXgafv :: !(Maybe Xgafv)
    , _pllUploadProtocol :: !(Maybe Text)
    , _pllPp :: !Bool
    , _pllAccessToken :: !(Maybe Text)
    , _pllUploadType :: !(Maybe Text)
    , _pllBearerToken :: !(Maybe Text)
    , _pllPageToken :: !(Maybe Text)
    , _pllPageSize :: !(Maybe (Textual Int32))
    , _pllFields :: !(Maybe Text)
    , _pllCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsLocationsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pllParent'
--
-- * 'pllXgafv'
--
-- * 'pllUploadProtocol'
--
-- * 'pllPp'
--
-- * 'pllAccessToken'
--
-- * 'pllUploadType'
--
-- * 'pllBearerToken'
--
-- * 'pllPageToken'
--
-- * 'pllPageSize'
--
-- * 'pllFields'
--
-- * 'pllCallback'
projectsLocationsList
    :: Text -- ^ 'pllParent'
    -> ProjectsLocationsList
projectsLocationsList pPllParent_ = 
    ProjectsLocationsList'
    { _pllParent = pPllParent_
    , _pllXgafv = Nothing
    , _pllUploadProtocol = Nothing
    , _pllPp = True
    , _pllAccessToken = Nothing
    , _pllUploadType = Nothing
    , _pllBearerToken = Nothing
    , _pllPageToken = Nothing
    , _pllPageSize = Nothing
    , _pllFields = Nothing
    , _pllCallback = Nothing
    }

-- | Required. The name of the project for which available locations are to
-- be listed (since some locations might be whitelisted for specific
-- projects).
pllParent :: Lens' ProjectsLocationsList Text
pllParent
  = lens _pllParent (\ s a -> s{_pllParent = a})

-- | V1 error format.
pllXgafv :: Lens' ProjectsLocationsList (Maybe Xgafv)
pllXgafv = lens _pllXgafv (\ s a -> s{_pllXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pllUploadProtocol :: Lens' ProjectsLocationsList (Maybe Text)
pllUploadProtocol
  = lens _pllUploadProtocol
      (\ s a -> s{_pllUploadProtocol = a})

-- | Pretty-print response.
pllPp :: Lens' ProjectsLocationsList Bool
pllPp = lens _pllPp (\ s a -> s{_pllPp = a})

-- | OAuth access token.
pllAccessToken :: Lens' ProjectsLocationsList (Maybe Text)
pllAccessToken
  = lens _pllAccessToken
      (\ s a -> s{_pllAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pllUploadType :: Lens' ProjectsLocationsList (Maybe Text)
pllUploadType
  = lens _pllUploadType
      (\ s a -> s{_pllUploadType = a})

-- | OAuth bearer token.
pllBearerToken :: Lens' ProjectsLocationsList (Maybe Text)
pllBearerToken
  = lens _pllBearerToken
      (\ s a -> s{_pllBearerToken = a})

-- | Optional. A page token to request the next page of results. You get the
-- token from the \`next_page_token\` field of the response from the
-- previous call.
pllPageToken :: Lens' ProjectsLocationsList (Maybe Text)
pllPageToken
  = lens _pllPageToken (\ s a -> s{_pllPageToken = a})

-- | Optional. The number of locations to retrieve per \"page\" of results.
-- If there are more remaining results than this number, the response
-- message will contain a valid value in the \`next_page_token\` field. The
-- default value is 20, and the maximum page size is 100.
pllPageSize :: Lens' ProjectsLocationsList (Maybe Int32)
pllPageSize
  = lens _pllPageSize (\ s a -> s{_pllPageSize = a}) .
      mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
pllFields :: Lens' ProjectsLocationsList (Maybe Text)
pllFields
  = lens _pllFields (\ s a -> s{_pllFields = a})

-- | JSONP
pllCallback :: Lens' ProjectsLocationsList (Maybe Text)
pllCallback
  = lens _pllCallback (\ s a -> s{_pllCallback = a})

instance GoogleRequest ProjectsLocationsList where
        type Rs ProjectsLocationsList =
             GoogleCloudMlV1__ListLocationsResponse
        type Scopes ProjectsLocationsList =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsLocationsList'{..}
          = go _pllParent _pllXgafv _pllUploadProtocol
              (Just _pllPp)
              _pllAccessToken
              _pllUploadType
              _pllBearerToken
              _pllPageToken
              _pllPageSize
              _pllCallback
              _pllFields
              (Just AltJSON)
              machineLearningService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsLocationsListResource)
                      mempty
