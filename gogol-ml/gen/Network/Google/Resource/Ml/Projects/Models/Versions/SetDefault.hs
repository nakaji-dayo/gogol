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
-- Module      : Network.Google.Resource.Ml.Projects.Models.Versions.SetDefault
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Designates a version to be the default for the model. The default
-- version is used for prediction requests made against the model that
-- don\'t specify a version. The first version to be created for a model is
-- automatically set as the default. You must make any subsequent changes
-- to the default version setting manually using this method.
--
-- /See:/ <https://cloud.google.com/ml/ Google Cloud Machine Learning Engine Reference> for @ml.projects.models.versions.setDefault@.
module Network.Google.Resource.Ml.Projects.Models.Versions.SetDefault
    (
    -- * REST Resource
      ProjectsModelsVersionsSetDefaultResource

    -- * Creating a Request
    , projectsModelsVersionsSetDefault
    , ProjectsModelsVersionsSetDefault

    -- * Request Lenses
    , pmvsdXgafv
    , pmvsdUploadProtocol
    , pmvsdPp
    , pmvsdAccessToken
    , pmvsdUploadType
    , pmvsdPayload
    , pmvsdBearerToken
    , pmvsdName
    , pmvsdFields
    , pmvsdCallback
    ) where

import Network.Google.MachineLearning.Types
import Network.Google.Prelude

-- | A resource alias for @ml.projects.models.versions.setDefault@ method which the
-- 'ProjectsModelsVersionsSetDefault' request conforms to.
type ProjectsModelsVersionsSetDefaultResource =
     "v1" :>
       CaptureMode "name" "setDefault" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON]
                             GoogleCloudMlV1__SetDefaultVersionRequest
                             :> Post '[JSON] GoogleCloudMlV1__Version

-- | Designates a version to be the default for the model. The default
-- version is used for prediction requests made against the model that
-- don\'t specify a version. The first version to be created for a model is
-- automatically set as the default. You must make any subsequent changes
-- to the default version setting manually using this method.
--
-- /See:/ 'projectsModelsVersionsSetDefault' smart constructor.
data ProjectsModelsVersionsSetDefault = ProjectsModelsVersionsSetDefault'
    { _pmvsdXgafv :: !(Maybe Xgafv)
    , _pmvsdUploadProtocol :: !(Maybe Text)
    , _pmvsdPp :: !Bool
    , _pmvsdAccessToken :: !(Maybe Text)
    , _pmvsdUploadType :: !(Maybe Text)
    , _pmvsdPayload :: !GoogleCloudMlV1__SetDefaultVersionRequest
    , _pmvsdBearerToken :: !(Maybe Text)
    , _pmvsdName :: !Text
    , _pmvsdFields :: !(Maybe Text)
    , _pmvsdCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsModelsVersionsSetDefault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmvsdXgafv'
--
-- * 'pmvsdUploadProtocol'
--
-- * 'pmvsdPp'
--
-- * 'pmvsdAccessToken'
--
-- * 'pmvsdUploadType'
--
-- * 'pmvsdPayload'
--
-- * 'pmvsdBearerToken'
--
-- * 'pmvsdName'
--
-- * 'pmvsdFields'
--
-- * 'pmvsdCallback'
projectsModelsVersionsSetDefault
    :: GoogleCloudMlV1__SetDefaultVersionRequest -- ^ 'pmvsdPayload'
    -> Text -- ^ 'pmvsdName'
    -> ProjectsModelsVersionsSetDefault
projectsModelsVersionsSetDefault pPmvsdPayload_ pPmvsdName_ = 
    ProjectsModelsVersionsSetDefault'
    { _pmvsdXgafv = Nothing
    , _pmvsdUploadProtocol = Nothing
    , _pmvsdPp = True
    , _pmvsdAccessToken = Nothing
    , _pmvsdUploadType = Nothing
    , _pmvsdPayload = pPmvsdPayload_
    , _pmvsdBearerToken = Nothing
    , _pmvsdName = pPmvsdName_
    , _pmvsdFields = Nothing
    , _pmvsdCallback = Nothing
    }

-- | V1 error format.
pmvsdXgafv :: Lens' ProjectsModelsVersionsSetDefault (Maybe Xgafv)
pmvsdXgafv
  = lens _pmvsdXgafv (\ s a -> s{_pmvsdXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pmvsdUploadProtocol :: Lens' ProjectsModelsVersionsSetDefault (Maybe Text)
pmvsdUploadProtocol
  = lens _pmvsdUploadProtocol
      (\ s a -> s{_pmvsdUploadProtocol = a})

-- | Pretty-print response.
pmvsdPp :: Lens' ProjectsModelsVersionsSetDefault Bool
pmvsdPp = lens _pmvsdPp (\ s a -> s{_pmvsdPp = a})

-- | OAuth access token.
pmvsdAccessToken :: Lens' ProjectsModelsVersionsSetDefault (Maybe Text)
pmvsdAccessToken
  = lens _pmvsdAccessToken
      (\ s a -> s{_pmvsdAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pmvsdUploadType :: Lens' ProjectsModelsVersionsSetDefault (Maybe Text)
pmvsdUploadType
  = lens _pmvsdUploadType
      (\ s a -> s{_pmvsdUploadType = a})

-- | Multipart request metadata.
pmvsdPayload :: Lens' ProjectsModelsVersionsSetDefault GoogleCloudMlV1__SetDefaultVersionRequest
pmvsdPayload
  = lens _pmvsdPayload (\ s a -> s{_pmvsdPayload = a})

-- | OAuth bearer token.
pmvsdBearerToken :: Lens' ProjectsModelsVersionsSetDefault (Maybe Text)
pmvsdBearerToken
  = lens _pmvsdBearerToken
      (\ s a -> s{_pmvsdBearerToken = a})

-- | Required. The name of the version to make the default for the model. You
-- can get the names of all the versions of a model by calling
-- [projects.models.versions.list](\/ml-engine\/reference\/rest\/v1\/projects.models.versions\/list).
pmvsdName :: Lens' ProjectsModelsVersionsSetDefault Text
pmvsdName
  = lens _pmvsdName (\ s a -> s{_pmvsdName = a})

-- | Selector specifying which fields to include in a partial response.
pmvsdFields :: Lens' ProjectsModelsVersionsSetDefault (Maybe Text)
pmvsdFields
  = lens _pmvsdFields (\ s a -> s{_pmvsdFields = a})

-- | JSONP
pmvsdCallback :: Lens' ProjectsModelsVersionsSetDefault (Maybe Text)
pmvsdCallback
  = lens _pmvsdCallback
      (\ s a -> s{_pmvsdCallback = a})

instance GoogleRequest
         ProjectsModelsVersionsSetDefault where
        type Rs ProjectsModelsVersionsSetDefault =
             GoogleCloudMlV1__Version
        type Scopes ProjectsModelsVersionsSetDefault =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsModelsVersionsSetDefault'{..}
          = go _pmvsdName _pmvsdXgafv _pmvsdUploadProtocol
              (Just _pmvsdPp)
              _pmvsdAccessToken
              _pmvsdUploadType
              _pmvsdBearerToken
              _pmvsdCallback
              _pmvsdFields
              (Just AltJSON)
              _pmvsdPayload
              machineLearningService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsModelsVersionsSetDefaultResource)
                      mempty
