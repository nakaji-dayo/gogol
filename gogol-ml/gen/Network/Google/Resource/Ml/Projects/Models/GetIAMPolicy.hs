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
-- Module      : Network.Google.Resource.Ml.Projects.Models.GetIAMPolicy
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the access control policy for a resource. Returns an empty policy
-- if the resource exists and does not have a policy set.
--
-- /See:/ <https://cloud.google.com/ml/ Google Cloud Machine Learning Engine Reference> for @ml.projects.models.getIamPolicy@.
module Network.Google.Resource.Ml.Projects.Models.GetIAMPolicy
    (
    -- * REST Resource
      ProjectsModelsGetIAMPolicyResource

    -- * Creating a Request
    , projectsModelsGetIAMPolicy
    , ProjectsModelsGetIAMPolicy

    -- * Request Lenses
    , pmgipXgafv
    , pmgipUploadProtocol
    , pmgipPp
    , pmgipAccessToken
    , pmgipUploadType
    , pmgipBearerToken
    , pmgipResource
    , pmgipFields
    , pmgipCallback
    ) where

import Network.Google.MachineLearning.Types
import Network.Google.Prelude

-- | A resource alias for @ml.projects.models.getIamPolicy@ method which the
-- 'ProjectsModelsGetIAMPolicy' request conforms to.
type ProjectsModelsGetIAMPolicyResource =
     "v1" :>
       CaptureMode "resource" "getIamPolicy" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON] GoogleIAMV1__Policy

-- | Gets the access control policy for a resource. Returns an empty policy
-- if the resource exists and does not have a policy set.
--
-- /See:/ 'projectsModelsGetIAMPolicy' smart constructor.
data ProjectsModelsGetIAMPolicy = ProjectsModelsGetIAMPolicy'
    { _pmgipXgafv :: !(Maybe Xgafv)
    , _pmgipUploadProtocol :: !(Maybe Text)
    , _pmgipPp :: !Bool
    , _pmgipAccessToken :: !(Maybe Text)
    , _pmgipUploadType :: !(Maybe Text)
    , _pmgipBearerToken :: !(Maybe Text)
    , _pmgipResource :: !Text
    , _pmgipFields :: !(Maybe Text)
    , _pmgipCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsModelsGetIAMPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmgipXgafv'
--
-- * 'pmgipUploadProtocol'
--
-- * 'pmgipPp'
--
-- * 'pmgipAccessToken'
--
-- * 'pmgipUploadType'
--
-- * 'pmgipBearerToken'
--
-- * 'pmgipResource'
--
-- * 'pmgipFields'
--
-- * 'pmgipCallback'
projectsModelsGetIAMPolicy
    :: Text -- ^ 'pmgipResource'
    -> ProjectsModelsGetIAMPolicy
projectsModelsGetIAMPolicy pPmgipResource_ = 
    ProjectsModelsGetIAMPolicy'
    { _pmgipXgafv = Nothing
    , _pmgipUploadProtocol = Nothing
    , _pmgipPp = True
    , _pmgipAccessToken = Nothing
    , _pmgipUploadType = Nothing
    , _pmgipBearerToken = Nothing
    , _pmgipResource = pPmgipResource_
    , _pmgipFields = Nothing
    , _pmgipCallback = Nothing
    }

-- | V1 error format.
pmgipXgafv :: Lens' ProjectsModelsGetIAMPolicy (Maybe Xgafv)
pmgipXgafv
  = lens _pmgipXgafv (\ s a -> s{_pmgipXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pmgipUploadProtocol :: Lens' ProjectsModelsGetIAMPolicy (Maybe Text)
pmgipUploadProtocol
  = lens _pmgipUploadProtocol
      (\ s a -> s{_pmgipUploadProtocol = a})

-- | Pretty-print response.
pmgipPp :: Lens' ProjectsModelsGetIAMPolicy Bool
pmgipPp = lens _pmgipPp (\ s a -> s{_pmgipPp = a})

-- | OAuth access token.
pmgipAccessToken :: Lens' ProjectsModelsGetIAMPolicy (Maybe Text)
pmgipAccessToken
  = lens _pmgipAccessToken
      (\ s a -> s{_pmgipAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pmgipUploadType :: Lens' ProjectsModelsGetIAMPolicy (Maybe Text)
pmgipUploadType
  = lens _pmgipUploadType
      (\ s a -> s{_pmgipUploadType = a})

-- | OAuth bearer token.
pmgipBearerToken :: Lens' ProjectsModelsGetIAMPolicy (Maybe Text)
pmgipBearerToken
  = lens _pmgipBearerToken
      (\ s a -> s{_pmgipBearerToken = a})

-- | REQUIRED: The resource for which the policy is being requested. See the
-- operation documentation for the appropriate value for this field.
pmgipResource :: Lens' ProjectsModelsGetIAMPolicy Text
pmgipResource
  = lens _pmgipResource
      (\ s a -> s{_pmgipResource = a})

-- | Selector specifying which fields to include in a partial response.
pmgipFields :: Lens' ProjectsModelsGetIAMPolicy (Maybe Text)
pmgipFields
  = lens _pmgipFields (\ s a -> s{_pmgipFields = a})

-- | JSONP
pmgipCallback :: Lens' ProjectsModelsGetIAMPolicy (Maybe Text)
pmgipCallback
  = lens _pmgipCallback
      (\ s a -> s{_pmgipCallback = a})

instance GoogleRequest ProjectsModelsGetIAMPolicy
         where
        type Rs ProjectsModelsGetIAMPolicy =
             GoogleIAMV1__Policy
        type Scopes ProjectsModelsGetIAMPolicy =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsModelsGetIAMPolicy'{..}
          = go _pmgipResource _pmgipXgafv _pmgipUploadProtocol
              (Just _pmgipPp)
              _pmgipAccessToken
              _pmgipUploadType
              _pmgipBearerToken
              _pmgipCallback
              _pmgipFields
              (Just AltJSON)
              machineLearningService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsModelsGetIAMPolicyResource)
                      mempty
