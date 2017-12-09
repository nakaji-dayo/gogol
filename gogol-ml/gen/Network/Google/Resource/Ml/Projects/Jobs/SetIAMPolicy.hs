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
-- Module      : Network.Google.Resource.Ml.Projects.Jobs.SetIAMPolicy
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the access control policy on the specified resource. Replaces any
-- existing policy.
--
-- /See:/ <https://cloud.google.com/ml/ Google Cloud Machine Learning Engine Reference> for @ml.projects.jobs.setIamPolicy@.
module Network.Google.Resource.Ml.Projects.Jobs.SetIAMPolicy
    (
    -- * REST Resource
      ProjectsJobsSetIAMPolicyResource

    -- * Creating a Request
    , projectsJobsSetIAMPolicy
    , ProjectsJobsSetIAMPolicy

    -- * Request Lenses
    , pjsipXgafv
    , pjsipUploadProtocol
    , pjsipPp
    , pjsipAccessToken
    , pjsipUploadType
    , pjsipPayload
    , pjsipBearerToken
    , pjsipResource
    , pjsipFields
    , pjsipCallback
    ) where

import Network.Google.MachineLearning.Types
import Network.Google.Prelude

-- | A resource alias for @ml.projects.jobs.setIamPolicy@ method which the
-- 'ProjectsJobsSetIAMPolicy' request conforms to.
type ProjectsJobsSetIAMPolicyResource =
     "v1" :>
       CaptureMode "resource" "setIamPolicy" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] GoogleIAMV1__SetIAMPolicyRequest :>
                             Post '[JSON] GoogleIAMV1__Policy

-- | Sets the access control policy on the specified resource. Replaces any
-- existing policy.
--
-- /See:/ 'projectsJobsSetIAMPolicy' smart constructor.
data ProjectsJobsSetIAMPolicy = ProjectsJobsSetIAMPolicy'
    { _pjsipXgafv :: !(Maybe Xgafv)
    , _pjsipUploadProtocol :: !(Maybe Text)
    , _pjsipPp :: !Bool
    , _pjsipAccessToken :: !(Maybe Text)
    , _pjsipUploadType :: !(Maybe Text)
    , _pjsipPayload :: !GoogleIAMV1__SetIAMPolicyRequest
    , _pjsipBearerToken :: !(Maybe Text)
    , _pjsipResource :: !Text
    , _pjsipFields :: !(Maybe Text)
    , _pjsipCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsJobsSetIAMPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pjsipXgafv'
--
-- * 'pjsipUploadProtocol'
--
-- * 'pjsipPp'
--
-- * 'pjsipAccessToken'
--
-- * 'pjsipUploadType'
--
-- * 'pjsipPayload'
--
-- * 'pjsipBearerToken'
--
-- * 'pjsipResource'
--
-- * 'pjsipFields'
--
-- * 'pjsipCallback'
projectsJobsSetIAMPolicy
    :: GoogleIAMV1__SetIAMPolicyRequest -- ^ 'pjsipPayload'
    -> Text -- ^ 'pjsipResource'
    -> ProjectsJobsSetIAMPolicy
projectsJobsSetIAMPolicy pPjsipPayload_ pPjsipResource_ = 
    ProjectsJobsSetIAMPolicy'
    { _pjsipXgafv = Nothing
    , _pjsipUploadProtocol = Nothing
    , _pjsipPp = True
    , _pjsipAccessToken = Nothing
    , _pjsipUploadType = Nothing
    , _pjsipPayload = pPjsipPayload_
    , _pjsipBearerToken = Nothing
    , _pjsipResource = pPjsipResource_
    , _pjsipFields = Nothing
    , _pjsipCallback = Nothing
    }

-- | V1 error format.
pjsipXgafv :: Lens' ProjectsJobsSetIAMPolicy (Maybe Xgafv)
pjsipXgafv
  = lens _pjsipXgafv (\ s a -> s{_pjsipXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pjsipUploadProtocol :: Lens' ProjectsJobsSetIAMPolicy (Maybe Text)
pjsipUploadProtocol
  = lens _pjsipUploadProtocol
      (\ s a -> s{_pjsipUploadProtocol = a})

-- | Pretty-print response.
pjsipPp :: Lens' ProjectsJobsSetIAMPolicy Bool
pjsipPp = lens _pjsipPp (\ s a -> s{_pjsipPp = a})

-- | OAuth access token.
pjsipAccessToken :: Lens' ProjectsJobsSetIAMPolicy (Maybe Text)
pjsipAccessToken
  = lens _pjsipAccessToken
      (\ s a -> s{_pjsipAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pjsipUploadType :: Lens' ProjectsJobsSetIAMPolicy (Maybe Text)
pjsipUploadType
  = lens _pjsipUploadType
      (\ s a -> s{_pjsipUploadType = a})

-- | Multipart request metadata.
pjsipPayload :: Lens' ProjectsJobsSetIAMPolicy GoogleIAMV1__SetIAMPolicyRequest
pjsipPayload
  = lens _pjsipPayload (\ s a -> s{_pjsipPayload = a})

-- | OAuth bearer token.
pjsipBearerToken :: Lens' ProjectsJobsSetIAMPolicy (Maybe Text)
pjsipBearerToken
  = lens _pjsipBearerToken
      (\ s a -> s{_pjsipBearerToken = a})

-- | REQUIRED: The resource for which the policy is being specified. See the
-- operation documentation for the appropriate value for this field.
pjsipResource :: Lens' ProjectsJobsSetIAMPolicy Text
pjsipResource
  = lens _pjsipResource
      (\ s a -> s{_pjsipResource = a})

-- | Selector specifying which fields to include in a partial response.
pjsipFields :: Lens' ProjectsJobsSetIAMPolicy (Maybe Text)
pjsipFields
  = lens _pjsipFields (\ s a -> s{_pjsipFields = a})

-- | JSONP
pjsipCallback :: Lens' ProjectsJobsSetIAMPolicy (Maybe Text)
pjsipCallback
  = lens _pjsipCallback
      (\ s a -> s{_pjsipCallback = a})

instance GoogleRequest ProjectsJobsSetIAMPolicy where
        type Rs ProjectsJobsSetIAMPolicy =
             GoogleIAMV1__Policy
        type Scopes ProjectsJobsSetIAMPolicy =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsJobsSetIAMPolicy'{..}
          = go _pjsipResource _pjsipXgafv _pjsipUploadProtocol
              (Just _pjsipPp)
              _pjsipAccessToken
              _pjsipUploadType
              _pjsipBearerToken
              _pjsipCallback
              _pjsipFields
              (Just AltJSON)
              _pjsipPayload
              machineLearningService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsJobsSetIAMPolicyResource)
                      mempty
