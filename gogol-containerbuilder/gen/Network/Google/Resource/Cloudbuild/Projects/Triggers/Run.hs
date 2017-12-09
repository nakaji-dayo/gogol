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
-- Module      : Network.Google.Resource.Cloudbuild.Projects.Triggers.Run
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs a BuildTrigger at a particular source revision.
--
-- /See:/ <https://cloud.google.com/container-builder/docs/ Google Cloud Container Builder API Reference> for @cloudbuild.projects.triggers.run@.
module Network.Google.Resource.Cloudbuild.Projects.Triggers.Run
    (
    -- * REST Resource
      ProjectsTriggersRunResource

    -- * Creating a Request
    , projectsTriggersRun
    , ProjectsTriggersRun

    -- * Request Lenses
    , ptrXgafv
    , ptrUploadProtocol
    , ptrTriggerId
    , ptrPp
    , ptrAccessToken
    , ptrUploadType
    , ptrPayload
    , ptrBearerToken
    , ptrProjectId
    , ptrFields
    , ptrCallback
    ) where

import Network.Google.ContainerBuilder.Types
import Network.Google.Prelude

-- | A resource alias for @cloudbuild.projects.triggers.run@ method which the
-- 'ProjectsTriggersRun' request conforms to.
type ProjectsTriggersRunResource =
     "v1" :>
       "projects" :>
         Capture "projectId" Text :>
           "triggers" :>
             CaptureMode "triggerId" "run" Text :>
               QueryParam "$.xgafv" Xgafv :>
                 QueryParam "upload_protocol" Text :>
                   QueryParam "pp" Bool :>
                     QueryParam "access_token" Text :>
                       QueryParam "uploadType" Text :>
                         QueryParam "bearer_token" Text :>
                           QueryParam "callback" Text :>
                             QueryParam "fields" Text :>
                               QueryParam "alt" AltJSON :>
                                 ReqBody '[JSON] RepoSource :>
                                   Post '[JSON] Operation

-- | Runs a BuildTrigger at a particular source revision.
--
-- /See:/ 'projectsTriggersRun' smart constructor.
data ProjectsTriggersRun = ProjectsTriggersRun'
    { _ptrXgafv :: !(Maybe Xgafv)
    , _ptrUploadProtocol :: !(Maybe Text)
    , _ptrTriggerId :: !Text
    , _ptrPp :: !Bool
    , _ptrAccessToken :: !(Maybe Text)
    , _ptrUploadType :: !(Maybe Text)
    , _ptrPayload :: !RepoSource
    , _ptrBearerToken :: !(Maybe Text)
    , _ptrProjectId :: !Text
    , _ptrFields :: !(Maybe Text)
    , _ptrCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsTriggersRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptrXgafv'
--
-- * 'ptrUploadProtocol'
--
-- * 'ptrTriggerId'
--
-- * 'ptrPp'
--
-- * 'ptrAccessToken'
--
-- * 'ptrUploadType'
--
-- * 'ptrPayload'
--
-- * 'ptrBearerToken'
--
-- * 'ptrProjectId'
--
-- * 'ptrFields'
--
-- * 'ptrCallback'
projectsTriggersRun
    :: Text -- ^ 'ptrTriggerId'
    -> RepoSource -- ^ 'ptrPayload'
    -> Text -- ^ 'ptrProjectId'
    -> ProjectsTriggersRun
projectsTriggersRun pPtrTriggerId_ pPtrPayload_ pPtrProjectId_ = 
    ProjectsTriggersRun'
    { _ptrXgafv = Nothing
    , _ptrUploadProtocol = Nothing
    , _ptrTriggerId = pPtrTriggerId_
    , _ptrPp = True
    , _ptrAccessToken = Nothing
    , _ptrUploadType = Nothing
    , _ptrPayload = pPtrPayload_
    , _ptrBearerToken = Nothing
    , _ptrProjectId = pPtrProjectId_
    , _ptrFields = Nothing
    , _ptrCallback = Nothing
    }

-- | V1 error format.
ptrXgafv :: Lens' ProjectsTriggersRun (Maybe Xgafv)
ptrXgafv = lens _ptrXgafv (\ s a -> s{_ptrXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
ptrUploadProtocol :: Lens' ProjectsTriggersRun (Maybe Text)
ptrUploadProtocol
  = lens _ptrUploadProtocol
      (\ s a -> s{_ptrUploadProtocol = a})

-- | ID of the trigger.
ptrTriggerId :: Lens' ProjectsTriggersRun Text
ptrTriggerId
  = lens _ptrTriggerId (\ s a -> s{_ptrTriggerId = a})

-- | Pretty-print response.
ptrPp :: Lens' ProjectsTriggersRun Bool
ptrPp = lens _ptrPp (\ s a -> s{_ptrPp = a})

-- | OAuth access token.
ptrAccessToken :: Lens' ProjectsTriggersRun (Maybe Text)
ptrAccessToken
  = lens _ptrAccessToken
      (\ s a -> s{_ptrAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
ptrUploadType :: Lens' ProjectsTriggersRun (Maybe Text)
ptrUploadType
  = lens _ptrUploadType
      (\ s a -> s{_ptrUploadType = a})

-- | Multipart request metadata.
ptrPayload :: Lens' ProjectsTriggersRun RepoSource
ptrPayload
  = lens _ptrPayload (\ s a -> s{_ptrPayload = a})

-- | OAuth bearer token.
ptrBearerToken :: Lens' ProjectsTriggersRun (Maybe Text)
ptrBearerToken
  = lens _ptrBearerToken
      (\ s a -> s{_ptrBearerToken = a})

-- | ID of the project.
ptrProjectId :: Lens' ProjectsTriggersRun Text
ptrProjectId
  = lens _ptrProjectId (\ s a -> s{_ptrProjectId = a})

-- | Selector specifying which fields to include in a partial response.
ptrFields :: Lens' ProjectsTriggersRun (Maybe Text)
ptrFields
  = lens _ptrFields (\ s a -> s{_ptrFields = a})

-- | JSONP
ptrCallback :: Lens' ProjectsTriggersRun (Maybe Text)
ptrCallback
  = lens _ptrCallback (\ s a -> s{_ptrCallback = a})

instance GoogleRequest ProjectsTriggersRun where
        type Rs ProjectsTriggersRun = Operation
        type Scopes ProjectsTriggersRun =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsTriggersRun'{..}
          = go _ptrProjectId _ptrTriggerId _ptrXgafv
              _ptrUploadProtocol
              (Just _ptrPp)
              _ptrAccessToken
              _ptrUploadType
              _ptrBearerToken
              _ptrCallback
              _ptrFields
              (Just AltJSON)
              _ptrPayload
              containerBuilderService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsTriggersRunResource)
                      mempty
