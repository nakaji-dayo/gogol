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
-- Module      : Network.Google.Resource.FirebaseRules.Projects.Releases.GetExecutable
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the \`Release\` executable to use when enforcing rules.
--
-- /See:/ <https://firebase.google.com/docs/storage/security Firebase Rules API Reference> for @firebaserules.projects.releases.getExecutable@.
module Network.Google.Resource.FirebaseRules.Projects.Releases.GetExecutable
    (
    -- * REST Resource
      ProjectsReleasesGetExecutableResource

    -- * Creating a Request
    , projectsReleasesGetExecutable
    , ProjectsReleasesGetExecutable

    -- * Request Lenses
    , prgeXgafv
    , prgeUploadProtocol
    , prgePp
    , prgeAccessToken
    , prgeUploadType
    , prgeBearerToken
    , prgeName
    , prgeExecutableVersion
    , prgeFields
    , prgeCallback
    ) where

import Network.Google.FirebaseRules.Types
import Network.Google.Prelude

-- | A resource alias for @firebaserules.projects.releases.getExecutable@ method which the
-- 'ProjectsReleasesGetExecutable' request conforms to.
type ProjectsReleasesGetExecutableResource =
     "v1" :>
       CaptureMode "name" "getExecutable" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "executableVersion" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] GetReleaseExecutableResponse

-- | Get the \`Release\` executable to use when enforcing rules.
--
-- /See:/ 'projectsReleasesGetExecutable' smart constructor.
data ProjectsReleasesGetExecutable = ProjectsReleasesGetExecutable'
    { _prgeXgafv :: !(Maybe Xgafv)
    , _prgeUploadProtocol :: !(Maybe Text)
    , _prgePp :: !Bool
    , _prgeAccessToken :: !(Maybe Text)
    , _prgeUploadType :: !(Maybe Text)
    , _prgeBearerToken :: !(Maybe Text)
    , _prgeName :: !Text
    , _prgeExecutableVersion :: !(Maybe Text)
    , _prgeFields :: !(Maybe Text)
    , _prgeCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsReleasesGetExecutable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prgeXgafv'
--
-- * 'prgeUploadProtocol'
--
-- * 'prgePp'
--
-- * 'prgeAccessToken'
--
-- * 'prgeUploadType'
--
-- * 'prgeBearerToken'
--
-- * 'prgeName'
--
-- * 'prgeExecutableVersion'
--
-- * 'prgeFields'
--
-- * 'prgeCallback'
projectsReleasesGetExecutable
    :: Text -- ^ 'prgeName'
    -> ProjectsReleasesGetExecutable
projectsReleasesGetExecutable pPrgeName_ = 
    ProjectsReleasesGetExecutable'
    { _prgeXgafv = Nothing
    , _prgeUploadProtocol = Nothing
    , _prgePp = True
    , _prgeAccessToken = Nothing
    , _prgeUploadType = Nothing
    , _prgeBearerToken = Nothing
    , _prgeName = pPrgeName_
    , _prgeExecutableVersion = Nothing
    , _prgeFields = Nothing
    , _prgeCallback = Nothing
    }

-- | V1 error format.
prgeXgafv :: Lens' ProjectsReleasesGetExecutable (Maybe Xgafv)
prgeXgafv
  = lens _prgeXgafv (\ s a -> s{_prgeXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
prgeUploadProtocol :: Lens' ProjectsReleasesGetExecutable (Maybe Text)
prgeUploadProtocol
  = lens _prgeUploadProtocol
      (\ s a -> s{_prgeUploadProtocol = a})

-- | Pretty-print response.
prgePp :: Lens' ProjectsReleasesGetExecutable Bool
prgePp = lens _prgePp (\ s a -> s{_prgePp = a})

-- | OAuth access token.
prgeAccessToken :: Lens' ProjectsReleasesGetExecutable (Maybe Text)
prgeAccessToken
  = lens _prgeAccessToken
      (\ s a -> s{_prgeAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
prgeUploadType :: Lens' ProjectsReleasesGetExecutable (Maybe Text)
prgeUploadType
  = lens _prgeUploadType
      (\ s a -> s{_prgeUploadType = a})

-- | OAuth bearer token.
prgeBearerToken :: Lens' ProjectsReleasesGetExecutable (Maybe Text)
prgeBearerToken
  = lens _prgeBearerToken
      (\ s a -> s{_prgeBearerToken = a})

-- | Resource name of the \`Release\`. Format:
-- \`projects\/{project_id}\/releases\/{release_id}\`
prgeName :: Lens' ProjectsReleasesGetExecutable Text
prgeName = lens _prgeName (\ s a -> s{_prgeName = a})

-- | The requested runtime executable version. Defaults to
-- FIREBASE_RULES_EXECUTABLE_V1
prgeExecutableVersion :: Lens' ProjectsReleasesGetExecutable (Maybe Text)
prgeExecutableVersion
  = lens _prgeExecutableVersion
      (\ s a -> s{_prgeExecutableVersion = a})

-- | Selector specifying which fields to include in a partial response.
prgeFields :: Lens' ProjectsReleasesGetExecutable (Maybe Text)
prgeFields
  = lens _prgeFields (\ s a -> s{_prgeFields = a})

-- | JSONP
prgeCallback :: Lens' ProjectsReleasesGetExecutable (Maybe Text)
prgeCallback
  = lens _prgeCallback (\ s a -> s{_prgeCallback = a})

instance GoogleRequest ProjectsReleasesGetExecutable
         where
        type Rs ProjectsReleasesGetExecutable =
             GetReleaseExecutableResponse
        type Scopes ProjectsReleasesGetExecutable =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/firebase",
               "https://www.googleapis.com/auth/firebase.readonly"]
        requestClient ProjectsReleasesGetExecutable'{..}
          = go _prgeName _prgeXgafv _prgeUploadProtocol
              (Just _prgePp)
              _prgeAccessToken
              _prgeUploadType
              _prgeBearerToken
              _prgeExecutableVersion
              _prgeCallback
              _prgeFields
              (Just AltJSON)
              firebaseRulesService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsReleasesGetExecutableResource)
                      mempty
