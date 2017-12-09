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
-- Module      : Network.Google.Resource.IAM.Projects.Roles.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Role.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.projects.roles.create@.
module Network.Google.Resource.IAM.Projects.Roles.Create
    (
    -- * REST Resource
      ProjectsRolesCreateResource

    -- * Creating a Request
    , projectsRolesCreate
    , ProjectsRolesCreate

    -- * Request Lenses
    , prcParent
    , prcXgafv
    , prcUploadProtocol
    , prcPp
    , prcAccessToken
    , prcUploadType
    , prcPayload
    , prcBearerToken
    , prcFields
    , prcCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.projects.roles.create@ method which the
-- 'ProjectsRolesCreate' request conforms to.
type ProjectsRolesCreateResource =
     "v1" :>
       Capture "parent" Text :>
         "roles" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             ReqBody '[JSON] CreateRoleRequest :>
                               Post '[JSON] Role

-- | Creates a new Role.
--
-- /See:/ 'projectsRolesCreate' smart constructor.
data ProjectsRolesCreate = ProjectsRolesCreate'
    { _prcParent :: !Text
    , _prcXgafv :: !(Maybe Xgafv)
    , _prcUploadProtocol :: !(Maybe Text)
    , _prcPp :: !Bool
    , _prcAccessToken :: !(Maybe Text)
    , _prcUploadType :: !(Maybe Text)
    , _prcPayload :: !CreateRoleRequest
    , _prcBearerToken :: !(Maybe Text)
    , _prcFields :: !(Maybe Text)
    , _prcCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsRolesCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prcParent'
--
-- * 'prcXgafv'
--
-- * 'prcUploadProtocol'
--
-- * 'prcPp'
--
-- * 'prcAccessToken'
--
-- * 'prcUploadType'
--
-- * 'prcPayload'
--
-- * 'prcBearerToken'
--
-- * 'prcFields'
--
-- * 'prcCallback'
projectsRolesCreate
    :: Text -- ^ 'prcParent'
    -> CreateRoleRequest -- ^ 'prcPayload'
    -> ProjectsRolesCreate
projectsRolesCreate pPrcParent_ pPrcPayload_ = 
    ProjectsRolesCreate'
    { _prcParent = pPrcParent_
    , _prcXgafv = Nothing
    , _prcUploadProtocol = Nothing
    , _prcPp = True
    , _prcAccessToken = Nothing
    , _prcUploadType = Nothing
    , _prcPayload = pPrcPayload_
    , _prcBearerToken = Nothing
    , _prcFields = Nothing
    , _prcCallback = Nothing
    }

-- | The resource name of the parent resource in one of the following
-- formats: \`organizations\/{ORGANIZATION_ID}\` \`projects\/{PROJECT_ID}\`
prcParent :: Lens' ProjectsRolesCreate Text
prcParent
  = lens _prcParent (\ s a -> s{_prcParent = a})

-- | V1 error format.
prcXgafv :: Lens' ProjectsRolesCreate (Maybe Xgafv)
prcXgafv = lens _prcXgafv (\ s a -> s{_prcXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
prcUploadProtocol :: Lens' ProjectsRolesCreate (Maybe Text)
prcUploadProtocol
  = lens _prcUploadProtocol
      (\ s a -> s{_prcUploadProtocol = a})

-- | Pretty-print response.
prcPp :: Lens' ProjectsRolesCreate Bool
prcPp = lens _prcPp (\ s a -> s{_prcPp = a})

-- | OAuth access token.
prcAccessToken :: Lens' ProjectsRolesCreate (Maybe Text)
prcAccessToken
  = lens _prcAccessToken
      (\ s a -> s{_prcAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
prcUploadType :: Lens' ProjectsRolesCreate (Maybe Text)
prcUploadType
  = lens _prcUploadType
      (\ s a -> s{_prcUploadType = a})

-- | Multipart request metadata.
prcPayload :: Lens' ProjectsRolesCreate CreateRoleRequest
prcPayload
  = lens _prcPayload (\ s a -> s{_prcPayload = a})

-- | OAuth bearer token.
prcBearerToken :: Lens' ProjectsRolesCreate (Maybe Text)
prcBearerToken
  = lens _prcBearerToken
      (\ s a -> s{_prcBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
prcFields :: Lens' ProjectsRolesCreate (Maybe Text)
prcFields
  = lens _prcFields (\ s a -> s{_prcFields = a})

-- | JSONP
prcCallback :: Lens' ProjectsRolesCreate (Maybe Text)
prcCallback
  = lens _prcCallback (\ s a -> s{_prcCallback = a})

instance GoogleRequest ProjectsRolesCreate where
        type Rs ProjectsRolesCreate = Role
        type Scopes ProjectsRolesCreate =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsRolesCreate'{..}
          = go _prcParent _prcXgafv _prcUploadProtocol
              (Just _prcPp)
              _prcAccessToken
              _prcUploadType
              _prcBearerToken
              _prcCallback
              _prcFields
              (Just AltJSON)
              _prcPayload
              iAMService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsRolesCreateResource)
                      mempty
