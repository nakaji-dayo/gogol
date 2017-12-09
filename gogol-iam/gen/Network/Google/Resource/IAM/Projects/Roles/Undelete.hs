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
-- Module      : Network.Google.Resource.IAM.Projects.Roles.Undelete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undelete a Role, bringing it back in its previous state.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.projects.roles.undelete@.
module Network.Google.Resource.IAM.Projects.Roles.Undelete
    (
    -- * REST Resource
      ProjectsRolesUndeleteResource

    -- * Creating a Request
    , projectsRolesUndelete
    , ProjectsRolesUndelete

    -- * Request Lenses
    , pruXgafv
    , pruUploadProtocol
    , pruPp
    , pruAccessToken
    , pruUploadType
    , pruPayload
    , pruBearerToken
    , pruName
    , pruFields
    , pruCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.projects.roles.undelete@ method which the
-- 'ProjectsRolesUndelete' request conforms to.
type ProjectsRolesUndeleteResource =
     "v1" :>
       CaptureMode "name" "undelete" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] UndeleteRoleRequest :>
                             Post '[JSON] Role

-- | Undelete a Role, bringing it back in its previous state.
--
-- /See:/ 'projectsRolesUndelete' smart constructor.
data ProjectsRolesUndelete = ProjectsRolesUndelete'
    { _pruXgafv :: !(Maybe Xgafv)
    , _pruUploadProtocol :: !(Maybe Text)
    , _pruPp :: !Bool
    , _pruAccessToken :: !(Maybe Text)
    , _pruUploadType :: !(Maybe Text)
    , _pruPayload :: !UndeleteRoleRequest
    , _pruBearerToken :: !(Maybe Text)
    , _pruName :: !Text
    , _pruFields :: !(Maybe Text)
    , _pruCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsRolesUndelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pruXgafv'
--
-- * 'pruUploadProtocol'
--
-- * 'pruPp'
--
-- * 'pruAccessToken'
--
-- * 'pruUploadType'
--
-- * 'pruPayload'
--
-- * 'pruBearerToken'
--
-- * 'pruName'
--
-- * 'pruFields'
--
-- * 'pruCallback'
projectsRolesUndelete
    :: UndeleteRoleRequest -- ^ 'pruPayload'
    -> Text -- ^ 'pruName'
    -> ProjectsRolesUndelete
projectsRolesUndelete pPruPayload_ pPruName_ = 
    ProjectsRolesUndelete'
    { _pruXgafv = Nothing
    , _pruUploadProtocol = Nothing
    , _pruPp = True
    , _pruAccessToken = Nothing
    , _pruUploadType = Nothing
    , _pruPayload = pPruPayload_
    , _pruBearerToken = Nothing
    , _pruName = pPruName_
    , _pruFields = Nothing
    , _pruCallback = Nothing
    }

-- | V1 error format.
pruXgafv :: Lens' ProjectsRolesUndelete (Maybe Xgafv)
pruXgafv = lens _pruXgafv (\ s a -> s{_pruXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pruUploadProtocol :: Lens' ProjectsRolesUndelete (Maybe Text)
pruUploadProtocol
  = lens _pruUploadProtocol
      (\ s a -> s{_pruUploadProtocol = a})

-- | Pretty-print response.
pruPp :: Lens' ProjectsRolesUndelete Bool
pruPp = lens _pruPp (\ s a -> s{_pruPp = a})

-- | OAuth access token.
pruAccessToken :: Lens' ProjectsRolesUndelete (Maybe Text)
pruAccessToken
  = lens _pruAccessToken
      (\ s a -> s{_pruAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pruUploadType :: Lens' ProjectsRolesUndelete (Maybe Text)
pruUploadType
  = lens _pruUploadType
      (\ s a -> s{_pruUploadType = a})

-- | Multipart request metadata.
pruPayload :: Lens' ProjectsRolesUndelete UndeleteRoleRequest
pruPayload
  = lens _pruPayload (\ s a -> s{_pruPayload = a})

-- | OAuth bearer token.
pruBearerToken :: Lens' ProjectsRolesUndelete (Maybe Text)
pruBearerToken
  = lens _pruBearerToken
      (\ s a -> s{_pruBearerToken = a})

-- | The resource name of the role in one of the following formats:
-- \`organizations\/{ORGANIZATION_ID}\/roles\/{ROLE_NAME}\`
-- \`projects\/{PROJECT_ID}\/roles\/{ROLE_NAME}\`
pruName :: Lens' ProjectsRolesUndelete Text
pruName = lens _pruName (\ s a -> s{_pruName = a})

-- | Selector specifying which fields to include in a partial response.
pruFields :: Lens' ProjectsRolesUndelete (Maybe Text)
pruFields
  = lens _pruFields (\ s a -> s{_pruFields = a})

-- | JSONP
pruCallback :: Lens' ProjectsRolesUndelete (Maybe Text)
pruCallback
  = lens _pruCallback (\ s a -> s{_pruCallback = a})

instance GoogleRequest ProjectsRolesUndelete where
        type Rs ProjectsRolesUndelete = Role
        type Scopes ProjectsRolesUndelete =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsRolesUndelete'{..}
          = go _pruName _pruXgafv _pruUploadProtocol
              (Just _pruPp)
              _pruAccessToken
              _pruUploadType
              _pruBearerToken
              _pruCallback
              _pruFields
              (Just AltJSON)
              _pruPayload
              iAMService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsRolesUndeleteResource)
                      mempty
