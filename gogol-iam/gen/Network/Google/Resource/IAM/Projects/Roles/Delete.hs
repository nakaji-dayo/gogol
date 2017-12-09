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
-- Module      : Network.Google.Resource.IAM.Projects.Roles.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Soft deletes a role. The role is suspended and cannot be used to create
-- new IAM Policy Bindings. The Role will not be included in
-- \`ListRoles()\` unless \`show_deleted\` is set in the
-- \`ListRolesRequest\`. The Role contains the deleted boolean set.
-- Existing Bindings remains, but are inactive. The Role can be undeleted
-- within 7 days. After 7 days the Role is deleted and all Bindings
-- associated with the role are removed.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.projects.roles.delete@.
module Network.Google.Resource.IAM.Projects.Roles.Delete
    (
    -- * REST Resource
      ProjectsRolesDeleteResource

    -- * Creating a Request
    , projectsRolesDelete
    , ProjectsRolesDelete

    -- * Request Lenses
    , prdXgafv
    , prdEtag
    , prdUploadProtocol
    , prdPp
    , prdAccessToken
    , prdUploadType
    , prdBearerToken
    , prdName
    , prdFields
    , prdCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.projects.roles.delete@ method which the
-- 'ProjectsRolesDelete' request conforms to.
type ProjectsRolesDeleteResource =
     "v1" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "etag" Bytes :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :> Delete '[JSON] Role

-- | Soft deletes a role. The role is suspended and cannot be used to create
-- new IAM Policy Bindings. The Role will not be included in
-- \`ListRoles()\` unless \`show_deleted\` is set in the
-- \`ListRolesRequest\`. The Role contains the deleted boolean set.
-- Existing Bindings remains, but are inactive. The Role can be undeleted
-- within 7 days. After 7 days the Role is deleted and all Bindings
-- associated with the role are removed.
--
-- /See:/ 'projectsRolesDelete' smart constructor.
data ProjectsRolesDelete = ProjectsRolesDelete'
    { _prdXgafv :: !(Maybe Xgafv)
    , _prdEtag :: !(Maybe Bytes)
    , _prdUploadProtocol :: !(Maybe Text)
    , _prdPp :: !Bool
    , _prdAccessToken :: !(Maybe Text)
    , _prdUploadType :: !(Maybe Text)
    , _prdBearerToken :: !(Maybe Text)
    , _prdName :: !Text
    , _prdFields :: !(Maybe Text)
    , _prdCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsRolesDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prdXgafv'
--
-- * 'prdEtag'
--
-- * 'prdUploadProtocol'
--
-- * 'prdPp'
--
-- * 'prdAccessToken'
--
-- * 'prdUploadType'
--
-- * 'prdBearerToken'
--
-- * 'prdName'
--
-- * 'prdFields'
--
-- * 'prdCallback'
projectsRolesDelete
    :: Text -- ^ 'prdName'
    -> ProjectsRolesDelete
projectsRolesDelete pPrdName_ = 
    ProjectsRolesDelete'
    { _prdXgafv = Nothing
    , _prdEtag = Nothing
    , _prdUploadProtocol = Nothing
    , _prdPp = True
    , _prdAccessToken = Nothing
    , _prdUploadType = Nothing
    , _prdBearerToken = Nothing
    , _prdName = pPrdName_
    , _prdFields = Nothing
    , _prdCallback = Nothing
    }

-- | V1 error format.
prdXgafv :: Lens' ProjectsRolesDelete (Maybe Xgafv)
prdXgafv = lens _prdXgafv (\ s a -> s{_prdXgafv = a})

-- | Used to perform a consistent read-modify-write.
prdEtag :: Lens' ProjectsRolesDelete (Maybe ByteString)
prdEtag
  = lens _prdEtag (\ s a -> s{_prdEtag = a}) .
      mapping _Bytes

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
prdUploadProtocol :: Lens' ProjectsRolesDelete (Maybe Text)
prdUploadProtocol
  = lens _prdUploadProtocol
      (\ s a -> s{_prdUploadProtocol = a})

-- | Pretty-print response.
prdPp :: Lens' ProjectsRolesDelete Bool
prdPp = lens _prdPp (\ s a -> s{_prdPp = a})

-- | OAuth access token.
prdAccessToken :: Lens' ProjectsRolesDelete (Maybe Text)
prdAccessToken
  = lens _prdAccessToken
      (\ s a -> s{_prdAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
prdUploadType :: Lens' ProjectsRolesDelete (Maybe Text)
prdUploadType
  = lens _prdUploadType
      (\ s a -> s{_prdUploadType = a})

-- | OAuth bearer token.
prdBearerToken :: Lens' ProjectsRolesDelete (Maybe Text)
prdBearerToken
  = lens _prdBearerToken
      (\ s a -> s{_prdBearerToken = a})

-- | The resource name of the role in one of the following formats:
-- \`organizations\/{ORGANIZATION_ID}\/roles\/{ROLE_NAME}\`
-- \`projects\/{PROJECT_ID}\/roles\/{ROLE_NAME}\`
prdName :: Lens' ProjectsRolesDelete Text
prdName = lens _prdName (\ s a -> s{_prdName = a})

-- | Selector specifying which fields to include in a partial response.
prdFields :: Lens' ProjectsRolesDelete (Maybe Text)
prdFields
  = lens _prdFields (\ s a -> s{_prdFields = a})

-- | JSONP
prdCallback :: Lens' ProjectsRolesDelete (Maybe Text)
prdCallback
  = lens _prdCallback (\ s a -> s{_prdCallback = a})

instance GoogleRequest ProjectsRolesDelete where
        type Rs ProjectsRolesDelete = Role
        type Scopes ProjectsRolesDelete =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsRolesDelete'{..}
          = go _prdName _prdXgafv _prdEtag _prdUploadProtocol
              (Just _prdPp)
              _prdAccessToken
              _prdUploadType
              _prdBearerToken
              _prdCallback
              _prdFields
              (Just AltJSON)
              iAMService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsRolesDeleteResource)
                      mempty
