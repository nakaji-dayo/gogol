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
-- Module      : Network.Google.Resource.IAM.Projects.Roles.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Roles defined on a resource.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.projects.roles.list@.
module Network.Google.Resource.IAM.Projects.Roles.List
    (
    -- * REST Resource
      ProjectsRolesListResource

    -- * Creating a Request
    , projectsRolesList
    , ProjectsRolesList

    -- * Request Lenses
    , prlParent
    , prlXgafv
    , prlUploadProtocol
    , prlPp
    , prlAccessToken
    , prlUploadType
    , prlShowDeleted
    , prlBearerToken
    , prlView
    , prlPageToken
    , prlPageSize
    , prlFields
    , prlCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.projects.roles.list@ method which the
-- 'ProjectsRolesList' request conforms to.
type ProjectsRolesListResource =
     "v1" :>
       Capture "parent" Text :>
         "roles" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "showDeleted" Bool :>
                       QueryParam "bearer_token" Text :>
                         QueryParam "view" Text :>
                           QueryParam "pageToken" Text :>
                             QueryParam "pageSize" (Textual Int32) :>
                               QueryParam "callback" Text :>
                                 QueryParam "fields" Text :>
                                   QueryParam "alt" AltJSON :>
                                     Get '[JSON] ListRolesResponse

-- | Lists the Roles defined on a resource.
--
-- /See:/ 'projectsRolesList' smart constructor.
data ProjectsRolesList = ProjectsRolesList'
    { _prlParent :: !Text
    , _prlXgafv :: !(Maybe Xgafv)
    , _prlUploadProtocol :: !(Maybe Text)
    , _prlPp :: !Bool
    , _prlAccessToken :: !(Maybe Text)
    , _prlUploadType :: !(Maybe Text)
    , _prlShowDeleted :: !(Maybe Bool)
    , _prlBearerToken :: !(Maybe Text)
    , _prlView :: !(Maybe Text)
    , _prlPageToken :: !(Maybe Text)
    , _prlPageSize :: !(Maybe (Textual Int32))
    , _prlFields :: !(Maybe Text)
    , _prlCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsRolesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prlParent'
--
-- * 'prlXgafv'
--
-- * 'prlUploadProtocol'
--
-- * 'prlPp'
--
-- * 'prlAccessToken'
--
-- * 'prlUploadType'
--
-- * 'prlShowDeleted'
--
-- * 'prlBearerToken'
--
-- * 'prlView'
--
-- * 'prlPageToken'
--
-- * 'prlPageSize'
--
-- * 'prlFields'
--
-- * 'prlCallback'
projectsRolesList
    :: Text -- ^ 'prlParent'
    -> ProjectsRolesList
projectsRolesList pPrlParent_ = 
    ProjectsRolesList'
    { _prlParent = pPrlParent_
    , _prlXgafv = Nothing
    , _prlUploadProtocol = Nothing
    , _prlPp = True
    , _prlAccessToken = Nothing
    , _prlUploadType = Nothing
    , _prlShowDeleted = Nothing
    , _prlBearerToken = Nothing
    , _prlView = Nothing
    , _prlPageToken = Nothing
    , _prlPageSize = Nothing
    , _prlFields = Nothing
    , _prlCallback = Nothing
    }

-- | The resource name of the parent resource in one of the following
-- formats: \`\` (empty string) -- this refers to curated roles.
-- \`organizations\/{ORGANIZATION_ID}\` \`projects\/{PROJECT_ID}\`
prlParent :: Lens' ProjectsRolesList Text
prlParent
  = lens _prlParent (\ s a -> s{_prlParent = a})

-- | V1 error format.
prlXgafv :: Lens' ProjectsRolesList (Maybe Xgafv)
prlXgafv = lens _prlXgafv (\ s a -> s{_prlXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
prlUploadProtocol :: Lens' ProjectsRolesList (Maybe Text)
prlUploadProtocol
  = lens _prlUploadProtocol
      (\ s a -> s{_prlUploadProtocol = a})

-- | Pretty-print response.
prlPp :: Lens' ProjectsRolesList Bool
prlPp = lens _prlPp (\ s a -> s{_prlPp = a})

-- | OAuth access token.
prlAccessToken :: Lens' ProjectsRolesList (Maybe Text)
prlAccessToken
  = lens _prlAccessToken
      (\ s a -> s{_prlAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
prlUploadType :: Lens' ProjectsRolesList (Maybe Text)
prlUploadType
  = lens _prlUploadType
      (\ s a -> s{_prlUploadType = a})

-- | Include Roles that have been deleted.
prlShowDeleted :: Lens' ProjectsRolesList (Maybe Bool)
prlShowDeleted
  = lens _prlShowDeleted
      (\ s a -> s{_prlShowDeleted = a})

-- | OAuth bearer token.
prlBearerToken :: Lens' ProjectsRolesList (Maybe Text)
prlBearerToken
  = lens _prlBearerToken
      (\ s a -> s{_prlBearerToken = a})

-- | Optional view for the returned Role objects.
prlView :: Lens' ProjectsRolesList (Maybe Text)
prlView = lens _prlView (\ s a -> s{_prlView = a})

-- | Optional pagination token returned in an earlier ListRolesResponse.
prlPageToken :: Lens' ProjectsRolesList (Maybe Text)
prlPageToken
  = lens _prlPageToken (\ s a -> s{_prlPageToken = a})

-- | Optional limit on the number of roles to include in the response.
prlPageSize :: Lens' ProjectsRolesList (Maybe Int32)
prlPageSize
  = lens _prlPageSize (\ s a -> s{_prlPageSize = a}) .
      mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
prlFields :: Lens' ProjectsRolesList (Maybe Text)
prlFields
  = lens _prlFields (\ s a -> s{_prlFields = a})

-- | JSONP
prlCallback :: Lens' ProjectsRolesList (Maybe Text)
prlCallback
  = lens _prlCallback (\ s a -> s{_prlCallback = a})

instance GoogleRequest ProjectsRolesList where
        type Rs ProjectsRolesList = ListRolesResponse
        type Scopes ProjectsRolesList =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsRolesList'{..}
          = go _prlParent _prlXgafv _prlUploadProtocol
              (Just _prlPp)
              _prlAccessToken
              _prlUploadType
              _prlShowDeleted
              _prlBearerToken
              _prlView
              _prlPageToken
              _prlPageSize
              _prlCallback
              _prlFields
              (Just AltJSON)
              iAMService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsRolesListResource)
                      mempty
