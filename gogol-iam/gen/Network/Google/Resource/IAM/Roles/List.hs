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
-- Module      : Network.Google.Resource.IAM.Roles.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Roles defined on a resource.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.roles.list@.
module Network.Google.Resource.IAM.Roles.List
    (
    -- * REST Resource
      RolesListResource

    -- * Creating a Request
    , rolesList
    , RolesList

    -- * Request Lenses
    , rlParent
    , rlXgafv
    , rlUploadProtocol
    , rlPp
    , rlAccessToken
    , rlUploadType
    , rlShowDeleted
    , rlBearerToken
    , rlView
    , rlPageToken
    , rlPageSize
    , rlFields
    , rlCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.roles.list@ method which the
-- 'RolesList' request conforms to.
type RolesListResource =
     "v1" :>
       "roles" :>
         QueryParam "parent" Text :>
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
-- /See:/ 'rolesList' smart constructor.
data RolesList = RolesList'
    { _rlParent :: !(Maybe Text)
    , _rlXgafv :: !(Maybe Xgafv)
    , _rlUploadProtocol :: !(Maybe Text)
    , _rlPp :: !Bool
    , _rlAccessToken :: !(Maybe Text)
    , _rlUploadType :: !(Maybe Text)
    , _rlShowDeleted :: !(Maybe Bool)
    , _rlBearerToken :: !(Maybe Text)
    , _rlView :: !(Maybe Text)
    , _rlPageToken :: !(Maybe Text)
    , _rlPageSize :: !(Maybe (Textual Int32))
    , _rlFields :: !(Maybe Text)
    , _rlCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RolesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlParent'
--
-- * 'rlXgafv'
--
-- * 'rlUploadProtocol'
--
-- * 'rlPp'
--
-- * 'rlAccessToken'
--
-- * 'rlUploadType'
--
-- * 'rlShowDeleted'
--
-- * 'rlBearerToken'
--
-- * 'rlView'
--
-- * 'rlPageToken'
--
-- * 'rlPageSize'
--
-- * 'rlFields'
--
-- * 'rlCallback'
rolesList
    :: RolesList
rolesList = 
    RolesList'
    { _rlParent = Nothing
    , _rlXgafv = Nothing
    , _rlUploadProtocol = Nothing
    , _rlPp = True
    , _rlAccessToken = Nothing
    , _rlUploadType = Nothing
    , _rlShowDeleted = Nothing
    , _rlBearerToken = Nothing
    , _rlView = Nothing
    , _rlPageToken = Nothing
    , _rlPageSize = Nothing
    , _rlFields = Nothing
    , _rlCallback = Nothing
    }

-- | The resource name of the parent resource in one of the following
-- formats: \`\` (empty string) -- this refers to curated roles.
-- \`organizations\/{ORGANIZATION_ID}\` \`projects\/{PROJECT_ID}\`
rlParent :: Lens' RolesList (Maybe Text)
rlParent = lens _rlParent (\ s a -> s{_rlParent = a})

-- | V1 error format.
rlXgafv :: Lens' RolesList (Maybe Xgafv)
rlXgafv = lens _rlXgafv (\ s a -> s{_rlXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
rlUploadProtocol :: Lens' RolesList (Maybe Text)
rlUploadProtocol
  = lens _rlUploadProtocol
      (\ s a -> s{_rlUploadProtocol = a})

-- | Pretty-print response.
rlPp :: Lens' RolesList Bool
rlPp = lens _rlPp (\ s a -> s{_rlPp = a})

-- | OAuth access token.
rlAccessToken :: Lens' RolesList (Maybe Text)
rlAccessToken
  = lens _rlAccessToken
      (\ s a -> s{_rlAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
rlUploadType :: Lens' RolesList (Maybe Text)
rlUploadType
  = lens _rlUploadType (\ s a -> s{_rlUploadType = a})

-- | Include Roles that have been deleted.
rlShowDeleted :: Lens' RolesList (Maybe Bool)
rlShowDeleted
  = lens _rlShowDeleted
      (\ s a -> s{_rlShowDeleted = a})

-- | OAuth bearer token.
rlBearerToken :: Lens' RolesList (Maybe Text)
rlBearerToken
  = lens _rlBearerToken
      (\ s a -> s{_rlBearerToken = a})

-- | Optional view for the returned Role objects.
rlView :: Lens' RolesList (Maybe Text)
rlView = lens _rlView (\ s a -> s{_rlView = a})

-- | Optional pagination token returned in an earlier ListRolesResponse.
rlPageToken :: Lens' RolesList (Maybe Text)
rlPageToken
  = lens _rlPageToken (\ s a -> s{_rlPageToken = a})

-- | Optional limit on the number of roles to include in the response.
rlPageSize :: Lens' RolesList (Maybe Int32)
rlPageSize
  = lens _rlPageSize (\ s a -> s{_rlPageSize = a}) .
      mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
rlFields :: Lens' RolesList (Maybe Text)
rlFields = lens _rlFields (\ s a -> s{_rlFields = a})

-- | JSONP
rlCallback :: Lens' RolesList (Maybe Text)
rlCallback
  = lens _rlCallback (\ s a -> s{_rlCallback = a})

instance GoogleRequest RolesList where
        type Rs RolesList = ListRolesResponse
        type Scopes RolesList =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient RolesList'{..}
          = go _rlParent _rlXgafv _rlUploadProtocol
              (Just _rlPp)
              _rlAccessToken
              _rlUploadType
              _rlShowDeleted
              _rlBearerToken
              _rlView
              _rlPageToken
              _rlPageSize
              _rlCallback
              _rlFields
              (Just AltJSON)
              iAMService
          where go
                  = buildClient (Proxy :: Proxy RolesListResource)
                      mempty
