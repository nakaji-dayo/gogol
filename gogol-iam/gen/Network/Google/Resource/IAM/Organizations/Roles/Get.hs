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
-- Module      : Network.Google.Resource.IAM.Organizations.Roles.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Role definition.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.organizations.roles.get@.
module Network.Google.Resource.IAM.Organizations.Roles.Get
    (
    -- * REST Resource
      OrganizationsRolesGetResource

    -- * Creating a Request
    , organizationsRolesGet
    , OrganizationsRolesGet

    -- * Request Lenses
    , orgXgafv
    , orgUploadProtocol
    , orgPp
    , orgAccessToken
    , orgUploadType
    , orgBearerToken
    , orgName
    , orgFields
    , orgCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.organizations.roles.get@ method which the
-- 'OrganizationsRolesGet' request conforms to.
type OrganizationsRolesGetResource =
     "v1" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :> Get '[JSON] Role

-- | Gets a Role definition.
--
-- /See:/ 'organizationsRolesGet' smart constructor.
data OrganizationsRolesGet = OrganizationsRolesGet'
    { _orgXgafv :: !(Maybe Xgafv)
    , _orgUploadProtocol :: !(Maybe Text)
    , _orgPp :: !Bool
    , _orgAccessToken :: !(Maybe Text)
    , _orgUploadType :: !(Maybe Text)
    , _orgBearerToken :: !(Maybe Text)
    , _orgName :: !Text
    , _orgFields :: !(Maybe Text)
    , _orgCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrganizationsRolesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orgXgafv'
--
-- * 'orgUploadProtocol'
--
-- * 'orgPp'
--
-- * 'orgAccessToken'
--
-- * 'orgUploadType'
--
-- * 'orgBearerToken'
--
-- * 'orgName'
--
-- * 'orgFields'
--
-- * 'orgCallback'
organizationsRolesGet
    :: Text -- ^ 'orgName'
    -> OrganizationsRolesGet
organizationsRolesGet pOrgName_ = 
    OrganizationsRolesGet'
    { _orgXgafv = Nothing
    , _orgUploadProtocol = Nothing
    , _orgPp = True
    , _orgAccessToken = Nothing
    , _orgUploadType = Nothing
    , _orgBearerToken = Nothing
    , _orgName = pOrgName_
    , _orgFields = Nothing
    , _orgCallback = Nothing
    }

-- | V1 error format.
orgXgafv :: Lens' OrganizationsRolesGet (Maybe Xgafv)
orgXgafv = lens _orgXgafv (\ s a -> s{_orgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
orgUploadProtocol :: Lens' OrganizationsRolesGet (Maybe Text)
orgUploadProtocol
  = lens _orgUploadProtocol
      (\ s a -> s{_orgUploadProtocol = a})

-- | Pretty-print response.
orgPp :: Lens' OrganizationsRolesGet Bool
orgPp = lens _orgPp (\ s a -> s{_orgPp = a})

-- | OAuth access token.
orgAccessToken :: Lens' OrganizationsRolesGet (Maybe Text)
orgAccessToken
  = lens _orgAccessToken
      (\ s a -> s{_orgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
orgUploadType :: Lens' OrganizationsRolesGet (Maybe Text)
orgUploadType
  = lens _orgUploadType
      (\ s a -> s{_orgUploadType = a})

-- | OAuth bearer token.
orgBearerToken :: Lens' OrganizationsRolesGet (Maybe Text)
orgBearerToken
  = lens _orgBearerToken
      (\ s a -> s{_orgBearerToken = a})

-- | The resource name of the role in one of the following formats:
-- \`roles\/{ROLE_NAME}\`
-- \`organizations\/{ORGANIZATION_ID}\/roles\/{ROLE_NAME}\`
-- \`projects\/{PROJECT_ID}\/roles\/{ROLE_NAME}\`
orgName :: Lens' OrganizationsRolesGet Text
orgName = lens _orgName (\ s a -> s{_orgName = a})

-- | Selector specifying which fields to include in a partial response.
orgFields :: Lens' OrganizationsRolesGet (Maybe Text)
orgFields
  = lens _orgFields (\ s a -> s{_orgFields = a})

-- | JSONP
orgCallback :: Lens' OrganizationsRolesGet (Maybe Text)
orgCallback
  = lens _orgCallback (\ s a -> s{_orgCallback = a})

instance GoogleRequest OrganizationsRolesGet where
        type Rs OrganizationsRolesGet = Role
        type Scopes OrganizationsRolesGet =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient OrganizationsRolesGet'{..}
          = go _orgName _orgXgafv _orgUploadProtocol
              (Just _orgPp)
              _orgAccessToken
              _orgUploadType
              _orgBearerToken
              _orgCallback
              _orgFields
              (Just AltJSON)
              iAMService
          where go
                  = buildClient
                      (Proxy :: Proxy OrganizationsRolesGetResource)
                      mempty
