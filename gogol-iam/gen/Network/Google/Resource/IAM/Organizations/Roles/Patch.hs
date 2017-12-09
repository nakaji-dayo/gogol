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
-- Module      : Network.Google.Resource.IAM.Organizations.Roles.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Role definition.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.organizations.roles.patch@.
module Network.Google.Resource.IAM.Organizations.Roles.Patch
    (
    -- * REST Resource
      OrganizationsRolesPatchResource

    -- * Creating a Request
    , organizationsRolesPatch
    , OrganizationsRolesPatch

    -- * Request Lenses
    , orpXgafv
    , orpUploadProtocol
    , orpUpdateMask
    , orpPp
    , orpAccessToken
    , orpUploadType
    , orpPayload
    , orpBearerToken
    , orpName
    , orpFields
    , orpCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.organizations.roles.patch@ method which the
-- 'OrganizationsRolesPatch' request conforms to.
type OrganizationsRolesPatchResource =
     "v1" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "updateMask" FieldMask :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             ReqBody '[JSON] Role :> Patch '[JSON] Role

-- | Updates a Role definition.
--
-- /See:/ 'organizationsRolesPatch' smart constructor.
data OrganizationsRolesPatch = OrganizationsRolesPatch'
    { _orpXgafv :: !(Maybe Xgafv)
    , _orpUploadProtocol :: !(Maybe Text)
    , _orpUpdateMask :: !(Maybe FieldMask)
    , _orpPp :: !Bool
    , _orpAccessToken :: !(Maybe Text)
    , _orpUploadType :: !(Maybe Text)
    , _orpPayload :: !Role
    , _orpBearerToken :: !(Maybe Text)
    , _orpName :: !Text
    , _orpFields :: !(Maybe Text)
    , _orpCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrganizationsRolesPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orpXgafv'
--
-- * 'orpUploadProtocol'
--
-- * 'orpUpdateMask'
--
-- * 'orpPp'
--
-- * 'orpAccessToken'
--
-- * 'orpUploadType'
--
-- * 'orpPayload'
--
-- * 'orpBearerToken'
--
-- * 'orpName'
--
-- * 'orpFields'
--
-- * 'orpCallback'
organizationsRolesPatch
    :: Role -- ^ 'orpPayload'
    -> Text -- ^ 'orpName'
    -> OrganizationsRolesPatch
organizationsRolesPatch pOrpPayload_ pOrpName_ = 
    OrganizationsRolesPatch'
    { _orpXgafv = Nothing
    , _orpUploadProtocol = Nothing
    , _orpUpdateMask = Nothing
    , _orpPp = True
    , _orpAccessToken = Nothing
    , _orpUploadType = Nothing
    , _orpPayload = pOrpPayload_
    , _orpBearerToken = Nothing
    , _orpName = pOrpName_
    , _orpFields = Nothing
    , _orpCallback = Nothing
    }

-- | V1 error format.
orpXgafv :: Lens' OrganizationsRolesPatch (Maybe Xgafv)
orpXgafv = lens _orpXgafv (\ s a -> s{_orpXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
orpUploadProtocol :: Lens' OrganizationsRolesPatch (Maybe Text)
orpUploadProtocol
  = lens _orpUploadProtocol
      (\ s a -> s{_orpUploadProtocol = a})

-- | A mask describing which fields in the Role have changed.
orpUpdateMask :: Lens' OrganizationsRolesPatch (Maybe FieldMask)
orpUpdateMask
  = lens _orpUpdateMask
      (\ s a -> s{_orpUpdateMask = a})

-- | Pretty-print response.
orpPp :: Lens' OrganizationsRolesPatch Bool
orpPp = lens _orpPp (\ s a -> s{_orpPp = a})

-- | OAuth access token.
orpAccessToken :: Lens' OrganizationsRolesPatch (Maybe Text)
orpAccessToken
  = lens _orpAccessToken
      (\ s a -> s{_orpAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
orpUploadType :: Lens' OrganizationsRolesPatch (Maybe Text)
orpUploadType
  = lens _orpUploadType
      (\ s a -> s{_orpUploadType = a})

-- | Multipart request metadata.
orpPayload :: Lens' OrganizationsRolesPatch Role
orpPayload
  = lens _orpPayload (\ s a -> s{_orpPayload = a})

-- | OAuth bearer token.
orpBearerToken :: Lens' OrganizationsRolesPatch (Maybe Text)
orpBearerToken
  = lens _orpBearerToken
      (\ s a -> s{_orpBearerToken = a})

-- | The resource name of the role in one of the following formats:
-- \`roles\/{ROLE_NAME}\`
-- \`organizations\/{ORGANIZATION_ID}\/roles\/{ROLE_NAME}\`
-- \`projects\/{PROJECT_ID}\/roles\/{ROLE_NAME}\`
orpName :: Lens' OrganizationsRolesPatch Text
orpName = lens _orpName (\ s a -> s{_orpName = a})

-- | Selector specifying which fields to include in a partial response.
orpFields :: Lens' OrganizationsRolesPatch (Maybe Text)
orpFields
  = lens _orpFields (\ s a -> s{_orpFields = a})

-- | JSONP
orpCallback :: Lens' OrganizationsRolesPatch (Maybe Text)
orpCallback
  = lens _orpCallback (\ s a -> s{_orpCallback = a})

instance GoogleRequest OrganizationsRolesPatch where
        type Rs OrganizationsRolesPatch = Role
        type Scopes OrganizationsRolesPatch =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient OrganizationsRolesPatch'{..}
          = go _orpName _orpXgafv _orpUploadProtocol
              _orpUpdateMask
              (Just _orpPp)
              _orpAccessToken
              _orpUploadType
              _orpBearerToken
              _orpCallback
              _orpFields
              (Just AltJSON)
              _orpPayload
              iAMService
          where go
                  = buildClient
                      (Proxy :: Proxy OrganizationsRolesPatchResource)
                      mempty
