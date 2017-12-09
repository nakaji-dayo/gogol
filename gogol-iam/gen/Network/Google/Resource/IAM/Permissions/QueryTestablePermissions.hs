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
-- Module      : Network.Google.Resource.IAM.Permissions.QueryTestablePermissions
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the permissions testable on a resource. A permission is testable
-- if it can be tested for an identity on a resource.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.permissions.queryTestablePermissions@.
module Network.Google.Resource.IAM.Permissions.QueryTestablePermissions
    (
    -- * REST Resource
      PermissionsQueryTestablePermissionsResource

    -- * Creating a Request
    , permissionsQueryTestablePermissions
    , PermissionsQueryTestablePermissions

    -- * Request Lenses
    , pqtpXgafv
    , pqtpUploadProtocol
    , pqtpPp
    , pqtpAccessToken
    , pqtpUploadType
    , pqtpPayload
    , pqtpBearerToken
    , pqtpFields
    , pqtpCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.permissions.queryTestablePermissions@ method which the
-- 'PermissionsQueryTestablePermissions' request conforms to.
type PermissionsQueryTestablePermissionsResource =
     "v1" :>
       "permissions:queryTestablePermissions" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] QueryTestablePermissionsRequest :>
                             Post '[JSON] QueryTestablePermissionsResponse

-- | Lists the permissions testable on a resource. A permission is testable
-- if it can be tested for an identity on a resource.
--
-- /See:/ 'permissionsQueryTestablePermissions' smart constructor.
data PermissionsQueryTestablePermissions = PermissionsQueryTestablePermissions'
    { _pqtpXgafv :: !(Maybe Xgafv)
    , _pqtpUploadProtocol :: !(Maybe Text)
    , _pqtpPp :: !Bool
    , _pqtpAccessToken :: !(Maybe Text)
    , _pqtpUploadType :: !(Maybe Text)
    , _pqtpPayload :: !QueryTestablePermissionsRequest
    , _pqtpBearerToken :: !(Maybe Text)
    , _pqtpFields :: !(Maybe Text)
    , _pqtpCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PermissionsQueryTestablePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pqtpXgafv'
--
-- * 'pqtpUploadProtocol'
--
-- * 'pqtpPp'
--
-- * 'pqtpAccessToken'
--
-- * 'pqtpUploadType'
--
-- * 'pqtpPayload'
--
-- * 'pqtpBearerToken'
--
-- * 'pqtpFields'
--
-- * 'pqtpCallback'
permissionsQueryTestablePermissions
    :: QueryTestablePermissionsRequest -- ^ 'pqtpPayload'
    -> PermissionsQueryTestablePermissions
permissionsQueryTestablePermissions pPqtpPayload_ = 
    PermissionsQueryTestablePermissions'
    { _pqtpXgafv = Nothing
    , _pqtpUploadProtocol = Nothing
    , _pqtpPp = True
    , _pqtpAccessToken = Nothing
    , _pqtpUploadType = Nothing
    , _pqtpPayload = pPqtpPayload_
    , _pqtpBearerToken = Nothing
    , _pqtpFields = Nothing
    , _pqtpCallback = Nothing
    }

-- | V1 error format.
pqtpXgafv :: Lens' PermissionsQueryTestablePermissions (Maybe Xgafv)
pqtpXgafv
  = lens _pqtpXgafv (\ s a -> s{_pqtpXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pqtpUploadProtocol :: Lens' PermissionsQueryTestablePermissions (Maybe Text)
pqtpUploadProtocol
  = lens _pqtpUploadProtocol
      (\ s a -> s{_pqtpUploadProtocol = a})

-- | Pretty-print response.
pqtpPp :: Lens' PermissionsQueryTestablePermissions Bool
pqtpPp = lens _pqtpPp (\ s a -> s{_pqtpPp = a})

-- | OAuth access token.
pqtpAccessToken :: Lens' PermissionsQueryTestablePermissions (Maybe Text)
pqtpAccessToken
  = lens _pqtpAccessToken
      (\ s a -> s{_pqtpAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pqtpUploadType :: Lens' PermissionsQueryTestablePermissions (Maybe Text)
pqtpUploadType
  = lens _pqtpUploadType
      (\ s a -> s{_pqtpUploadType = a})

-- | Multipart request metadata.
pqtpPayload :: Lens' PermissionsQueryTestablePermissions QueryTestablePermissionsRequest
pqtpPayload
  = lens _pqtpPayload (\ s a -> s{_pqtpPayload = a})

-- | OAuth bearer token.
pqtpBearerToken :: Lens' PermissionsQueryTestablePermissions (Maybe Text)
pqtpBearerToken
  = lens _pqtpBearerToken
      (\ s a -> s{_pqtpBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
pqtpFields :: Lens' PermissionsQueryTestablePermissions (Maybe Text)
pqtpFields
  = lens _pqtpFields (\ s a -> s{_pqtpFields = a})

-- | JSONP
pqtpCallback :: Lens' PermissionsQueryTestablePermissions (Maybe Text)
pqtpCallback
  = lens _pqtpCallback (\ s a -> s{_pqtpCallback = a})

instance GoogleRequest
         PermissionsQueryTestablePermissions where
        type Rs PermissionsQueryTestablePermissions =
             QueryTestablePermissionsResponse
        type Scopes PermissionsQueryTestablePermissions =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient
          PermissionsQueryTestablePermissions'{..}
          = go _pqtpXgafv _pqtpUploadProtocol (Just _pqtpPp)
              _pqtpAccessToken
              _pqtpUploadType
              _pqtpBearerToken
              _pqtpCallback
              _pqtpFields
              (Just AltJSON)
              _pqtpPayload
              iAMService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy PermissionsQueryTestablePermissionsResource)
                      mempty
