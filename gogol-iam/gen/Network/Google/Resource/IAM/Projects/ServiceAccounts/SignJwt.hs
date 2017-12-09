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
-- Module      : Network.Google.Resource.IAM.Projects.ServiceAccounts.SignJwt
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signs a JWT using a service account\'s system-managed private key. If no
-- expiry time (\`exp\`) is provided in the \`SignJwtRequest\`, IAM sets an
-- an expiry time of one hour by default. If you request an expiry time of
-- more than one hour, the request will fail.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference> for @iam.projects.serviceAccounts.signJwt@.
module Network.Google.Resource.IAM.Projects.ServiceAccounts.SignJwt
    (
    -- * REST Resource
      ProjectsServiceAccountsSignJwtResource

    -- * Creating a Request
    , projectsServiceAccountsSignJwt
    , ProjectsServiceAccountsSignJwt

    -- * Request Lenses
    , psasjXgafv
    , psasjUploadProtocol
    , psasjPp
    , psasjAccessToken
    , psasjUploadType
    , psasjPayload
    , psasjBearerToken
    , psasjName
    , psasjFields
    , psasjCallback
    ) where

import Network.Google.IAM.Types
import Network.Google.Prelude

-- | A resource alias for @iam.projects.serviceAccounts.signJwt@ method which the
-- 'ProjectsServiceAccountsSignJwt' request conforms to.
type ProjectsServiceAccountsSignJwtResource =
     "v1" :>
       CaptureMode "name" "signJwt" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] SignJwtRequest :>
                             Post '[JSON] SignJwtResponse

-- | Signs a JWT using a service account\'s system-managed private key. If no
-- expiry time (\`exp\`) is provided in the \`SignJwtRequest\`, IAM sets an
-- an expiry time of one hour by default. If you request an expiry time of
-- more than one hour, the request will fail.
--
-- /See:/ 'projectsServiceAccountsSignJwt' smart constructor.
data ProjectsServiceAccountsSignJwt = ProjectsServiceAccountsSignJwt'
    { _psasjXgafv :: !(Maybe Xgafv)
    , _psasjUploadProtocol :: !(Maybe Text)
    , _psasjPp :: !Bool
    , _psasjAccessToken :: !(Maybe Text)
    , _psasjUploadType :: !(Maybe Text)
    , _psasjPayload :: !SignJwtRequest
    , _psasjBearerToken :: !(Maybe Text)
    , _psasjName :: !Text
    , _psasjFields :: !(Maybe Text)
    , _psasjCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsServiceAccountsSignJwt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psasjXgafv'
--
-- * 'psasjUploadProtocol'
--
-- * 'psasjPp'
--
-- * 'psasjAccessToken'
--
-- * 'psasjUploadType'
--
-- * 'psasjPayload'
--
-- * 'psasjBearerToken'
--
-- * 'psasjName'
--
-- * 'psasjFields'
--
-- * 'psasjCallback'
projectsServiceAccountsSignJwt
    :: SignJwtRequest -- ^ 'psasjPayload'
    -> Text -- ^ 'psasjName'
    -> ProjectsServiceAccountsSignJwt
projectsServiceAccountsSignJwt pPsasjPayload_ pPsasjName_ = 
    ProjectsServiceAccountsSignJwt'
    { _psasjXgafv = Nothing
    , _psasjUploadProtocol = Nothing
    , _psasjPp = True
    , _psasjAccessToken = Nothing
    , _psasjUploadType = Nothing
    , _psasjPayload = pPsasjPayload_
    , _psasjBearerToken = Nothing
    , _psasjName = pPsasjName_
    , _psasjFields = Nothing
    , _psasjCallback = Nothing
    }

-- | V1 error format.
psasjXgafv :: Lens' ProjectsServiceAccountsSignJwt (Maybe Xgafv)
psasjXgafv
  = lens _psasjXgafv (\ s a -> s{_psasjXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
psasjUploadProtocol :: Lens' ProjectsServiceAccountsSignJwt (Maybe Text)
psasjUploadProtocol
  = lens _psasjUploadProtocol
      (\ s a -> s{_psasjUploadProtocol = a})

-- | Pretty-print response.
psasjPp :: Lens' ProjectsServiceAccountsSignJwt Bool
psasjPp = lens _psasjPp (\ s a -> s{_psasjPp = a})

-- | OAuth access token.
psasjAccessToken :: Lens' ProjectsServiceAccountsSignJwt (Maybe Text)
psasjAccessToken
  = lens _psasjAccessToken
      (\ s a -> s{_psasjAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
psasjUploadType :: Lens' ProjectsServiceAccountsSignJwt (Maybe Text)
psasjUploadType
  = lens _psasjUploadType
      (\ s a -> s{_psasjUploadType = a})

-- | Multipart request metadata.
psasjPayload :: Lens' ProjectsServiceAccountsSignJwt SignJwtRequest
psasjPayload
  = lens _psasjPayload (\ s a -> s{_psasjPayload = a})

-- | OAuth bearer token.
psasjBearerToken :: Lens' ProjectsServiceAccountsSignJwt (Maybe Text)
psasjBearerToken
  = lens _psasjBearerToken
      (\ s a -> s{_psasjBearerToken = a})

-- | The resource name of the service account in the following format:
-- \`projects\/{PROJECT_ID}\/serviceAccounts\/{ACCOUNT}\`. Using \`-\` as a
-- wildcard for the \`PROJECT_ID\` will infer the project from the account.
-- The \`ACCOUNT\` value can be the \`email\` address or the \`unique_id\`
-- of the service account.
psasjName :: Lens' ProjectsServiceAccountsSignJwt Text
psasjName
  = lens _psasjName (\ s a -> s{_psasjName = a})

-- | Selector specifying which fields to include in a partial response.
psasjFields :: Lens' ProjectsServiceAccountsSignJwt (Maybe Text)
psasjFields
  = lens _psasjFields (\ s a -> s{_psasjFields = a})

-- | JSONP
psasjCallback :: Lens' ProjectsServiceAccountsSignJwt (Maybe Text)
psasjCallback
  = lens _psasjCallback
      (\ s a -> s{_psasjCallback = a})

instance GoogleRequest ProjectsServiceAccountsSignJwt
         where
        type Rs ProjectsServiceAccountsSignJwt =
             SignJwtResponse
        type Scopes ProjectsServiceAccountsSignJwt =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsServiceAccountsSignJwt'{..}
          = go _psasjName _psasjXgafv _psasjUploadProtocol
              (Just _psasjPp)
              _psasjAccessToken
              _psasjUploadType
              _psasjBearerToken
              _psasjCallback
              _psasjFields
              (Just AltJSON)
              _psasjPayload
              iAMService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsServiceAccountsSignJwtResource)
                      mempty
