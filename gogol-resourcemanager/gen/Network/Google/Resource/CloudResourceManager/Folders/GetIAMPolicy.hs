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
-- Module      : Network.Google.Resource.CloudResourceManager.Folders.GetIAMPolicy
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the access control policy for a Folder. The returned policy may be
-- empty if no such policy or resource exists. The \`resource\` field
-- should be the Folder\'s resource name, e.g. \"folders\/1234\". The
-- caller must have \`resourcemanager.folders.getIamPolicy\` permission on
-- the identified folder.
--
-- /See:/ <https://cloud.google.com/resource-manager Google Cloud Resource Manager API Reference> for @cloudresourcemanager.folders.getIamPolicy@.
module Network.Google.Resource.CloudResourceManager.Folders.GetIAMPolicy
    (
    -- * REST Resource
      FoldersGetIAMPolicyResource

    -- * Creating a Request
    , foldersGetIAMPolicy
    , FoldersGetIAMPolicy

    -- * Request Lenses
    , fgipXgafv
    , fgipUploadProtocol
    , fgipPp
    , fgipAccessToken
    , fgipUploadType
    , fgipPayload
    , fgipBearerToken
    , fgipResource
    , fgipFields
    , fgipCallback
    ) where

import Network.Google.Prelude
import Network.Google.ResourceManager.Types

-- | A resource alias for @cloudresourcemanager.folders.getIamPolicy@ method which the
-- 'FoldersGetIAMPolicy' request conforms to.
type FoldersGetIAMPolicyResource =
     "v2beta1" :>
       CaptureMode "resource" "getIamPolicy" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] GetIAMPolicyRequest :>
                             Post '[JSON] Policy

-- | Gets the access control policy for a Folder. The returned policy may be
-- empty if no such policy or resource exists. The \`resource\` field
-- should be the Folder\'s resource name, e.g. \"folders\/1234\". The
-- caller must have \`resourcemanager.folders.getIamPolicy\` permission on
-- the identified folder.
--
-- /See:/ 'foldersGetIAMPolicy' smart constructor.
data FoldersGetIAMPolicy = FoldersGetIAMPolicy'
    { _fgipXgafv :: !(Maybe Xgafv)
    , _fgipUploadProtocol :: !(Maybe Text)
    , _fgipPp :: !Bool
    , _fgipAccessToken :: !(Maybe Text)
    , _fgipUploadType :: !(Maybe Text)
    , _fgipPayload :: !GetIAMPolicyRequest
    , _fgipBearerToken :: !(Maybe Text)
    , _fgipResource :: !Text
    , _fgipFields :: !(Maybe Text)
    , _fgipCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FoldersGetIAMPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fgipXgafv'
--
-- * 'fgipUploadProtocol'
--
-- * 'fgipPp'
--
-- * 'fgipAccessToken'
--
-- * 'fgipUploadType'
--
-- * 'fgipPayload'
--
-- * 'fgipBearerToken'
--
-- * 'fgipResource'
--
-- * 'fgipFields'
--
-- * 'fgipCallback'
foldersGetIAMPolicy
    :: GetIAMPolicyRequest -- ^ 'fgipPayload'
    -> Text -- ^ 'fgipResource'
    -> FoldersGetIAMPolicy
foldersGetIAMPolicy pFgipPayload_ pFgipResource_ = 
    FoldersGetIAMPolicy'
    { _fgipXgafv = Nothing
    , _fgipUploadProtocol = Nothing
    , _fgipPp = True
    , _fgipAccessToken = Nothing
    , _fgipUploadType = Nothing
    , _fgipPayload = pFgipPayload_
    , _fgipBearerToken = Nothing
    , _fgipResource = pFgipResource_
    , _fgipFields = Nothing
    , _fgipCallback = Nothing
    }

-- | V1 error format.
fgipXgafv :: Lens' FoldersGetIAMPolicy (Maybe Xgafv)
fgipXgafv
  = lens _fgipXgafv (\ s a -> s{_fgipXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
fgipUploadProtocol :: Lens' FoldersGetIAMPolicy (Maybe Text)
fgipUploadProtocol
  = lens _fgipUploadProtocol
      (\ s a -> s{_fgipUploadProtocol = a})

-- | Pretty-print response.
fgipPp :: Lens' FoldersGetIAMPolicy Bool
fgipPp = lens _fgipPp (\ s a -> s{_fgipPp = a})

-- | OAuth access token.
fgipAccessToken :: Lens' FoldersGetIAMPolicy (Maybe Text)
fgipAccessToken
  = lens _fgipAccessToken
      (\ s a -> s{_fgipAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
fgipUploadType :: Lens' FoldersGetIAMPolicy (Maybe Text)
fgipUploadType
  = lens _fgipUploadType
      (\ s a -> s{_fgipUploadType = a})

-- | Multipart request metadata.
fgipPayload :: Lens' FoldersGetIAMPolicy GetIAMPolicyRequest
fgipPayload
  = lens _fgipPayload (\ s a -> s{_fgipPayload = a})

-- | OAuth bearer token.
fgipBearerToken :: Lens' FoldersGetIAMPolicy (Maybe Text)
fgipBearerToken
  = lens _fgipBearerToken
      (\ s a -> s{_fgipBearerToken = a})

-- | REQUIRED: The resource for which the policy is being requested. See the
-- operation documentation for the appropriate value for this field.
fgipResource :: Lens' FoldersGetIAMPolicy Text
fgipResource
  = lens _fgipResource (\ s a -> s{_fgipResource = a})

-- | Selector specifying which fields to include in a partial response.
fgipFields :: Lens' FoldersGetIAMPolicy (Maybe Text)
fgipFields
  = lens _fgipFields (\ s a -> s{_fgipFields = a})

-- | JSONP
fgipCallback :: Lens' FoldersGetIAMPolicy (Maybe Text)
fgipCallback
  = lens _fgipCallback (\ s a -> s{_fgipCallback = a})

instance GoogleRequest FoldersGetIAMPolicy where
        type Rs FoldersGetIAMPolicy = Policy
        type Scopes FoldersGetIAMPolicy =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/cloud-platform.read-only"]
        requestClient FoldersGetIAMPolicy'{..}
          = go _fgipResource _fgipXgafv _fgipUploadProtocol
              (Just _fgipPp)
              _fgipAccessToken
              _fgipUploadType
              _fgipBearerToken
              _fgipCallback
              _fgipFields
              (Just AltJSON)
              _fgipPayload
              resourceManagerService
          where go
                  = buildClient
                      (Proxy :: Proxy FoldersGetIAMPolicyResource)
                      mempty
