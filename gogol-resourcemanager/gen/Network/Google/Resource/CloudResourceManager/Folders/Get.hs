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
-- Module      : Network.Google.Resource.CloudResourceManager.Folders.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a Folder identified by the supplied resource name. Valid
-- Folder resource names have the format \`folders\/{folder_id}\` (for
-- example, \`folders\/1234\`). The caller must have
-- \`resourcemanager.folders.get\` permission on the identified folder.
--
-- /See:/ <https://cloud.google.com/resource-manager Google Cloud Resource Manager API Reference> for @cloudresourcemanager.folders.get@.
module Network.Google.Resource.CloudResourceManager.Folders.Get
    (
    -- * REST Resource
      FoldersGetResource

    -- * Creating a Request
    , foldersGet
    , FoldersGet

    -- * Request Lenses
    , fgXgafv
    , fgUploadProtocol
    , fgPp
    , fgAccessToken
    , fgUploadType
    , fgBearerToken
    , fgName
    , fgFields
    , fgCallback
    ) where

import Network.Google.Prelude
import Network.Google.ResourceManager.Types

-- | A resource alias for @cloudresourcemanager.folders.get@ method which the
-- 'FoldersGet' request conforms to.
type FoldersGetResource =
     "v2beta1" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :> Get '[JSON] Folder

-- | Retrieves a Folder identified by the supplied resource name. Valid
-- Folder resource names have the format \`folders\/{folder_id}\` (for
-- example, \`folders\/1234\`). The caller must have
-- \`resourcemanager.folders.get\` permission on the identified folder.
--
-- /See:/ 'foldersGet' smart constructor.
data FoldersGet = FoldersGet'
    { _fgXgafv :: !(Maybe Xgafv)
    , _fgUploadProtocol :: !(Maybe Text)
    , _fgPp :: !Bool
    , _fgAccessToken :: !(Maybe Text)
    , _fgUploadType :: !(Maybe Text)
    , _fgBearerToken :: !(Maybe Text)
    , _fgName :: !Text
    , _fgFields :: !(Maybe Text)
    , _fgCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FoldersGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fgXgafv'
--
-- * 'fgUploadProtocol'
--
-- * 'fgPp'
--
-- * 'fgAccessToken'
--
-- * 'fgUploadType'
--
-- * 'fgBearerToken'
--
-- * 'fgName'
--
-- * 'fgFields'
--
-- * 'fgCallback'
foldersGet
    :: Text -- ^ 'fgName'
    -> FoldersGet
foldersGet pFgName_ = 
    FoldersGet'
    { _fgXgafv = Nothing
    , _fgUploadProtocol = Nothing
    , _fgPp = True
    , _fgAccessToken = Nothing
    , _fgUploadType = Nothing
    , _fgBearerToken = Nothing
    , _fgName = pFgName_
    , _fgFields = Nothing
    , _fgCallback = Nothing
    }

-- | V1 error format.
fgXgafv :: Lens' FoldersGet (Maybe Xgafv)
fgXgafv = lens _fgXgafv (\ s a -> s{_fgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
fgUploadProtocol :: Lens' FoldersGet (Maybe Text)
fgUploadProtocol
  = lens _fgUploadProtocol
      (\ s a -> s{_fgUploadProtocol = a})

-- | Pretty-print response.
fgPp :: Lens' FoldersGet Bool
fgPp = lens _fgPp (\ s a -> s{_fgPp = a})

-- | OAuth access token.
fgAccessToken :: Lens' FoldersGet (Maybe Text)
fgAccessToken
  = lens _fgAccessToken
      (\ s a -> s{_fgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
fgUploadType :: Lens' FoldersGet (Maybe Text)
fgUploadType
  = lens _fgUploadType (\ s a -> s{_fgUploadType = a})

-- | OAuth bearer token.
fgBearerToken :: Lens' FoldersGet (Maybe Text)
fgBearerToken
  = lens _fgBearerToken
      (\ s a -> s{_fgBearerToken = a})

-- | The resource name of the Folder to retrieve. Must be of the form
-- \`folders\/{folder_id}\`.
fgName :: Lens' FoldersGet Text
fgName = lens _fgName (\ s a -> s{_fgName = a})

-- | Selector specifying which fields to include in a partial response.
fgFields :: Lens' FoldersGet (Maybe Text)
fgFields = lens _fgFields (\ s a -> s{_fgFields = a})

-- | JSONP
fgCallback :: Lens' FoldersGet (Maybe Text)
fgCallback
  = lens _fgCallback (\ s a -> s{_fgCallback = a})

instance GoogleRequest FoldersGet where
        type Rs FoldersGet = Folder
        type Scopes FoldersGet =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/cloud-platform.read-only"]
        requestClient FoldersGet'{..}
          = go _fgName _fgXgafv _fgUploadProtocol (Just _fgPp)
              _fgAccessToken
              _fgUploadType
              _fgBearerToken
              _fgCallback
              _fgFields
              (Just AltJSON)
              resourceManagerService
          where go
                  = buildClient (Proxy :: Proxy FoldersGetResource)
                      mempty
