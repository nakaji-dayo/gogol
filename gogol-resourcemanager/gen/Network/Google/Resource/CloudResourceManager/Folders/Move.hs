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
-- Module      : Network.Google.Resource.CloudResourceManager.Folders.Move
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves a Folder under a new resource parent. Returns an Operation which
-- can be used to track the progress of the folder move workflow. Upon
-- success the Operation.response field will be populated with the moved
-- Folder. Upon failure, a FolderOperationError categorizing the failure
-- cause will be returned - if the failure occurs synchronously then the
-- FolderOperationError will be returned via the Status.details field and
-- if it occurs asynchronously then the FolderOperation will be returned
-- via the the Operation.error field. In addition, the Operation.metadata
-- field will be populated with a FolderOperation message as an aid to
-- stateless clients. Folder moves will be rejected if they violate either
-- the naming, height or fanout constraints described in the [CreateFolder]
-- documentation. The caller must have \`resourcemanager.folders.move\`
-- permission on the folder\'s current and proposed new parent.
--
-- /See:/ <https://cloud.google.com/resource-manager Google Cloud Resource Manager API Reference> for @cloudresourcemanager.folders.move@.
module Network.Google.Resource.CloudResourceManager.Folders.Move
    (
    -- * REST Resource
      FoldersMoveResource

    -- * Creating a Request
    , foldersMove
    , FoldersMove

    -- * Request Lenses
    , fmXgafv
    , fmUploadProtocol
    , fmPp
    , fmAccessToken
    , fmUploadType
    , fmPayload
    , fmBearerToken
    , fmName
    , fmFields
    , fmCallback
    ) where

import Network.Google.Prelude
import Network.Google.ResourceManager.Types

-- | A resource alias for @cloudresourcemanager.folders.move@ method which the
-- 'FoldersMove' request conforms to.
type FoldersMoveResource =
     "v2beta1" :>
       CaptureMode "name" "move" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] MoveFolderRequest :>
                             Post '[JSON] Operation

-- | Moves a Folder under a new resource parent. Returns an Operation which
-- can be used to track the progress of the folder move workflow. Upon
-- success the Operation.response field will be populated with the moved
-- Folder. Upon failure, a FolderOperationError categorizing the failure
-- cause will be returned - if the failure occurs synchronously then the
-- FolderOperationError will be returned via the Status.details field and
-- if it occurs asynchronously then the FolderOperation will be returned
-- via the the Operation.error field. In addition, the Operation.metadata
-- field will be populated with a FolderOperation message as an aid to
-- stateless clients. Folder moves will be rejected if they violate either
-- the naming, height or fanout constraints described in the [CreateFolder]
-- documentation. The caller must have \`resourcemanager.folders.move\`
-- permission on the folder\'s current and proposed new parent.
--
-- /See:/ 'foldersMove' smart constructor.
data FoldersMove = FoldersMove'
    { _fmXgafv :: !(Maybe Xgafv)
    , _fmUploadProtocol :: !(Maybe Text)
    , _fmPp :: !Bool
    , _fmAccessToken :: !(Maybe Text)
    , _fmUploadType :: !(Maybe Text)
    , _fmPayload :: !MoveFolderRequest
    , _fmBearerToken :: !(Maybe Text)
    , _fmName :: !Text
    , _fmFields :: !(Maybe Text)
    , _fmCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FoldersMove' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmXgafv'
--
-- * 'fmUploadProtocol'
--
-- * 'fmPp'
--
-- * 'fmAccessToken'
--
-- * 'fmUploadType'
--
-- * 'fmPayload'
--
-- * 'fmBearerToken'
--
-- * 'fmName'
--
-- * 'fmFields'
--
-- * 'fmCallback'
foldersMove
    :: MoveFolderRequest -- ^ 'fmPayload'
    -> Text -- ^ 'fmName'
    -> FoldersMove
foldersMove pFmPayload_ pFmName_ = 
    FoldersMove'
    { _fmXgafv = Nothing
    , _fmUploadProtocol = Nothing
    , _fmPp = True
    , _fmAccessToken = Nothing
    , _fmUploadType = Nothing
    , _fmPayload = pFmPayload_
    , _fmBearerToken = Nothing
    , _fmName = pFmName_
    , _fmFields = Nothing
    , _fmCallback = Nothing
    }

-- | V1 error format.
fmXgafv :: Lens' FoldersMove (Maybe Xgafv)
fmXgafv = lens _fmXgafv (\ s a -> s{_fmXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
fmUploadProtocol :: Lens' FoldersMove (Maybe Text)
fmUploadProtocol
  = lens _fmUploadProtocol
      (\ s a -> s{_fmUploadProtocol = a})

-- | Pretty-print response.
fmPp :: Lens' FoldersMove Bool
fmPp = lens _fmPp (\ s a -> s{_fmPp = a})

-- | OAuth access token.
fmAccessToken :: Lens' FoldersMove (Maybe Text)
fmAccessToken
  = lens _fmAccessToken
      (\ s a -> s{_fmAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
fmUploadType :: Lens' FoldersMove (Maybe Text)
fmUploadType
  = lens _fmUploadType (\ s a -> s{_fmUploadType = a})

-- | Multipart request metadata.
fmPayload :: Lens' FoldersMove MoveFolderRequest
fmPayload
  = lens _fmPayload (\ s a -> s{_fmPayload = a})

-- | OAuth bearer token.
fmBearerToken :: Lens' FoldersMove (Maybe Text)
fmBearerToken
  = lens _fmBearerToken
      (\ s a -> s{_fmBearerToken = a})

-- | The resource name of the Folder to move. Must be of the form
-- folders\/{folder_id}
fmName :: Lens' FoldersMove Text
fmName = lens _fmName (\ s a -> s{_fmName = a})

-- | Selector specifying which fields to include in a partial response.
fmFields :: Lens' FoldersMove (Maybe Text)
fmFields = lens _fmFields (\ s a -> s{_fmFields = a})

-- | JSONP
fmCallback :: Lens' FoldersMove (Maybe Text)
fmCallback
  = lens _fmCallback (\ s a -> s{_fmCallback = a})

instance GoogleRequest FoldersMove where
        type Rs FoldersMove = Operation
        type Scopes FoldersMove =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient FoldersMove'{..}
          = go _fmName _fmXgafv _fmUploadProtocol (Just _fmPp)
              _fmAccessToken
              _fmUploadType
              _fmBearerToken
              _fmCallback
              _fmFields
              (Just AltJSON)
              _fmPayload
              resourceManagerService
          where go
                  = buildClient (Proxy :: Proxy FoldersMoveResource)
                      mempty
