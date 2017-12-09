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
-- Module      : Network.Google.Resource.Datastore.Projects.Operations.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a long-running operation. This method indicates that the client
-- is no longer interested in the operation result. It does not cancel the
-- operation. If the server doesn\'t support this method, it returns
-- \`google.rpc.Code.UNIMPLEMENTED\`.
--
-- /See:/ <https://cloud.google.com/datastore/ Google Cloud Datastore API Reference> for @datastore.projects.operations.delete@.
module Network.Google.Resource.Datastore.Projects.Operations.Delete
    (
    -- * REST Resource
      ProjectsOperationsDeleteResource

    -- * Creating a Request
    , projectsOperationsDelete
    , ProjectsOperationsDelete

    -- * Request Lenses
    , podXgafv
    , podUploadProtocol
    , podPp
    , podAccessToken
    , podUploadType
    , podBearerToken
    , podName
    , podFields
    , podCallback
    ) where

import Network.Google.Datastore.Types
import Network.Google.Prelude

-- | A resource alias for @datastore.projects.operations.delete@ method which the
-- 'ProjectsOperationsDelete' request conforms to.
type ProjectsOperationsDeleteResource =
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
                         QueryParam "alt" AltJSON :> Delete '[JSON] Empty

-- | Deletes a long-running operation. This method indicates that the client
-- is no longer interested in the operation result. It does not cancel the
-- operation. If the server doesn\'t support this method, it returns
-- \`google.rpc.Code.UNIMPLEMENTED\`.
--
-- /See:/ 'projectsOperationsDelete' smart constructor.
data ProjectsOperationsDelete = ProjectsOperationsDelete'
    { _podXgafv :: !(Maybe Xgafv)
    , _podUploadProtocol :: !(Maybe Text)
    , _podPp :: !Bool
    , _podAccessToken :: !(Maybe Text)
    , _podUploadType :: !(Maybe Text)
    , _podBearerToken :: !(Maybe Text)
    , _podName :: !Text
    , _podFields :: !(Maybe Text)
    , _podCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsOperationsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'podXgafv'
--
-- * 'podUploadProtocol'
--
-- * 'podPp'
--
-- * 'podAccessToken'
--
-- * 'podUploadType'
--
-- * 'podBearerToken'
--
-- * 'podName'
--
-- * 'podFields'
--
-- * 'podCallback'
projectsOperationsDelete
    :: Text -- ^ 'podName'
    -> ProjectsOperationsDelete
projectsOperationsDelete pPodName_ = 
    ProjectsOperationsDelete'
    { _podXgafv = Nothing
    , _podUploadProtocol = Nothing
    , _podPp = True
    , _podAccessToken = Nothing
    , _podUploadType = Nothing
    , _podBearerToken = Nothing
    , _podName = pPodName_
    , _podFields = Nothing
    , _podCallback = Nothing
    }

-- | V1 error format.
podXgafv :: Lens' ProjectsOperationsDelete (Maybe Xgafv)
podXgafv = lens _podXgafv (\ s a -> s{_podXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
podUploadProtocol :: Lens' ProjectsOperationsDelete (Maybe Text)
podUploadProtocol
  = lens _podUploadProtocol
      (\ s a -> s{_podUploadProtocol = a})

-- | Pretty-print response.
podPp :: Lens' ProjectsOperationsDelete Bool
podPp = lens _podPp (\ s a -> s{_podPp = a})

-- | OAuth access token.
podAccessToken :: Lens' ProjectsOperationsDelete (Maybe Text)
podAccessToken
  = lens _podAccessToken
      (\ s a -> s{_podAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
podUploadType :: Lens' ProjectsOperationsDelete (Maybe Text)
podUploadType
  = lens _podUploadType
      (\ s a -> s{_podUploadType = a})

-- | OAuth bearer token.
podBearerToken :: Lens' ProjectsOperationsDelete (Maybe Text)
podBearerToken
  = lens _podBearerToken
      (\ s a -> s{_podBearerToken = a})

-- | The name of the operation resource to be deleted.
podName :: Lens' ProjectsOperationsDelete Text
podName = lens _podName (\ s a -> s{_podName = a})

-- | Selector specifying which fields to include in a partial response.
podFields :: Lens' ProjectsOperationsDelete (Maybe Text)
podFields
  = lens _podFields (\ s a -> s{_podFields = a})

-- | JSONP
podCallback :: Lens' ProjectsOperationsDelete (Maybe Text)
podCallback
  = lens _podCallback (\ s a -> s{_podCallback = a})

instance GoogleRequest ProjectsOperationsDelete where
        type Rs ProjectsOperationsDelete = Empty
        type Scopes ProjectsOperationsDelete =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/datastore"]
        requestClient ProjectsOperationsDelete'{..}
          = go _podName _podXgafv _podUploadProtocol
              (Just _podPp)
              _podAccessToken
              _podUploadType
              _podBearerToken
              _podCallback
              _podFields
              (Just AltJSON)
              datastoreService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsOperationsDeleteResource)
                      mempty
