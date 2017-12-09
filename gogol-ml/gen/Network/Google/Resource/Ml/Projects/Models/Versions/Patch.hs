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
-- Module      : Network.Google.Resource.Ml.Projects.Models.Versions.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified Version resource. Currently the only supported
-- field to update is \`description\`.
--
-- /See:/ <https://cloud.google.com/ml/ Google Cloud Machine Learning Engine Reference> for @ml.projects.models.versions.patch@.
module Network.Google.Resource.Ml.Projects.Models.Versions.Patch
    (
    -- * REST Resource
      ProjectsModelsVersionsPatchResource

    -- * Creating a Request
    , projectsModelsVersionsPatch
    , ProjectsModelsVersionsPatch

    -- * Request Lenses
    , pmvpXgafv
    , pmvpUploadProtocol
    , pmvpUpdateMask
    , pmvpPp
    , pmvpAccessToken
    , pmvpUploadType
    , pmvpPayload
    , pmvpBearerToken
    , pmvpName
    , pmvpFields
    , pmvpCallback
    ) where

import Network.Google.MachineLearning.Types
import Network.Google.Prelude

-- | A resource alias for @ml.projects.models.versions.patch@ method which the
-- 'ProjectsModelsVersionsPatch' request conforms to.
type ProjectsModelsVersionsPatchResource =
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
                             ReqBody '[JSON] GoogleCloudMlV1__Version :>
                               Patch '[JSON] GoogleLongrunning__Operation

-- | Updates the specified Version resource. Currently the only supported
-- field to update is \`description\`.
--
-- /See:/ 'projectsModelsVersionsPatch' smart constructor.
data ProjectsModelsVersionsPatch = ProjectsModelsVersionsPatch'
    { _pmvpXgafv :: !(Maybe Xgafv)
    , _pmvpUploadProtocol :: !(Maybe Text)
    , _pmvpUpdateMask :: !(Maybe FieldMask)
    , _pmvpPp :: !Bool
    , _pmvpAccessToken :: !(Maybe Text)
    , _pmvpUploadType :: !(Maybe Text)
    , _pmvpPayload :: !GoogleCloudMlV1__Version
    , _pmvpBearerToken :: !(Maybe Text)
    , _pmvpName :: !Text
    , _pmvpFields :: !(Maybe Text)
    , _pmvpCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsModelsVersionsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmvpXgafv'
--
-- * 'pmvpUploadProtocol'
--
-- * 'pmvpUpdateMask'
--
-- * 'pmvpPp'
--
-- * 'pmvpAccessToken'
--
-- * 'pmvpUploadType'
--
-- * 'pmvpPayload'
--
-- * 'pmvpBearerToken'
--
-- * 'pmvpName'
--
-- * 'pmvpFields'
--
-- * 'pmvpCallback'
projectsModelsVersionsPatch
    :: GoogleCloudMlV1__Version -- ^ 'pmvpPayload'
    -> Text -- ^ 'pmvpName'
    -> ProjectsModelsVersionsPatch
projectsModelsVersionsPatch pPmvpPayload_ pPmvpName_ = 
    ProjectsModelsVersionsPatch'
    { _pmvpXgafv = Nothing
    , _pmvpUploadProtocol = Nothing
    , _pmvpUpdateMask = Nothing
    , _pmvpPp = True
    , _pmvpAccessToken = Nothing
    , _pmvpUploadType = Nothing
    , _pmvpPayload = pPmvpPayload_
    , _pmvpBearerToken = Nothing
    , _pmvpName = pPmvpName_
    , _pmvpFields = Nothing
    , _pmvpCallback = Nothing
    }

-- | V1 error format.
pmvpXgafv :: Lens' ProjectsModelsVersionsPatch (Maybe Xgafv)
pmvpXgafv
  = lens _pmvpXgafv (\ s a -> s{_pmvpXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pmvpUploadProtocol :: Lens' ProjectsModelsVersionsPatch (Maybe Text)
pmvpUploadProtocol
  = lens _pmvpUploadProtocol
      (\ s a -> s{_pmvpUploadProtocol = a})

-- | Required. Specifies the path, relative to \`Version\`, of the field to
-- update. Must be present and non-empty. For example, to change the
-- description of a version to \"foo\", the \`update_mask\` parameter would
-- be specified as \`description\`, and the \`PATCH\` request body would
-- specify the new value, as follows: { \"description\": \"foo\" } In this
-- example, the version is blindly overwritten since no etag is given. To
-- adopt etag mechanism, include \`etag\` field in the mask, and include
-- the \`etag\` value in your version resource. Currently the only
-- supported update masks are \`description\`, \`labels\`, and \`etag\`.
pmvpUpdateMask :: Lens' ProjectsModelsVersionsPatch (Maybe FieldMask)
pmvpUpdateMask
  = lens _pmvpUpdateMask
      (\ s a -> s{_pmvpUpdateMask = a})

-- | Pretty-print response.
pmvpPp :: Lens' ProjectsModelsVersionsPatch Bool
pmvpPp = lens _pmvpPp (\ s a -> s{_pmvpPp = a})

-- | OAuth access token.
pmvpAccessToken :: Lens' ProjectsModelsVersionsPatch (Maybe Text)
pmvpAccessToken
  = lens _pmvpAccessToken
      (\ s a -> s{_pmvpAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pmvpUploadType :: Lens' ProjectsModelsVersionsPatch (Maybe Text)
pmvpUploadType
  = lens _pmvpUploadType
      (\ s a -> s{_pmvpUploadType = a})

-- | Multipart request metadata.
pmvpPayload :: Lens' ProjectsModelsVersionsPatch GoogleCloudMlV1__Version
pmvpPayload
  = lens _pmvpPayload (\ s a -> s{_pmvpPayload = a})

-- | OAuth bearer token.
pmvpBearerToken :: Lens' ProjectsModelsVersionsPatch (Maybe Text)
pmvpBearerToken
  = lens _pmvpBearerToken
      (\ s a -> s{_pmvpBearerToken = a})

-- | Required. The name of the model.
pmvpName :: Lens' ProjectsModelsVersionsPatch Text
pmvpName = lens _pmvpName (\ s a -> s{_pmvpName = a})

-- | Selector specifying which fields to include in a partial response.
pmvpFields :: Lens' ProjectsModelsVersionsPatch (Maybe Text)
pmvpFields
  = lens _pmvpFields (\ s a -> s{_pmvpFields = a})

-- | JSONP
pmvpCallback :: Lens' ProjectsModelsVersionsPatch (Maybe Text)
pmvpCallback
  = lens _pmvpCallback (\ s a -> s{_pmvpCallback = a})

instance GoogleRequest ProjectsModelsVersionsPatch
         where
        type Rs ProjectsModelsVersionsPatch =
             GoogleLongrunning__Operation
        type Scopes ProjectsModelsVersionsPatch =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsModelsVersionsPatch'{..}
          = go _pmvpName _pmvpXgafv _pmvpUploadProtocol
              _pmvpUpdateMask
              (Just _pmvpPp)
              _pmvpAccessToken
              _pmvpUploadType
              _pmvpBearerToken
              _pmvpCallback
              _pmvpFields
              (Just AltJSON)
              _pmvpPayload
              machineLearningService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsModelsVersionsPatchResource)
                      mempty
