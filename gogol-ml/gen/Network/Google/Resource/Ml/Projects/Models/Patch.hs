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
-- Module      : Network.Google.Resource.Ml.Projects.Models.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specific model resource. Currently the only supported fields
-- to update are \`description\` and \`default_version.name\`.
--
-- /See:/ <https://cloud.google.com/ml/ Google Cloud Machine Learning Engine Reference> for @ml.projects.models.patch@.
module Network.Google.Resource.Ml.Projects.Models.Patch
    (
    -- * REST Resource
      ProjectsModelsPatchResource

    -- * Creating a Request
    , projectsModelsPatch
    , ProjectsModelsPatch

    -- * Request Lenses
    , pmpXgafv
    , pmpUploadProtocol
    , pmpUpdateMask
    , pmpPp
    , pmpAccessToken
    , pmpUploadType
    , pmpPayload
    , pmpBearerToken
    , pmpName
    , pmpFields
    , pmpCallback
    ) where

import Network.Google.MachineLearning.Types
import Network.Google.Prelude

-- | A resource alias for @ml.projects.models.patch@ method which the
-- 'ProjectsModelsPatch' request conforms to.
type ProjectsModelsPatchResource =
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
                             ReqBody '[JSON] GoogleCloudMlV1__Model :>
                               Patch '[JSON] GoogleLongrunning__Operation

-- | Updates a specific model resource. Currently the only supported fields
-- to update are \`description\` and \`default_version.name\`.
--
-- /See:/ 'projectsModelsPatch' smart constructor.
data ProjectsModelsPatch = ProjectsModelsPatch'
    { _pmpXgafv :: !(Maybe Xgafv)
    , _pmpUploadProtocol :: !(Maybe Text)
    , _pmpUpdateMask :: !(Maybe FieldMask)
    , _pmpPp :: !Bool
    , _pmpAccessToken :: !(Maybe Text)
    , _pmpUploadType :: !(Maybe Text)
    , _pmpPayload :: !GoogleCloudMlV1__Model
    , _pmpBearerToken :: !(Maybe Text)
    , _pmpName :: !Text
    , _pmpFields :: !(Maybe Text)
    , _pmpCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsModelsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmpXgafv'
--
-- * 'pmpUploadProtocol'
--
-- * 'pmpUpdateMask'
--
-- * 'pmpPp'
--
-- * 'pmpAccessToken'
--
-- * 'pmpUploadType'
--
-- * 'pmpPayload'
--
-- * 'pmpBearerToken'
--
-- * 'pmpName'
--
-- * 'pmpFields'
--
-- * 'pmpCallback'
projectsModelsPatch
    :: GoogleCloudMlV1__Model -- ^ 'pmpPayload'
    -> Text -- ^ 'pmpName'
    -> ProjectsModelsPatch
projectsModelsPatch pPmpPayload_ pPmpName_ = 
    ProjectsModelsPatch'
    { _pmpXgafv = Nothing
    , _pmpUploadProtocol = Nothing
    , _pmpUpdateMask = Nothing
    , _pmpPp = True
    , _pmpAccessToken = Nothing
    , _pmpUploadType = Nothing
    , _pmpPayload = pPmpPayload_
    , _pmpBearerToken = Nothing
    , _pmpName = pPmpName_
    , _pmpFields = Nothing
    , _pmpCallback = Nothing
    }

-- | V1 error format.
pmpXgafv :: Lens' ProjectsModelsPatch (Maybe Xgafv)
pmpXgafv = lens _pmpXgafv (\ s a -> s{_pmpXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pmpUploadProtocol :: Lens' ProjectsModelsPatch (Maybe Text)
pmpUploadProtocol
  = lens _pmpUploadProtocol
      (\ s a -> s{_pmpUploadProtocol = a})

-- | Required. Specifies the path, relative to \`Model\`, of the field to
-- update. For example, to change the description of a model to \"foo\" and
-- set its default version to \"version_1\", the \`update_mask\` parameter
-- would be specified as \`description\`, \`default_version.name\`, and the
-- \`PATCH\` request body would specify the new value, as follows: {
-- \"description\": \"foo\", \"defaultVersion\": { \"name\":\"version_1\" }
-- } In this example, the model is blindly overwritten since no etag is
-- given. To adopt etag mechanism, include \`etag\` field in the mask, and
-- include the \`etag\` value in your model resource. Currently the
-- supported update masks are \`description\`, \`default_version.name\`,
-- \`labels\`, and \`etag\`.
pmpUpdateMask :: Lens' ProjectsModelsPatch (Maybe FieldMask)
pmpUpdateMask
  = lens _pmpUpdateMask
      (\ s a -> s{_pmpUpdateMask = a})

-- | Pretty-print response.
pmpPp :: Lens' ProjectsModelsPatch Bool
pmpPp = lens _pmpPp (\ s a -> s{_pmpPp = a})

-- | OAuth access token.
pmpAccessToken :: Lens' ProjectsModelsPatch (Maybe Text)
pmpAccessToken
  = lens _pmpAccessToken
      (\ s a -> s{_pmpAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pmpUploadType :: Lens' ProjectsModelsPatch (Maybe Text)
pmpUploadType
  = lens _pmpUploadType
      (\ s a -> s{_pmpUploadType = a})

-- | Multipart request metadata.
pmpPayload :: Lens' ProjectsModelsPatch GoogleCloudMlV1__Model
pmpPayload
  = lens _pmpPayload (\ s a -> s{_pmpPayload = a})

-- | OAuth bearer token.
pmpBearerToken :: Lens' ProjectsModelsPatch (Maybe Text)
pmpBearerToken
  = lens _pmpBearerToken
      (\ s a -> s{_pmpBearerToken = a})

-- | Required. The project name.
pmpName :: Lens' ProjectsModelsPatch Text
pmpName = lens _pmpName (\ s a -> s{_pmpName = a})

-- | Selector specifying which fields to include in a partial response.
pmpFields :: Lens' ProjectsModelsPatch (Maybe Text)
pmpFields
  = lens _pmpFields (\ s a -> s{_pmpFields = a})

-- | JSONP
pmpCallback :: Lens' ProjectsModelsPatch (Maybe Text)
pmpCallback
  = lens _pmpCallback (\ s a -> s{_pmpCallback = a})

instance GoogleRequest ProjectsModelsPatch where
        type Rs ProjectsModelsPatch =
             GoogleLongrunning__Operation
        type Scopes ProjectsModelsPatch =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient ProjectsModelsPatch'{..}
          = go _pmpName _pmpXgafv _pmpUploadProtocol
              _pmpUpdateMask
              (Just _pmpPp)
              _pmpAccessToken
              _pmpUploadType
              _pmpBearerToken
              _pmpCallback
              _pmpFields
              (Just AltJSON)
              _pmpPayload
              machineLearningService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsModelsPatchResource)
                      mempty
