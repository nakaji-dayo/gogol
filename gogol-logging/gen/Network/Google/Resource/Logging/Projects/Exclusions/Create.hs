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
-- Module      : Network.Google.Resource.Logging.Projects.Exclusions.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new exclusion in a specified parent resource. Only log entries
-- belonging to that resource can be excluded. You can have up to 10
-- exclusions in a resource.
--
-- /See:/ <https://cloud.google.com/logging/docs/ Stackdriver Logging API Reference> for @logging.projects.exclusions.create@.
module Network.Google.Resource.Logging.Projects.Exclusions.Create
    (
    -- * REST Resource
      ProjectsExclusionsCreateResource

    -- * Creating a Request
    , projectsExclusionsCreate
    , ProjectsExclusionsCreate

    -- * Request Lenses
    , pecParent
    , pecXgafv
    , pecUploadProtocol
    , pecPp
    , pecAccessToken
    , pecUploadType
    , pecPayload
    , pecBearerToken
    , pecFields
    , pecCallback
    ) where

import Network.Google.Logging.Types
import Network.Google.Prelude

-- | A resource alias for @logging.projects.exclusions.create@ method which the
-- 'ProjectsExclusionsCreate' request conforms to.
type ProjectsExclusionsCreateResource =
     "v2" :>
       Capture "parent" Text :>
         "exclusions" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             ReqBody '[JSON] LogExclusion :>
                               Post '[JSON] LogExclusion

-- | Creates a new exclusion in a specified parent resource. Only log entries
-- belonging to that resource can be excluded. You can have up to 10
-- exclusions in a resource.
--
-- /See:/ 'projectsExclusionsCreate' smart constructor.
data ProjectsExclusionsCreate = ProjectsExclusionsCreate'
    { _pecParent :: !Text
    , _pecXgafv :: !(Maybe Xgafv)
    , _pecUploadProtocol :: !(Maybe Text)
    , _pecPp :: !Bool
    , _pecAccessToken :: !(Maybe Text)
    , _pecUploadType :: !(Maybe Text)
    , _pecPayload :: !LogExclusion
    , _pecBearerToken :: !(Maybe Text)
    , _pecFields :: !(Maybe Text)
    , _pecCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsExclusionsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pecParent'
--
-- * 'pecXgafv'
--
-- * 'pecUploadProtocol'
--
-- * 'pecPp'
--
-- * 'pecAccessToken'
--
-- * 'pecUploadType'
--
-- * 'pecPayload'
--
-- * 'pecBearerToken'
--
-- * 'pecFields'
--
-- * 'pecCallback'
projectsExclusionsCreate
    :: Text -- ^ 'pecParent'
    -> LogExclusion -- ^ 'pecPayload'
    -> ProjectsExclusionsCreate
projectsExclusionsCreate pPecParent_ pPecPayload_ = 
    ProjectsExclusionsCreate'
    { _pecParent = pPecParent_
    , _pecXgafv = Nothing
    , _pecUploadProtocol = Nothing
    , _pecPp = True
    , _pecAccessToken = Nothing
    , _pecUploadType = Nothing
    , _pecPayload = pPecPayload_
    , _pecBearerToken = Nothing
    , _pecFields = Nothing
    , _pecCallback = Nothing
    }

-- | Required. The parent resource in which to create the exclusion:
-- \"projects\/[PROJECT_ID]\" \"organizations\/[ORGANIZATION_ID]\"
-- \"billingAccounts\/[BILLING_ACCOUNT_ID]\" \"folders\/[FOLDER_ID]\"
-- Examples: \"projects\/my-logging-project\",
-- \"organizations\/123456789\".
pecParent :: Lens' ProjectsExclusionsCreate Text
pecParent
  = lens _pecParent (\ s a -> s{_pecParent = a})

-- | V1 error format.
pecXgafv :: Lens' ProjectsExclusionsCreate (Maybe Xgafv)
pecXgafv = lens _pecXgafv (\ s a -> s{_pecXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pecUploadProtocol :: Lens' ProjectsExclusionsCreate (Maybe Text)
pecUploadProtocol
  = lens _pecUploadProtocol
      (\ s a -> s{_pecUploadProtocol = a})

-- | Pretty-print response.
pecPp :: Lens' ProjectsExclusionsCreate Bool
pecPp = lens _pecPp (\ s a -> s{_pecPp = a})

-- | OAuth access token.
pecAccessToken :: Lens' ProjectsExclusionsCreate (Maybe Text)
pecAccessToken
  = lens _pecAccessToken
      (\ s a -> s{_pecAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pecUploadType :: Lens' ProjectsExclusionsCreate (Maybe Text)
pecUploadType
  = lens _pecUploadType
      (\ s a -> s{_pecUploadType = a})

-- | Multipart request metadata.
pecPayload :: Lens' ProjectsExclusionsCreate LogExclusion
pecPayload
  = lens _pecPayload (\ s a -> s{_pecPayload = a})

-- | OAuth bearer token.
pecBearerToken :: Lens' ProjectsExclusionsCreate (Maybe Text)
pecBearerToken
  = lens _pecBearerToken
      (\ s a -> s{_pecBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
pecFields :: Lens' ProjectsExclusionsCreate (Maybe Text)
pecFields
  = lens _pecFields (\ s a -> s{_pecFields = a})

-- | JSONP
pecCallback :: Lens' ProjectsExclusionsCreate (Maybe Text)
pecCallback
  = lens _pecCallback (\ s a -> s{_pecCallback = a})

instance GoogleRequest ProjectsExclusionsCreate where
        type Rs ProjectsExclusionsCreate = LogExclusion
        type Scopes ProjectsExclusionsCreate =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/logging.admin"]
        requestClient ProjectsExclusionsCreate'{..}
          = go _pecParent _pecXgafv _pecUploadProtocol
              (Just _pecPp)
              _pecAccessToken
              _pecUploadType
              _pecBearerToken
              _pecCallback
              _pecFields
              (Just AltJSON)
              _pecPayload
              loggingService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsExclusionsCreateResource)
                      mempty
