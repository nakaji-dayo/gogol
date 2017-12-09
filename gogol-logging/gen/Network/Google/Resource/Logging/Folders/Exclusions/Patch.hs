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
-- Module      : Network.Google.Resource.Logging.Folders.Exclusions.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes one or more properties of an existing exclusion.
--
-- /See:/ <https://cloud.google.com/logging/docs/ Stackdriver Logging API Reference> for @logging.folders.exclusions.patch@.
module Network.Google.Resource.Logging.Folders.Exclusions.Patch
    (
    -- * REST Resource
      FoldersExclusionsPatchResource

    -- * Creating a Request
    , foldersExclusionsPatch
    , FoldersExclusionsPatch

    -- * Request Lenses
    , fepXgafv
    , fepUploadProtocol
    , fepUpdateMask
    , fepPp
    , fepAccessToken
    , fepUploadType
    , fepPayload
    , fepBearerToken
    , fepName
    , fepFields
    , fepCallback
    ) where

import Network.Google.Logging.Types
import Network.Google.Prelude

-- | A resource alias for @logging.folders.exclusions.patch@ method which the
-- 'FoldersExclusionsPatch' request conforms to.
type FoldersExclusionsPatchResource =
     "v2" :>
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
                             ReqBody '[JSON] LogExclusion :>
                               Patch '[JSON] LogExclusion

-- | Changes one or more properties of an existing exclusion.
--
-- /See:/ 'foldersExclusionsPatch' smart constructor.
data FoldersExclusionsPatch = FoldersExclusionsPatch'
    { _fepXgafv :: !(Maybe Xgafv)
    , _fepUploadProtocol :: !(Maybe Text)
    , _fepUpdateMask :: !(Maybe FieldMask)
    , _fepPp :: !Bool
    , _fepAccessToken :: !(Maybe Text)
    , _fepUploadType :: !(Maybe Text)
    , _fepPayload :: !LogExclusion
    , _fepBearerToken :: !(Maybe Text)
    , _fepName :: !Text
    , _fepFields :: !(Maybe Text)
    , _fepCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FoldersExclusionsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fepXgafv'
--
-- * 'fepUploadProtocol'
--
-- * 'fepUpdateMask'
--
-- * 'fepPp'
--
-- * 'fepAccessToken'
--
-- * 'fepUploadType'
--
-- * 'fepPayload'
--
-- * 'fepBearerToken'
--
-- * 'fepName'
--
-- * 'fepFields'
--
-- * 'fepCallback'
foldersExclusionsPatch
    :: LogExclusion -- ^ 'fepPayload'
    -> Text -- ^ 'fepName'
    -> FoldersExclusionsPatch
foldersExclusionsPatch pFepPayload_ pFepName_ = 
    FoldersExclusionsPatch'
    { _fepXgafv = Nothing
    , _fepUploadProtocol = Nothing
    , _fepUpdateMask = Nothing
    , _fepPp = True
    , _fepAccessToken = Nothing
    , _fepUploadType = Nothing
    , _fepPayload = pFepPayload_
    , _fepBearerToken = Nothing
    , _fepName = pFepName_
    , _fepFields = Nothing
    , _fepCallback = Nothing
    }

-- | V1 error format.
fepXgafv :: Lens' FoldersExclusionsPatch (Maybe Xgafv)
fepXgafv = lens _fepXgafv (\ s a -> s{_fepXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
fepUploadProtocol :: Lens' FoldersExclusionsPatch (Maybe Text)
fepUploadProtocol
  = lens _fepUploadProtocol
      (\ s a -> s{_fepUploadProtocol = a})

-- | Required. A nonempty list of fields to change in the existing exclusion.
-- New values for the fields are taken from the corresponding fields in the
-- LogExclusion included in this request. Fields not mentioned in
-- update_mask are not changed and are ignored in the request.For example,
-- to change the filter and description of an exclusion, specify an
-- update_mask of \"filter,description\".
fepUpdateMask :: Lens' FoldersExclusionsPatch (Maybe FieldMask)
fepUpdateMask
  = lens _fepUpdateMask
      (\ s a -> s{_fepUpdateMask = a})

-- | Pretty-print response.
fepPp :: Lens' FoldersExclusionsPatch Bool
fepPp = lens _fepPp (\ s a -> s{_fepPp = a})

-- | OAuth access token.
fepAccessToken :: Lens' FoldersExclusionsPatch (Maybe Text)
fepAccessToken
  = lens _fepAccessToken
      (\ s a -> s{_fepAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
fepUploadType :: Lens' FoldersExclusionsPatch (Maybe Text)
fepUploadType
  = lens _fepUploadType
      (\ s a -> s{_fepUploadType = a})

-- | Multipart request metadata.
fepPayload :: Lens' FoldersExclusionsPatch LogExclusion
fepPayload
  = lens _fepPayload (\ s a -> s{_fepPayload = a})

-- | OAuth bearer token.
fepBearerToken :: Lens' FoldersExclusionsPatch (Maybe Text)
fepBearerToken
  = lens _fepBearerToken
      (\ s a -> s{_fepBearerToken = a})

-- | Required. The resource name of the exclusion to update:
-- \"projects\/[PROJECT_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"organizations\/[ORGANIZATION_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"billingAccounts\/[BILLING_ACCOUNT_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"folders\/[FOLDER_ID]\/exclusions\/[EXCLUSION_ID]\" Example:
-- \"projects\/my-project-id\/exclusions\/my-exclusion-id\".
fepName :: Lens' FoldersExclusionsPatch Text
fepName = lens _fepName (\ s a -> s{_fepName = a})

-- | Selector specifying which fields to include in a partial response.
fepFields :: Lens' FoldersExclusionsPatch (Maybe Text)
fepFields
  = lens _fepFields (\ s a -> s{_fepFields = a})

-- | JSONP
fepCallback :: Lens' FoldersExclusionsPatch (Maybe Text)
fepCallback
  = lens _fepCallback (\ s a -> s{_fepCallback = a})

instance GoogleRequest FoldersExclusionsPatch where
        type Rs FoldersExclusionsPatch = LogExclusion
        type Scopes FoldersExclusionsPatch =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/logging.admin"]
        requestClient FoldersExclusionsPatch'{..}
          = go _fepName _fepXgafv _fepUploadProtocol
              _fepUpdateMask
              (Just _fepPp)
              _fepAccessToken
              _fepUploadType
              _fepBearerToken
              _fepCallback
              _fepFields
              (Just AltJSON)
              _fepPayload
              loggingService
          where go
                  = buildClient
                      (Proxy :: Proxy FoldersExclusionsPatchResource)
                      mempty
