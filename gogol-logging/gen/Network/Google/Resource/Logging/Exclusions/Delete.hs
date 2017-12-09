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
-- Module      : Network.Google.Resource.Logging.Exclusions.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an exclusion.
--
-- /See:/ <https://cloud.google.com/logging/docs/ Stackdriver Logging API Reference> for @logging.exclusions.delete@.
module Network.Google.Resource.Logging.Exclusions.Delete
    (
    -- * REST Resource
      ExclusionsDeleteResource

    -- * Creating a Request
    , exclusionsDelete
    , ExclusionsDelete

    -- * Request Lenses
    , edXgafv
    , edUploadProtocol
    , edPp
    , edAccessToken
    , edUploadType
    , edBearerToken
    , edName
    , edFields
    , edCallback
    ) where

import Network.Google.Logging.Types
import Network.Google.Prelude

-- | A resource alias for @logging.exclusions.delete@ method which the
-- 'ExclusionsDelete' request conforms to.
type ExclusionsDeleteResource =
     "v2" :>
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

-- | Deletes an exclusion.
--
-- /See:/ 'exclusionsDelete' smart constructor.
data ExclusionsDelete = ExclusionsDelete'
    { _edXgafv :: !(Maybe Xgafv)
    , _edUploadProtocol :: !(Maybe Text)
    , _edPp :: !Bool
    , _edAccessToken :: !(Maybe Text)
    , _edUploadType :: !(Maybe Text)
    , _edBearerToken :: !(Maybe Text)
    , _edName :: !Text
    , _edFields :: !(Maybe Text)
    , _edCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExclusionsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edXgafv'
--
-- * 'edUploadProtocol'
--
-- * 'edPp'
--
-- * 'edAccessToken'
--
-- * 'edUploadType'
--
-- * 'edBearerToken'
--
-- * 'edName'
--
-- * 'edFields'
--
-- * 'edCallback'
exclusionsDelete
    :: Text -- ^ 'edName'
    -> ExclusionsDelete
exclusionsDelete pEdName_ = 
    ExclusionsDelete'
    { _edXgafv = Nothing
    , _edUploadProtocol = Nothing
    , _edPp = True
    , _edAccessToken = Nothing
    , _edUploadType = Nothing
    , _edBearerToken = Nothing
    , _edName = pEdName_
    , _edFields = Nothing
    , _edCallback = Nothing
    }

-- | V1 error format.
edXgafv :: Lens' ExclusionsDelete (Maybe Xgafv)
edXgafv = lens _edXgafv (\ s a -> s{_edXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
edUploadProtocol :: Lens' ExclusionsDelete (Maybe Text)
edUploadProtocol
  = lens _edUploadProtocol
      (\ s a -> s{_edUploadProtocol = a})

-- | Pretty-print response.
edPp :: Lens' ExclusionsDelete Bool
edPp = lens _edPp (\ s a -> s{_edPp = a})

-- | OAuth access token.
edAccessToken :: Lens' ExclusionsDelete (Maybe Text)
edAccessToken
  = lens _edAccessToken
      (\ s a -> s{_edAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
edUploadType :: Lens' ExclusionsDelete (Maybe Text)
edUploadType
  = lens _edUploadType (\ s a -> s{_edUploadType = a})

-- | OAuth bearer token.
edBearerToken :: Lens' ExclusionsDelete (Maybe Text)
edBearerToken
  = lens _edBearerToken
      (\ s a -> s{_edBearerToken = a})

-- | Required. The resource name of an existing exclusion to delete:
-- \"projects\/[PROJECT_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"organizations\/[ORGANIZATION_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"billingAccounts\/[BILLING_ACCOUNT_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"folders\/[FOLDER_ID]\/exclusions\/[EXCLUSION_ID]\" Example:
-- \"projects\/my-project-id\/exclusions\/my-exclusion-id\".
edName :: Lens' ExclusionsDelete Text
edName = lens _edName (\ s a -> s{_edName = a})

-- | Selector specifying which fields to include in a partial response.
edFields :: Lens' ExclusionsDelete (Maybe Text)
edFields = lens _edFields (\ s a -> s{_edFields = a})

-- | JSONP
edCallback :: Lens' ExclusionsDelete (Maybe Text)
edCallback
  = lens _edCallback (\ s a -> s{_edCallback = a})

instance GoogleRequest ExclusionsDelete where
        type Rs ExclusionsDelete = Empty
        type Scopes ExclusionsDelete =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/logging.admin"]
        requestClient ExclusionsDelete'{..}
          = go _edName _edXgafv _edUploadProtocol (Just _edPp)
              _edAccessToken
              _edUploadType
              _edBearerToken
              _edCallback
              _edFields
              (Just AltJSON)
              loggingService
          where go
                  = buildClient
                      (Proxy :: Proxy ExclusionsDeleteResource)
                      mempty
