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
-- Module      : Network.Google.Resource.Logging.Sinks.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists sinks.
--
-- /See:/ <https://cloud.google.com/logging/docs/ Stackdriver Logging API Reference> for @logging.sinks.list@.
module Network.Google.Resource.Logging.Sinks.List
    (
    -- * REST Resource
      SinksListResource

    -- * Creating a Request
    , sinksList
    , SinksList

    -- * Request Lenses
    , slParent
    , slXgafv
    , slUploadProtocol
    , slPp
    , slAccessToken
    , slUploadType
    , slBearerToken
    , slPageToken
    , slPageSize
    , slFields
    , slCallback
    ) where

import Network.Google.Logging.Types
import Network.Google.Prelude

-- | A resource alias for @logging.sinks.list@ method which the
-- 'SinksList' request conforms to.
type SinksListResource =
     "v2" :>
       Capture "parent" Text :>
         "sinks" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "pageSize" (Textual Int32) :>
                           QueryParam "callback" Text :>
                             QueryParam "fields" Text :>
                               QueryParam "alt" AltJSON :>
                                 Get '[JSON] ListSinksResponse

-- | Lists sinks.
--
-- /See:/ 'sinksList' smart constructor.
data SinksList = SinksList'
    { _slParent :: !Text
    , _slXgafv :: !(Maybe Xgafv)
    , _slUploadProtocol :: !(Maybe Text)
    , _slPp :: !Bool
    , _slAccessToken :: !(Maybe Text)
    , _slUploadType :: !(Maybe Text)
    , _slBearerToken :: !(Maybe Text)
    , _slPageToken :: !(Maybe Text)
    , _slPageSize :: !(Maybe (Textual Int32))
    , _slFields :: !(Maybe Text)
    , _slCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SinksList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slParent'
--
-- * 'slXgafv'
--
-- * 'slUploadProtocol'
--
-- * 'slPp'
--
-- * 'slAccessToken'
--
-- * 'slUploadType'
--
-- * 'slBearerToken'
--
-- * 'slPageToken'
--
-- * 'slPageSize'
--
-- * 'slFields'
--
-- * 'slCallback'
sinksList
    :: Text -- ^ 'slParent'
    -> SinksList
sinksList pSlParent_ = 
    SinksList'
    { _slParent = pSlParent_
    , _slXgafv = Nothing
    , _slUploadProtocol = Nothing
    , _slPp = True
    , _slAccessToken = Nothing
    , _slUploadType = Nothing
    , _slBearerToken = Nothing
    , _slPageToken = Nothing
    , _slPageSize = Nothing
    , _slFields = Nothing
    , _slCallback = Nothing
    }

-- | Required. The parent resource whose sinks are to be listed:
-- \"projects\/[PROJECT_ID]\" \"organizations\/[ORGANIZATION_ID]\"
-- \"billingAccounts\/[BILLING_ACCOUNT_ID]\" \"folders\/[FOLDER_ID]\"
slParent :: Lens' SinksList Text
slParent = lens _slParent (\ s a -> s{_slParent = a})

-- | V1 error format.
slXgafv :: Lens' SinksList (Maybe Xgafv)
slXgafv = lens _slXgafv (\ s a -> s{_slXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
slUploadProtocol :: Lens' SinksList (Maybe Text)
slUploadProtocol
  = lens _slUploadProtocol
      (\ s a -> s{_slUploadProtocol = a})

-- | Pretty-print response.
slPp :: Lens' SinksList Bool
slPp = lens _slPp (\ s a -> s{_slPp = a})

-- | OAuth access token.
slAccessToken :: Lens' SinksList (Maybe Text)
slAccessToken
  = lens _slAccessToken
      (\ s a -> s{_slAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
slUploadType :: Lens' SinksList (Maybe Text)
slUploadType
  = lens _slUploadType (\ s a -> s{_slUploadType = a})

-- | OAuth bearer token.
slBearerToken :: Lens' SinksList (Maybe Text)
slBearerToken
  = lens _slBearerToken
      (\ s a -> s{_slBearerToken = a})

-- | Optional. If present, then retrieve the next batch of results from the
-- preceding call to this method. pageToken must be the value of
-- nextPageToken from the previous response. The values of other method
-- parameters should be identical to those in the previous call.
slPageToken :: Lens' SinksList (Maybe Text)
slPageToken
  = lens _slPageToken (\ s a -> s{_slPageToken = a})

-- | Optional. The maximum number of results to return from this request.
-- Non-positive values are ignored. The presence of nextPageToken in the
-- response indicates that more results might be available.
slPageSize :: Lens' SinksList (Maybe Int32)
slPageSize
  = lens _slPageSize (\ s a -> s{_slPageSize = a}) .
      mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
slFields :: Lens' SinksList (Maybe Text)
slFields = lens _slFields (\ s a -> s{_slFields = a})

-- | JSONP
slCallback :: Lens' SinksList (Maybe Text)
slCallback
  = lens _slCallback (\ s a -> s{_slCallback = a})

instance GoogleRequest SinksList where
        type Rs SinksList = ListSinksResponse
        type Scopes SinksList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/cloud-platform.read-only",
               "https://www.googleapis.com/auth/logging.admin",
               "https://www.googleapis.com/auth/logging.read"]
        requestClient SinksList'{..}
          = go _slParent _slXgafv _slUploadProtocol
              (Just _slPp)
              _slAccessToken
              _slUploadType
              _slBearerToken
              _slPageToken
              _slPageSize
              _slCallback
              _slFields
              (Just AltJSON)
              loggingService
          where go
                  = buildClient (Proxy :: Proxy SinksListResource)
                      mempty
