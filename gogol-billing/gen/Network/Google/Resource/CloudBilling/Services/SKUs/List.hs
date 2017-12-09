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
-- Module      : Network.Google.Resource.CloudBilling.Services.SKUs.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all publicly available SKUs for a given cloud service.
--
-- /See:/ <https://cloud.google.com/billing/ Google Cloud Billing API Reference> for @cloudbilling.services.skus.list@.
module Network.Google.Resource.CloudBilling.Services.SKUs.List
    (
    -- * REST Resource
      ServicesSKUsListResource

    -- * Creating a Request
    , servicesSKUsList
    , ServicesSKUsList

    -- * Request Lenses
    , sskulParent
    , sskulXgafv
    , sskulCurrencyCode
    , sskulUploadProtocol
    , sskulStartTime
    , sskulPp
    , sskulAccessToken
    , sskulUploadType
    , sskulBearerToken
    , sskulEndTime
    , sskulPageToken
    , sskulPageSize
    , sskulFields
    , sskulCallback
    ) where

import Network.Google.Billing.Types
import Network.Google.Prelude

-- | A resource alias for @cloudbilling.services.skus.list@ method which the
-- 'ServicesSKUsList' request conforms to.
type ServicesSKUsListResource =
     "v1" :>
       Capture "parent" Text :>
         "skus" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "currencyCode" Text :>
               QueryParam "upload_protocol" Text :>
                 QueryParam "startTime" DateTime' :>
                   QueryParam "pp" Bool :>
                     QueryParam "access_token" Text :>
                       QueryParam "uploadType" Text :>
                         QueryParam "bearer_token" Text :>
                           QueryParam "endTime" DateTime' :>
                             QueryParam "pageToken" Text :>
                               QueryParam "pageSize" (Textual Int32) :>
                                 QueryParam "callback" Text :>
                                   QueryParam "fields" Text :>
                                     QueryParam "alt" AltJSON :>
                                       Get '[JSON] ListSKUsResponse

-- | Lists all publicly available SKUs for a given cloud service.
--
-- /See:/ 'servicesSKUsList' smart constructor.
data ServicesSKUsList = ServicesSKUsList'
    { _sskulParent :: !Text
    , _sskulXgafv :: !(Maybe Xgafv)
    , _sskulCurrencyCode :: !(Maybe Text)
    , _sskulUploadProtocol :: !(Maybe Text)
    , _sskulStartTime :: !(Maybe DateTime')
    , _sskulPp :: !Bool
    , _sskulAccessToken :: !(Maybe Text)
    , _sskulUploadType :: !(Maybe Text)
    , _sskulBearerToken :: !(Maybe Text)
    , _sskulEndTime :: !(Maybe DateTime')
    , _sskulPageToken :: !(Maybe Text)
    , _sskulPageSize :: !(Maybe (Textual Int32))
    , _sskulFields :: !(Maybe Text)
    , _sskulCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ServicesSKUsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sskulParent'
--
-- * 'sskulXgafv'
--
-- * 'sskulCurrencyCode'
--
-- * 'sskulUploadProtocol'
--
-- * 'sskulStartTime'
--
-- * 'sskulPp'
--
-- * 'sskulAccessToken'
--
-- * 'sskulUploadType'
--
-- * 'sskulBearerToken'
--
-- * 'sskulEndTime'
--
-- * 'sskulPageToken'
--
-- * 'sskulPageSize'
--
-- * 'sskulFields'
--
-- * 'sskulCallback'
servicesSKUsList
    :: Text -- ^ 'sskulParent'
    -> ServicesSKUsList
servicesSKUsList pSskulParent_ = 
    ServicesSKUsList'
    { _sskulParent = pSskulParent_
    , _sskulXgafv = Nothing
    , _sskulCurrencyCode = Nothing
    , _sskulUploadProtocol = Nothing
    , _sskulStartTime = Nothing
    , _sskulPp = True
    , _sskulAccessToken = Nothing
    , _sskulUploadType = Nothing
    , _sskulBearerToken = Nothing
    , _sskulEndTime = Nothing
    , _sskulPageToken = Nothing
    , _sskulPageSize = Nothing
    , _sskulFields = Nothing
    , _sskulCallback = Nothing
    }

-- | The name of the service. Example: \"services\/DA34-426B-A397\"
sskulParent :: Lens' ServicesSKUsList Text
sskulParent
  = lens _sskulParent (\ s a -> s{_sskulParent = a})

-- | V1 error format.
sskulXgafv :: Lens' ServicesSKUsList (Maybe Xgafv)
sskulXgafv
  = lens _sskulXgafv (\ s a -> s{_sskulXgafv = a})

-- | The ISO 4217 currency code for the pricing info in the response proto.
-- Will use the conversion rate as of start_time. Optional. If not
-- specified USD will be used.
sskulCurrencyCode :: Lens' ServicesSKUsList (Maybe Text)
sskulCurrencyCode
  = lens _sskulCurrencyCode
      (\ s a -> s{_sskulCurrencyCode = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
sskulUploadProtocol :: Lens' ServicesSKUsList (Maybe Text)
sskulUploadProtocol
  = lens _sskulUploadProtocol
      (\ s a -> s{_sskulUploadProtocol = a})

-- | Optional inclusive start time of the time range for which the pricing
-- versions will be returned. Timestamps in the future are not allowed.
-- Maximum allowable time range is 1 month (31 days). Time range as a whole
-- is optional. If not specified, the latest pricing will be returned (up
-- to 12 hours old at most).
sskulStartTime :: Lens' ServicesSKUsList (Maybe UTCTime)
sskulStartTime
  = lens _sskulStartTime
      (\ s a -> s{_sskulStartTime = a})
      . mapping _DateTime

-- | Pretty-print response.
sskulPp :: Lens' ServicesSKUsList Bool
sskulPp = lens _sskulPp (\ s a -> s{_sskulPp = a})

-- | OAuth access token.
sskulAccessToken :: Lens' ServicesSKUsList (Maybe Text)
sskulAccessToken
  = lens _sskulAccessToken
      (\ s a -> s{_sskulAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
sskulUploadType :: Lens' ServicesSKUsList (Maybe Text)
sskulUploadType
  = lens _sskulUploadType
      (\ s a -> s{_sskulUploadType = a})

-- | OAuth bearer token.
sskulBearerToken :: Lens' ServicesSKUsList (Maybe Text)
sskulBearerToken
  = lens _sskulBearerToken
      (\ s a -> s{_sskulBearerToken = a})

-- | Optional exclusive end time of the time range for which the pricing
-- versions will be returned. Timestamps in the future are not allowed.
-- Maximum allowable time range is 1 month (31 days). Time range as a whole
-- is optional. If not specified, the latest pricing will be returned (up
-- to 12 hours old at most).
sskulEndTime :: Lens' ServicesSKUsList (Maybe UTCTime)
sskulEndTime
  = lens _sskulEndTime (\ s a -> s{_sskulEndTime = a})
      . mapping _DateTime

-- | A token identifying a page of results to return. This should be a
-- \`next_page_token\` value returned from a previous \`ListSkus\` call. If
-- unspecified, the first page of results is returned.
sskulPageToken :: Lens' ServicesSKUsList (Maybe Text)
sskulPageToken
  = lens _sskulPageToken
      (\ s a -> s{_sskulPageToken = a})

-- | Requested page size. Defaults to 5000.
sskulPageSize :: Lens' ServicesSKUsList (Maybe Int32)
sskulPageSize
  = lens _sskulPageSize
      (\ s a -> s{_sskulPageSize = a})
      . mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
sskulFields :: Lens' ServicesSKUsList (Maybe Text)
sskulFields
  = lens _sskulFields (\ s a -> s{_sskulFields = a})

-- | JSONP
sskulCallback :: Lens' ServicesSKUsList (Maybe Text)
sskulCallback
  = lens _sskulCallback
      (\ s a -> s{_sskulCallback = a})

instance GoogleRequest ServicesSKUsList where
        type Rs ServicesSKUsList = ListSKUsResponse
        type Scopes ServicesSKUsList = '[]
        requestClient ServicesSKUsList'{..}
          = go _sskulParent _sskulXgafv _sskulCurrencyCode
              _sskulUploadProtocol
              _sskulStartTime
              (Just _sskulPp)
              _sskulAccessToken
              _sskulUploadType
              _sskulBearerToken
              _sskulEndTime
              _sskulPageToken
              _sskulPageSize
              _sskulCallback
              _sskulFields
              (Just AltJSON)
              billingService
          where go
                  = buildClient
                      (Proxy :: Proxy ServicesSKUsListResource)
                      mempty
