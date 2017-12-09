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
-- Module      : Network.Google.Resource.CloudBilling.Services.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all public cloud services.
--
-- /See:/ <https://cloud.google.com/billing/ Google Cloud Billing API Reference> for @cloudbilling.services.list@.
module Network.Google.Resource.CloudBilling.Services.List
    (
    -- * REST Resource
      ServicesListResource

    -- * Creating a Request
    , servicesList
    , ServicesList

    -- * Request Lenses
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

import Network.Google.Billing.Types
import Network.Google.Prelude

-- | A resource alias for @cloudbilling.services.list@ method which the
-- 'ServicesList' request conforms to.
type ServicesListResource =
     "v1" :>
       "services" :>
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
                               Get '[JSON] ListServicesResponse

-- | Lists all public cloud services.
--
-- /See:/ 'servicesList' smart constructor.
data ServicesList = ServicesList'
    { _slXgafv :: !(Maybe Xgafv)
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

-- | Creates a value of 'ServicesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
servicesList
    :: ServicesList
servicesList = 
    ServicesList'
    { _slXgafv = Nothing
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

-- | V1 error format.
slXgafv :: Lens' ServicesList (Maybe Xgafv)
slXgafv = lens _slXgafv (\ s a -> s{_slXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
slUploadProtocol :: Lens' ServicesList (Maybe Text)
slUploadProtocol
  = lens _slUploadProtocol
      (\ s a -> s{_slUploadProtocol = a})

-- | Pretty-print response.
slPp :: Lens' ServicesList Bool
slPp = lens _slPp (\ s a -> s{_slPp = a})

-- | OAuth access token.
slAccessToken :: Lens' ServicesList (Maybe Text)
slAccessToken
  = lens _slAccessToken
      (\ s a -> s{_slAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
slUploadType :: Lens' ServicesList (Maybe Text)
slUploadType
  = lens _slUploadType (\ s a -> s{_slUploadType = a})

-- | OAuth bearer token.
slBearerToken :: Lens' ServicesList (Maybe Text)
slBearerToken
  = lens _slBearerToken
      (\ s a -> s{_slBearerToken = a})

-- | A token identifying a page of results to return. This should be a
-- \`next_page_token\` value returned from a previous \`ListServices\`
-- call. If unspecified, the first page of results is returned.
slPageToken :: Lens' ServicesList (Maybe Text)
slPageToken
  = lens _slPageToken (\ s a -> s{_slPageToken = a})

-- | Requested page size. Defaults to 5000.
slPageSize :: Lens' ServicesList (Maybe Int32)
slPageSize
  = lens _slPageSize (\ s a -> s{_slPageSize = a}) .
      mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
slFields :: Lens' ServicesList (Maybe Text)
slFields = lens _slFields (\ s a -> s{_slFields = a})

-- | JSONP
slCallback :: Lens' ServicesList (Maybe Text)
slCallback
  = lens _slCallback (\ s a -> s{_slCallback = a})

instance GoogleRequest ServicesList where
        type Rs ServicesList = ListServicesResponse
        type Scopes ServicesList = '[]
        requestClient ServicesList'{..}
          = go _slXgafv _slUploadProtocol (Just _slPp)
              _slAccessToken
              _slUploadType
              _slBearerToken
              _slPageToken
              _slPageSize
              _slCallback
              _slFields
              (Just AltJSON)
              billingService
          where go
                  = buildClient (Proxy :: Proxy ServicesListResource)
                      mempty
