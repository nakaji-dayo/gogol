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
-- Module      : Network.Google.Resource.Monitoring.UptimeCheckIPs.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of IPs that checkers run from
--
-- /See:/ <https://cloud.google.com/monitoring/api/ Stackdriver Monitoring API Reference> for @monitoring.uptimeCheckIps.list@.
module Network.Google.Resource.Monitoring.UptimeCheckIPs.List
    (
    -- * REST Resource
      UptimeCheckIPsListResource

    -- * Creating a Request
    , uptimeCheckIPsList
    , UptimeCheckIPsList

    -- * Request Lenses
    , ucilXgafv
    , ucilUploadProtocol
    , ucilPp
    , ucilAccessToken
    , ucilUploadType
    , ucilBearerToken
    , ucilPageToken
    , ucilPageSize
    , ucilFields
    , ucilCallback
    ) where

import Network.Google.Monitoring.Types
import Network.Google.Prelude

-- | A resource alias for @monitoring.uptimeCheckIps.list@ method which the
-- 'UptimeCheckIPsList' request conforms to.
type UptimeCheckIPsListResource =
     "v3" :>
       "uptimeCheckIps" :>
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
                               Get '[JSON] ListUptimeCheckIPsResponse

-- | Returns the list of IPs that checkers run from
--
-- /See:/ 'uptimeCheckIPsList' smart constructor.
data UptimeCheckIPsList = UptimeCheckIPsList'
    { _ucilXgafv :: !(Maybe Xgafv)
    , _ucilUploadProtocol :: !(Maybe Text)
    , _ucilPp :: !Bool
    , _ucilAccessToken :: !(Maybe Text)
    , _ucilUploadType :: !(Maybe Text)
    , _ucilBearerToken :: !(Maybe Text)
    , _ucilPageToken :: !(Maybe Text)
    , _ucilPageSize :: !(Maybe (Textual Int32))
    , _ucilFields :: !(Maybe Text)
    , _ucilCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UptimeCheckIPsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucilXgafv'
--
-- * 'ucilUploadProtocol'
--
-- * 'ucilPp'
--
-- * 'ucilAccessToken'
--
-- * 'ucilUploadType'
--
-- * 'ucilBearerToken'
--
-- * 'ucilPageToken'
--
-- * 'ucilPageSize'
--
-- * 'ucilFields'
--
-- * 'ucilCallback'
uptimeCheckIPsList
    :: UptimeCheckIPsList
uptimeCheckIPsList = 
    UptimeCheckIPsList'
    { _ucilXgafv = Nothing
    , _ucilUploadProtocol = Nothing
    , _ucilPp = True
    , _ucilAccessToken = Nothing
    , _ucilUploadType = Nothing
    , _ucilBearerToken = Nothing
    , _ucilPageToken = Nothing
    , _ucilPageSize = Nothing
    , _ucilFields = Nothing
    , _ucilCallback = Nothing
    }

-- | V1 error format.
ucilXgafv :: Lens' UptimeCheckIPsList (Maybe Xgafv)
ucilXgafv
  = lens _ucilXgafv (\ s a -> s{_ucilXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
ucilUploadProtocol :: Lens' UptimeCheckIPsList (Maybe Text)
ucilUploadProtocol
  = lens _ucilUploadProtocol
      (\ s a -> s{_ucilUploadProtocol = a})

-- | Pretty-print response.
ucilPp :: Lens' UptimeCheckIPsList Bool
ucilPp = lens _ucilPp (\ s a -> s{_ucilPp = a})

-- | OAuth access token.
ucilAccessToken :: Lens' UptimeCheckIPsList (Maybe Text)
ucilAccessToken
  = lens _ucilAccessToken
      (\ s a -> s{_ucilAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
ucilUploadType :: Lens' UptimeCheckIPsList (Maybe Text)
ucilUploadType
  = lens _ucilUploadType
      (\ s a -> s{_ucilUploadType = a})

-- | OAuth bearer token.
ucilBearerToken :: Lens' UptimeCheckIPsList (Maybe Text)
ucilBearerToken
  = lens _ucilBearerToken
      (\ s a -> s{_ucilBearerToken = a})

-- | If this field is not empty then it must contain the nextPageToken value
-- returned by a previous call to this method. Using this field causes the
-- method to return more results from the previous method call. NOTE: this
-- field is not yet implemented
ucilPageToken :: Lens' UptimeCheckIPsList (Maybe Text)
ucilPageToken
  = lens _ucilPageToken
      (\ s a -> s{_ucilPageToken = a})

-- | The maximum number of results to return in a single response. The server
-- may further constrain the maximum number of results returned in a single
-- page. If the page_size is \<=0, the server will decide the number of
-- results to be returned. NOTE: this field is not yet implemented
ucilPageSize :: Lens' UptimeCheckIPsList (Maybe Int32)
ucilPageSize
  = lens _ucilPageSize (\ s a -> s{_ucilPageSize = a})
      . mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
ucilFields :: Lens' UptimeCheckIPsList (Maybe Text)
ucilFields
  = lens _ucilFields (\ s a -> s{_ucilFields = a})

-- | JSONP
ucilCallback :: Lens' UptimeCheckIPsList (Maybe Text)
ucilCallback
  = lens _ucilCallback (\ s a -> s{_ucilCallback = a})

instance GoogleRequest UptimeCheckIPsList where
        type Rs UptimeCheckIPsList =
             ListUptimeCheckIPsResponse
        type Scopes UptimeCheckIPsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/monitoring",
               "https://www.googleapis.com/auth/monitoring.read"]
        requestClient UptimeCheckIPsList'{..}
          = go _ucilXgafv _ucilUploadProtocol (Just _ucilPp)
              _ucilAccessToken
              _ucilUploadType
              _ucilBearerToken
              _ucilPageToken
              _ucilPageSize
              _ucilCallback
              _ucilFields
              (Just AltJSON)
              monitoringService
          where go
                  = buildClient
                      (Proxy :: Proxy UptimeCheckIPsListResource)
                      mempty
