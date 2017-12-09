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
-- Module      : Network.Google.Resource.ServiceControl.Services.ReleaseQuota
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases previously allocated quota done through AllocateQuota method.
-- This method requires the \`servicemanagement.services.quota\` permission
-- on the specified service. For more information, see [Cloud
-- IAM](https:\/\/cloud.google.com\/iam). **NOTE:** The client **must**
-- fail-open on server errors \`INTERNAL\`, \`UNKNOWN\`,
-- \`DEADLINE_EXCEEDED\`, and \`UNAVAILABLE\`. To ensure system
-- reliability, the server may inject these errors to prohibit any hard
-- dependency on the quota functionality.
--
-- /See:/ <https://cloud.google.com/service-control/ Google Service Control API Reference> for @servicecontrol.services.releaseQuota@.
module Network.Google.Resource.ServiceControl.Services.ReleaseQuota
    (
    -- * REST Resource
      ServicesReleaseQuotaResource

    -- * Creating a Request
    , servicesReleaseQuota
    , ServicesReleaseQuota

    -- * Request Lenses
    , srqXgafv
    , srqUploadProtocol
    , srqPp
    , srqAccessToken
    , srqUploadType
    , srqPayload
    , srqBearerToken
    , srqServiceName
    , srqFields
    , srqCallback
    ) where

import Network.Google.Prelude
import Network.Google.ServiceControl.Types

-- | A resource alias for @servicecontrol.services.releaseQuota@ method which the
-- 'ServicesReleaseQuota' request conforms to.
type ServicesReleaseQuotaResource =
     "v1" :>
       "services" :>
         CaptureMode "serviceName" "releaseQuota" Text :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             ReqBody '[JSON] ReleaseQuotaRequest :>
                               Post '[JSON] ReleaseQuotaResponse

-- | Releases previously allocated quota done through AllocateQuota method.
-- This method requires the \`servicemanagement.services.quota\` permission
-- on the specified service. For more information, see [Cloud
-- IAM](https:\/\/cloud.google.com\/iam). **NOTE:** The client **must**
-- fail-open on server errors \`INTERNAL\`, \`UNKNOWN\`,
-- \`DEADLINE_EXCEEDED\`, and \`UNAVAILABLE\`. To ensure system
-- reliability, the server may inject these errors to prohibit any hard
-- dependency on the quota functionality.
--
-- /See:/ 'servicesReleaseQuota' smart constructor.
data ServicesReleaseQuota = ServicesReleaseQuota'
    { _srqXgafv :: !(Maybe Xgafv)
    , _srqUploadProtocol :: !(Maybe Text)
    , _srqPp :: !Bool
    , _srqAccessToken :: !(Maybe Text)
    , _srqUploadType :: !(Maybe Text)
    , _srqPayload :: !ReleaseQuotaRequest
    , _srqBearerToken :: !(Maybe Text)
    , _srqServiceName :: !Text
    , _srqFields :: !(Maybe Text)
    , _srqCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ServicesReleaseQuota' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srqXgafv'
--
-- * 'srqUploadProtocol'
--
-- * 'srqPp'
--
-- * 'srqAccessToken'
--
-- * 'srqUploadType'
--
-- * 'srqPayload'
--
-- * 'srqBearerToken'
--
-- * 'srqServiceName'
--
-- * 'srqFields'
--
-- * 'srqCallback'
servicesReleaseQuota
    :: ReleaseQuotaRequest -- ^ 'srqPayload'
    -> Text -- ^ 'srqServiceName'
    -> ServicesReleaseQuota
servicesReleaseQuota pSrqPayload_ pSrqServiceName_ = 
    ServicesReleaseQuota'
    { _srqXgafv = Nothing
    , _srqUploadProtocol = Nothing
    , _srqPp = True
    , _srqAccessToken = Nothing
    , _srqUploadType = Nothing
    , _srqPayload = pSrqPayload_
    , _srqBearerToken = Nothing
    , _srqServiceName = pSrqServiceName_
    , _srqFields = Nothing
    , _srqCallback = Nothing
    }

-- | V1 error format.
srqXgafv :: Lens' ServicesReleaseQuota (Maybe Xgafv)
srqXgafv = lens _srqXgafv (\ s a -> s{_srqXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
srqUploadProtocol :: Lens' ServicesReleaseQuota (Maybe Text)
srqUploadProtocol
  = lens _srqUploadProtocol
      (\ s a -> s{_srqUploadProtocol = a})

-- | Pretty-print response.
srqPp :: Lens' ServicesReleaseQuota Bool
srqPp = lens _srqPp (\ s a -> s{_srqPp = a})

-- | OAuth access token.
srqAccessToken :: Lens' ServicesReleaseQuota (Maybe Text)
srqAccessToken
  = lens _srqAccessToken
      (\ s a -> s{_srqAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
srqUploadType :: Lens' ServicesReleaseQuota (Maybe Text)
srqUploadType
  = lens _srqUploadType
      (\ s a -> s{_srqUploadType = a})

-- | Multipart request metadata.
srqPayload :: Lens' ServicesReleaseQuota ReleaseQuotaRequest
srqPayload
  = lens _srqPayload (\ s a -> s{_srqPayload = a})

-- | OAuth bearer token.
srqBearerToken :: Lens' ServicesReleaseQuota (Maybe Text)
srqBearerToken
  = lens _srqBearerToken
      (\ s a -> s{_srqBearerToken = a})

-- | Name of the service as specified in the service configuration. For
-- example, \`\"pubsub.googleapis.com\"\`. See google.api.Service for the
-- definition of a service name.
srqServiceName :: Lens' ServicesReleaseQuota Text
srqServiceName
  = lens _srqServiceName
      (\ s a -> s{_srqServiceName = a})

-- | Selector specifying which fields to include in a partial response.
srqFields :: Lens' ServicesReleaseQuota (Maybe Text)
srqFields
  = lens _srqFields (\ s a -> s{_srqFields = a})

-- | JSONP
srqCallback :: Lens' ServicesReleaseQuota (Maybe Text)
srqCallback
  = lens _srqCallback (\ s a -> s{_srqCallback = a})

instance GoogleRequest ServicesReleaseQuota where
        type Rs ServicesReleaseQuota = ReleaseQuotaResponse
        type Scopes ServicesReleaseQuota =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/servicecontrol"]
        requestClient ServicesReleaseQuota'{..}
          = go _srqServiceName _srqXgafv _srqUploadProtocol
              (Just _srqPp)
              _srqAccessToken
              _srqUploadType
              _srqBearerToken
              _srqCallback
              _srqFields
              (Just AltJSON)
              _srqPayload
              serviceControlService
          where go
                  = buildClient
                      (Proxy :: Proxy ServicesReleaseQuotaResource)
                      mempty
