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
-- Module      : Network.Google.Resource.ServiceControl.Services.StartReconciliation
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlike rate quota, allocation quota does not get refilled periodically.
-- So, it is possible that the quota usage as seen by the service differs
-- from what the One Platform considers the usage is. This is expected to
-- happen only rarely, but over time this can accumulate. Services can
-- invoke StartReconciliation and EndReconciliation to correct this usage
-- drift, as described below: 1. Service sends StartReconciliation with a
-- timestamp in future for each metric that needs to be reconciled. The
-- timestamp being in future allows to account for in-flight AllocateQuota
-- and ReleaseQuota requests for the same metric. 2. One Platform records
-- this timestamp and starts tracking subsequent AllocateQuota and
-- ReleaseQuota requests until EndReconciliation is called. 3. At or after
-- the time specified in the StartReconciliation, service sends
-- EndReconciliation with the usage that needs to be reconciled to. 4. One
-- Platform adjusts its own record of usage for that metric to the value
-- specified in EndReconciliation by taking in to account any allocation or
-- release between StartReconciliation and EndReconciliation. Signals the
-- quota controller that the service wants to perform a usage
-- reconciliation as specified in the request. This method requires the
-- \`servicemanagement.services.quota\` permission on the specified
-- service. For more information, see [Google Cloud
-- IAM](https:\/\/cloud.google.com\/iam).
--
-- /See:/ <https://cloud.google.com/service-control/ Google Service Control API Reference> for @servicecontrol.services.startReconciliation@.
module Network.Google.Resource.ServiceControl.Services.StartReconciliation
    (
    -- * REST Resource
      ServicesStartReconciliationResource

    -- * Creating a Request
    , servicesStartReconciliation
    , ServicesStartReconciliation

    -- * Request Lenses
    , ssrXgafv
    , ssrUploadProtocol
    , ssrPp
    , ssrAccessToken
    , ssrUploadType
    , ssrPayload
    , ssrBearerToken
    , ssrServiceName
    , ssrFields
    , ssrCallback
    ) where

import Network.Google.Prelude
import Network.Google.ServiceControl.Types

-- | A resource alias for @servicecontrol.services.startReconciliation@ method which the
-- 'ServicesStartReconciliation' request conforms to.
type ServicesStartReconciliationResource =
     "v1" :>
       "services" :>
         CaptureMode "serviceName" "startReconciliation" Text
           :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             ReqBody '[JSON] StartReconciliationRequest :>
                               Post '[JSON] StartReconciliationResponse

-- | Unlike rate quota, allocation quota does not get refilled periodically.
-- So, it is possible that the quota usage as seen by the service differs
-- from what the One Platform considers the usage is. This is expected to
-- happen only rarely, but over time this can accumulate. Services can
-- invoke StartReconciliation and EndReconciliation to correct this usage
-- drift, as described below: 1. Service sends StartReconciliation with a
-- timestamp in future for each metric that needs to be reconciled. The
-- timestamp being in future allows to account for in-flight AllocateQuota
-- and ReleaseQuota requests for the same metric. 2. One Platform records
-- this timestamp and starts tracking subsequent AllocateQuota and
-- ReleaseQuota requests until EndReconciliation is called. 3. At or after
-- the time specified in the StartReconciliation, service sends
-- EndReconciliation with the usage that needs to be reconciled to. 4. One
-- Platform adjusts its own record of usage for that metric to the value
-- specified in EndReconciliation by taking in to account any allocation or
-- release between StartReconciliation and EndReconciliation. Signals the
-- quota controller that the service wants to perform a usage
-- reconciliation as specified in the request. This method requires the
-- \`servicemanagement.services.quota\` permission on the specified
-- service. For more information, see [Google Cloud
-- IAM](https:\/\/cloud.google.com\/iam).
--
-- /See:/ 'servicesStartReconciliation' smart constructor.
data ServicesStartReconciliation = ServicesStartReconciliation'
    { _ssrXgafv :: !(Maybe Xgafv)
    , _ssrUploadProtocol :: !(Maybe Text)
    , _ssrPp :: !Bool
    , _ssrAccessToken :: !(Maybe Text)
    , _ssrUploadType :: !(Maybe Text)
    , _ssrPayload :: !StartReconciliationRequest
    , _ssrBearerToken :: !(Maybe Text)
    , _ssrServiceName :: !Text
    , _ssrFields :: !(Maybe Text)
    , _ssrCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ServicesStartReconciliation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssrXgafv'
--
-- * 'ssrUploadProtocol'
--
-- * 'ssrPp'
--
-- * 'ssrAccessToken'
--
-- * 'ssrUploadType'
--
-- * 'ssrPayload'
--
-- * 'ssrBearerToken'
--
-- * 'ssrServiceName'
--
-- * 'ssrFields'
--
-- * 'ssrCallback'
servicesStartReconciliation
    :: StartReconciliationRequest -- ^ 'ssrPayload'
    -> Text -- ^ 'ssrServiceName'
    -> ServicesStartReconciliation
servicesStartReconciliation pSsrPayload_ pSsrServiceName_ = 
    ServicesStartReconciliation'
    { _ssrXgafv = Nothing
    , _ssrUploadProtocol = Nothing
    , _ssrPp = True
    , _ssrAccessToken = Nothing
    , _ssrUploadType = Nothing
    , _ssrPayload = pSsrPayload_
    , _ssrBearerToken = Nothing
    , _ssrServiceName = pSsrServiceName_
    , _ssrFields = Nothing
    , _ssrCallback = Nothing
    }

-- | V1 error format.
ssrXgafv :: Lens' ServicesStartReconciliation (Maybe Xgafv)
ssrXgafv = lens _ssrXgafv (\ s a -> s{_ssrXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
ssrUploadProtocol :: Lens' ServicesStartReconciliation (Maybe Text)
ssrUploadProtocol
  = lens _ssrUploadProtocol
      (\ s a -> s{_ssrUploadProtocol = a})

-- | Pretty-print response.
ssrPp :: Lens' ServicesStartReconciliation Bool
ssrPp = lens _ssrPp (\ s a -> s{_ssrPp = a})

-- | OAuth access token.
ssrAccessToken :: Lens' ServicesStartReconciliation (Maybe Text)
ssrAccessToken
  = lens _ssrAccessToken
      (\ s a -> s{_ssrAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
ssrUploadType :: Lens' ServicesStartReconciliation (Maybe Text)
ssrUploadType
  = lens _ssrUploadType
      (\ s a -> s{_ssrUploadType = a})

-- | Multipart request metadata.
ssrPayload :: Lens' ServicesStartReconciliation StartReconciliationRequest
ssrPayload
  = lens _ssrPayload (\ s a -> s{_ssrPayload = a})

-- | OAuth bearer token.
ssrBearerToken :: Lens' ServicesStartReconciliation (Maybe Text)
ssrBearerToken
  = lens _ssrBearerToken
      (\ s a -> s{_ssrBearerToken = a})

-- | Name of the service as specified in the service configuration. For
-- example, \`\"pubsub.googleapis.com\"\`. See google.api.Service for the
-- definition of a service name.
ssrServiceName :: Lens' ServicesStartReconciliation Text
ssrServiceName
  = lens _ssrServiceName
      (\ s a -> s{_ssrServiceName = a})

-- | Selector specifying which fields to include in a partial response.
ssrFields :: Lens' ServicesStartReconciliation (Maybe Text)
ssrFields
  = lens _ssrFields (\ s a -> s{_ssrFields = a})

-- | JSONP
ssrCallback :: Lens' ServicesStartReconciliation (Maybe Text)
ssrCallback
  = lens _ssrCallback (\ s a -> s{_ssrCallback = a})

instance GoogleRequest ServicesStartReconciliation
         where
        type Rs ServicesStartReconciliation =
             StartReconciliationResponse
        type Scopes ServicesStartReconciliation =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/servicecontrol"]
        requestClient ServicesStartReconciliation'{..}
          = go _ssrServiceName _ssrXgafv _ssrUploadProtocol
              (Just _ssrPp)
              _ssrAccessToken
              _ssrUploadType
              _ssrBearerToken
              _ssrCallback
              _ssrFields
              (Just AltJSON)
              _ssrPayload
              serviceControlService
          where go
                  = buildClient
                      (Proxy :: Proxy ServicesStartReconciliationResource)
                      mempty
