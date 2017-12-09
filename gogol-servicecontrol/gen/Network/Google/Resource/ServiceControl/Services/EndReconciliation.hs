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
-- Module      : Network.Google.Resource.ServiceControl.Services.EndReconciliation
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signals the quota controller that service ends the ongoing usage
-- reconciliation. This method requires the
-- \`servicemanagement.services.quota\` permission on the specified
-- service. For more information, see [Google Cloud
-- IAM](https:\/\/cloud.google.com\/iam).
--
-- /See:/ <https://cloud.google.com/service-control/ Google Service Control API Reference> for @servicecontrol.services.endReconciliation@.
module Network.Google.Resource.ServiceControl.Services.EndReconciliation
    (
    -- * REST Resource
      ServicesEndReconciliationResource

    -- * Creating a Request
    , servicesEndReconciliation
    , ServicesEndReconciliation

    -- * Request Lenses
    , serXgafv
    , serUploadProtocol
    , serPp
    , serAccessToken
    , serUploadType
    , serPayload
    , serBearerToken
    , serServiceName
    , serFields
    , serCallback
    ) where

import Network.Google.Prelude
import Network.Google.ServiceControl.Types

-- | A resource alias for @servicecontrol.services.endReconciliation@ method which the
-- 'ServicesEndReconciliation' request conforms to.
type ServicesEndReconciliationResource =
     "v1" :>
       "services" :>
         CaptureMode "serviceName" "endReconciliation" Text :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             ReqBody '[JSON] EndReconciliationRequest :>
                               Post '[JSON] EndReconciliationResponse

-- | Signals the quota controller that service ends the ongoing usage
-- reconciliation. This method requires the
-- \`servicemanagement.services.quota\` permission on the specified
-- service. For more information, see [Google Cloud
-- IAM](https:\/\/cloud.google.com\/iam).
--
-- /See:/ 'servicesEndReconciliation' smart constructor.
data ServicesEndReconciliation = ServicesEndReconciliation'
    { _serXgafv :: !(Maybe Xgafv)
    , _serUploadProtocol :: !(Maybe Text)
    , _serPp :: !Bool
    , _serAccessToken :: !(Maybe Text)
    , _serUploadType :: !(Maybe Text)
    , _serPayload :: !EndReconciliationRequest
    , _serBearerToken :: !(Maybe Text)
    , _serServiceName :: !Text
    , _serFields :: !(Maybe Text)
    , _serCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ServicesEndReconciliation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'serXgafv'
--
-- * 'serUploadProtocol'
--
-- * 'serPp'
--
-- * 'serAccessToken'
--
-- * 'serUploadType'
--
-- * 'serPayload'
--
-- * 'serBearerToken'
--
-- * 'serServiceName'
--
-- * 'serFields'
--
-- * 'serCallback'
servicesEndReconciliation
    :: EndReconciliationRequest -- ^ 'serPayload'
    -> Text -- ^ 'serServiceName'
    -> ServicesEndReconciliation
servicesEndReconciliation pSerPayload_ pSerServiceName_ = 
    ServicesEndReconciliation'
    { _serXgafv = Nothing
    , _serUploadProtocol = Nothing
    , _serPp = True
    , _serAccessToken = Nothing
    , _serUploadType = Nothing
    , _serPayload = pSerPayload_
    , _serBearerToken = Nothing
    , _serServiceName = pSerServiceName_
    , _serFields = Nothing
    , _serCallback = Nothing
    }

-- | V1 error format.
serXgafv :: Lens' ServicesEndReconciliation (Maybe Xgafv)
serXgafv = lens _serXgafv (\ s a -> s{_serXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
serUploadProtocol :: Lens' ServicesEndReconciliation (Maybe Text)
serUploadProtocol
  = lens _serUploadProtocol
      (\ s a -> s{_serUploadProtocol = a})

-- | Pretty-print response.
serPp :: Lens' ServicesEndReconciliation Bool
serPp = lens _serPp (\ s a -> s{_serPp = a})

-- | OAuth access token.
serAccessToken :: Lens' ServicesEndReconciliation (Maybe Text)
serAccessToken
  = lens _serAccessToken
      (\ s a -> s{_serAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
serUploadType :: Lens' ServicesEndReconciliation (Maybe Text)
serUploadType
  = lens _serUploadType
      (\ s a -> s{_serUploadType = a})

-- | Multipart request metadata.
serPayload :: Lens' ServicesEndReconciliation EndReconciliationRequest
serPayload
  = lens _serPayload (\ s a -> s{_serPayload = a})

-- | OAuth bearer token.
serBearerToken :: Lens' ServicesEndReconciliation (Maybe Text)
serBearerToken
  = lens _serBearerToken
      (\ s a -> s{_serBearerToken = a})

-- | Name of the service as specified in the service configuration. For
-- example, \`\"pubsub.googleapis.com\"\`. See google.api.Service for the
-- definition of a service name.
serServiceName :: Lens' ServicesEndReconciliation Text
serServiceName
  = lens _serServiceName
      (\ s a -> s{_serServiceName = a})

-- | Selector specifying which fields to include in a partial response.
serFields :: Lens' ServicesEndReconciliation (Maybe Text)
serFields
  = lens _serFields (\ s a -> s{_serFields = a})

-- | JSONP
serCallback :: Lens' ServicesEndReconciliation (Maybe Text)
serCallback
  = lens _serCallback (\ s a -> s{_serCallback = a})

instance GoogleRequest ServicesEndReconciliation
         where
        type Rs ServicesEndReconciliation =
             EndReconciliationResponse
        type Scopes ServicesEndReconciliation =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/servicecontrol"]
        requestClient ServicesEndReconciliation'{..}
          = go _serServiceName _serXgafv _serUploadProtocol
              (Just _serPp)
              _serAccessToken
              _serUploadType
              _serBearerToken
              _serCallback
              _serFields
              (Just AltJSON)
              _serPayload
              serviceControlService
          where go
                  = buildClient
                      (Proxy :: Proxy ServicesEndReconciliationResource)
                      mempty
