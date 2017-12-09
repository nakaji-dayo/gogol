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
-- Module      : Network.Google.Resource.ServiceManagement.Services.Consumers.SetIAMPolicy
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the access control policy on the specified resource. Replaces any
-- existing policy.
--
-- /See:/ <https://cloud.google.com/service-management/ Google Service Management API Reference> for @servicemanagement.services.consumers.setIamPolicy@.
module Network.Google.Resource.ServiceManagement.Services.Consumers.SetIAMPolicy
    (
    -- * REST Resource
      ServicesConsumersSetIAMPolicyResource

    -- * Creating a Request
    , servicesConsumersSetIAMPolicy
    , ServicesConsumersSetIAMPolicy

    -- * Request Lenses
    , scsipXgafv
    , scsipUploadProtocol
    , scsipPp
    , scsipAccessToken
    , scsipUploadType
    , scsipPayload
    , scsipBearerToken
    , scsipResource
    , scsipFields
    , scsipCallback
    ) where

import Network.Google.Prelude
import Network.Google.ServiceManagement.Types

-- | A resource alias for @servicemanagement.services.consumers.setIamPolicy@ method which the
-- 'ServicesConsumersSetIAMPolicy' request conforms to.
type ServicesConsumersSetIAMPolicyResource =
     "v1" :>
       CaptureMode "resource" "setIamPolicy" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] SetIAMPolicyRequest :>
                             Post '[JSON] Policy

-- | Sets the access control policy on the specified resource. Replaces any
-- existing policy.
--
-- /See:/ 'servicesConsumersSetIAMPolicy' smart constructor.
data ServicesConsumersSetIAMPolicy = ServicesConsumersSetIAMPolicy'
    { _scsipXgafv :: !(Maybe Xgafv)
    , _scsipUploadProtocol :: !(Maybe Text)
    , _scsipPp :: !Bool
    , _scsipAccessToken :: !(Maybe Text)
    , _scsipUploadType :: !(Maybe Text)
    , _scsipPayload :: !SetIAMPolicyRequest
    , _scsipBearerToken :: !(Maybe Text)
    , _scsipResource :: !Text
    , _scsipFields :: !(Maybe Text)
    , _scsipCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ServicesConsumersSetIAMPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scsipXgafv'
--
-- * 'scsipUploadProtocol'
--
-- * 'scsipPp'
--
-- * 'scsipAccessToken'
--
-- * 'scsipUploadType'
--
-- * 'scsipPayload'
--
-- * 'scsipBearerToken'
--
-- * 'scsipResource'
--
-- * 'scsipFields'
--
-- * 'scsipCallback'
servicesConsumersSetIAMPolicy
    :: SetIAMPolicyRequest -- ^ 'scsipPayload'
    -> Text -- ^ 'scsipResource'
    -> ServicesConsumersSetIAMPolicy
servicesConsumersSetIAMPolicy pScsipPayload_ pScsipResource_ = 
    ServicesConsumersSetIAMPolicy'
    { _scsipXgafv = Nothing
    , _scsipUploadProtocol = Nothing
    , _scsipPp = True
    , _scsipAccessToken = Nothing
    , _scsipUploadType = Nothing
    , _scsipPayload = pScsipPayload_
    , _scsipBearerToken = Nothing
    , _scsipResource = pScsipResource_
    , _scsipFields = Nothing
    , _scsipCallback = Nothing
    }

-- | V1 error format.
scsipXgafv :: Lens' ServicesConsumersSetIAMPolicy (Maybe Xgafv)
scsipXgafv
  = lens _scsipXgafv (\ s a -> s{_scsipXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
scsipUploadProtocol :: Lens' ServicesConsumersSetIAMPolicy (Maybe Text)
scsipUploadProtocol
  = lens _scsipUploadProtocol
      (\ s a -> s{_scsipUploadProtocol = a})

-- | Pretty-print response.
scsipPp :: Lens' ServicesConsumersSetIAMPolicy Bool
scsipPp = lens _scsipPp (\ s a -> s{_scsipPp = a})

-- | OAuth access token.
scsipAccessToken :: Lens' ServicesConsumersSetIAMPolicy (Maybe Text)
scsipAccessToken
  = lens _scsipAccessToken
      (\ s a -> s{_scsipAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
scsipUploadType :: Lens' ServicesConsumersSetIAMPolicy (Maybe Text)
scsipUploadType
  = lens _scsipUploadType
      (\ s a -> s{_scsipUploadType = a})

-- | Multipart request metadata.
scsipPayload :: Lens' ServicesConsumersSetIAMPolicy SetIAMPolicyRequest
scsipPayload
  = lens _scsipPayload (\ s a -> s{_scsipPayload = a})

-- | OAuth bearer token.
scsipBearerToken :: Lens' ServicesConsumersSetIAMPolicy (Maybe Text)
scsipBearerToken
  = lens _scsipBearerToken
      (\ s a -> s{_scsipBearerToken = a})

-- | REQUIRED: The resource for which the policy is being specified. See the
-- operation documentation for the appropriate value for this field.
scsipResource :: Lens' ServicesConsumersSetIAMPolicy Text
scsipResource
  = lens _scsipResource
      (\ s a -> s{_scsipResource = a})

-- | Selector specifying which fields to include in a partial response.
scsipFields :: Lens' ServicesConsumersSetIAMPolicy (Maybe Text)
scsipFields
  = lens _scsipFields (\ s a -> s{_scsipFields = a})

-- | JSONP
scsipCallback :: Lens' ServicesConsumersSetIAMPolicy (Maybe Text)
scsipCallback
  = lens _scsipCallback
      (\ s a -> s{_scsipCallback = a})

instance GoogleRequest ServicesConsumersSetIAMPolicy
         where
        type Rs ServicesConsumersSetIAMPolicy = Policy
        type Scopes ServicesConsumersSetIAMPolicy =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/service.management"]
        requestClient ServicesConsumersSetIAMPolicy'{..}
          = go _scsipResource _scsipXgafv _scsipUploadProtocol
              (Just _scsipPp)
              _scsipAccessToken
              _scsipUploadType
              _scsipBearerToken
              _scsipCallback
              _scsipFields
              (Just AltJSON)
              _scsipPayload
              serviceManagementService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ServicesConsumersSetIAMPolicyResource)
                      mempty
