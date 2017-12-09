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
-- Module      : Network.Google.Resource.Logging.BillingAccounts.Sinks.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a sink. This method replaces the following fields in the
-- existing sink with values from the new sink: destination, and filter.
-- The updated sink might also have a new writer_identity; see the
-- unique_writer_identity field.
--
-- /See:/ <https://cloud.google.com/logging/docs/ Stackdriver Logging API Reference> for @logging.billingAccounts.sinks.update@.
module Network.Google.Resource.Logging.BillingAccounts.Sinks.Update
    (
    -- * REST Resource
      BillingAccountsSinksUpdateResource

    -- * Creating a Request
    , billingAccountsSinksUpdate
    , BillingAccountsSinksUpdate

    -- * Request Lenses
    , basuXgafv
    , basuUniqueWriterIdentity
    , basuUploadProtocol
    , basuUpdateMask
    , basuPp
    , basuAccessToken
    , basuUploadType
    , basuPayload
    , basuBearerToken
    , basuSinkName
    , basuFields
    , basuCallback
    ) where

import Network.Google.Logging.Types
import Network.Google.Prelude

-- | A resource alias for @logging.billingAccounts.sinks.update@ method which the
-- 'BillingAccountsSinksUpdate' request conforms to.
type BillingAccountsSinksUpdateResource =
     "v2" :>
       Capture "sinkName" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "uniqueWriterIdentity" Bool :>
             QueryParam "upload_protocol" Text :>
               QueryParam "updateMask" FieldMask :>
                 QueryParam "pp" Bool :>
                   QueryParam "access_token" Text :>
                     QueryParam "uploadType" Text :>
                       QueryParam "bearer_token" Text :>
                         QueryParam "callback" Text :>
                           QueryParam "fields" Text :>
                             QueryParam "alt" AltJSON :>
                               ReqBody '[JSON] LogSink :> Put '[JSON] LogSink

-- | Updates a sink. This method replaces the following fields in the
-- existing sink with values from the new sink: destination, and filter.
-- The updated sink might also have a new writer_identity; see the
-- unique_writer_identity field.
--
-- /See:/ 'billingAccountsSinksUpdate' smart constructor.
data BillingAccountsSinksUpdate = BillingAccountsSinksUpdate'
    { _basuXgafv :: !(Maybe Xgafv)
    , _basuUniqueWriterIdentity :: !(Maybe Bool)
    , _basuUploadProtocol :: !(Maybe Text)
    , _basuUpdateMask :: !(Maybe FieldMask)
    , _basuPp :: !Bool
    , _basuAccessToken :: !(Maybe Text)
    , _basuUploadType :: !(Maybe Text)
    , _basuPayload :: !LogSink
    , _basuBearerToken :: !(Maybe Text)
    , _basuSinkName :: !Text
    , _basuFields :: !(Maybe Text)
    , _basuCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BillingAccountsSinksUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'basuXgafv'
--
-- * 'basuUniqueWriterIdentity'
--
-- * 'basuUploadProtocol'
--
-- * 'basuUpdateMask'
--
-- * 'basuPp'
--
-- * 'basuAccessToken'
--
-- * 'basuUploadType'
--
-- * 'basuPayload'
--
-- * 'basuBearerToken'
--
-- * 'basuSinkName'
--
-- * 'basuFields'
--
-- * 'basuCallback'
billingAccountsSinksUpdate
    :: LogSink -- ^ 'basuPayload'
    -> Text -- ^ 'basuSinkName'
    -> BillingAccountsSinksUpdate
billingAccountsSinksUpdate pBasuPayload_ pBasuSinkName_ = 
    BillingAccountsSinksUpdate'
    { _basuXgafv = Nothing
    , _basuUniqueWriterIdentity = Nothing
    , _basuUploadProtocol = Nothing
    , _basuUpdateMask = Nothing
    , _basuPp = True
    , _basuAccessToken = Nothing
    , _basuUploadType = Nothing
    , _basuPayload = pBasuPayload_
    , _basuBearerToken = Nothing
    , _basuSinkName = pBasuSinkName_
    , _basuFields = Nothing
    , _basuCallback = Nothing
    }

-- | V1 error format.
basuXgafv :: Lens' BillingAccountsSinksUpdate (Maybe Xgafv)
basuXgafv
  = lens _basuXgafv (\ s a -> s{_basuXgafv = a})

-- | Optional. See sinks.create for a description of this field. When
-- updating a sink, the effect of this field on the value of
-- writer_identity in the updated sink depends on both the old and new
-- values of this field: If the old and new values of this field are both
-- false or both true, then there is no change to the sink\'s
-- writer_identity. If the old value is false and the new value is true,
-- then writer_identity is changed to a unique service account. It is an
-- error if the old value is true and the new value is set to false or
-- defaulted to false.
basuUniqueWriterIdentity :: Lens' BillingAccountsSinksUpdate (Maybe Bool)
basuUniqueWriterIdentity
  = lens _basuUniqueWriterIdentity
      (\ s a -> s{_basuUniqueWriterIdentity = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
basuUploadProtocol :: Lens' BillingAccountsSinksUpdate (Maybe Text)
basuUploadProtocol
  = lens _basuUploadProtocol
      (\ s a -> s{_basuUploadProtocol = a})

-- | Optional. Field mask that specifies the fields in sink that need an
-- update. A sink field will be overwritten if, and only if, it is in the
-- update mask. name and output only fields cannot be updated.An empty
-- updateMask is temporarily treated as using the following mask for
-- backwards compatibility purposes: destination,filter,includeChildren At
-- some point in the future, behavior will be removed and specifying an
-- empty updateMask will be an error.For a detailed FieldMask definition,
-- see
-- https:\/\/developers.google.com\/protocol-buffers\/docs\/reference\/google.protobuf#fieldmaskExample:
-- updateMask=filter.
basuUpdateMask :: Lens' BillingAccountsSinksUpdate (Maybe FieldMask)
basuUpdateMask
  = lens _basuUpdateMask
      (\ s a -> s{_basuUpdateMask = a})

-- | Pretty-print response.
basuPp :: Lens' BillingAccountsSinksUpdate Bool
basuPp = lens _basuPp (\ s a -> s{_basuPp = a})

-- | OAuth access token.
basuAccessToken :: Lens' BillingAccountsSinksUpdate (Maybe Text)
basuAccessToken
  = lens _basuAccessToken
      (\ s a -> s{_basuAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
basuUploadType :: Lens' BillingAccountsSinksUpdate (Maybe Text)
basuUploadType
  = lens _basuUploadType
      (\ s a -> s{_basuUploadType = a})

-- | Multipart request metadata.
basuPayload :: Lens' BillingAccountsSinksUpdate LogSink
basuPayload
  = lens _basuPayload (\ s a -> s{_basuPayload = a})

-- | OAuth bearer token.
basuBearerToken :: Lens' BillingAccountsSinksUpdate (Maybe Text)
basuBearerToken
  = lens _basuBearerToken
      (\ s a -> s{_basuBearerToken = a})

-- | Required. The full resource name of the sink to update, including the
-- parent resource and the sink identifier:
-- \"projects\/[PROJECT_ID]\/sinks\/[SINK_ID]\"
-- \"organizations\/[ORGANIZATION_ID]\/sinks\/[SINK_ID]\"
-- \"billingAccounts\/[BILLING_ACCOUNT_ID]\/sinks\/[SINK_ID]\"
-- \"folders\/[FOLDER_ID]\/sinks\/[SINK_ID]\" Example:
-- \"projects\/my-project-id\/sinks\/my-sink-id\".
basuSinkName :: Lens' BillingAccountsSinksUpdate Text
basuSinkName
  = lens _basuSinkName (\ s a -> s{_basuSinkName = a})

-- | Selector specifying which fields to include in a partial response.
basuFields :: Lens' BillingAccountsSinksUpdate (Maybe Text)
basuFields
  = lens _basuFields (\ s a -> s{_basuFields = a})

-- | JSONP
basuCallback :: Lens' BillingAccountsSinksUpdate (Maybe Text)
basuCallback
  = lens _basuCallback (\ s a -> s{_basuCallback = a})

instance GoogleRequest BillingAccountsSinksUpdate
         where
        type Rs BillingAccountsSinksUpdate = LogSink
        type Scopes BillingAccountsSinksUpdate =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/logging.admin"]
        requestClient BillingAccountsSinksUpdate'{..}
          = go _basuSinkName _basuXgafv
              _basuUniqueWriterIdentity
              _basuUploadProtocol
              _basuUpdateMask
              (Just _basuPp)
              _basuAccessToken
              _basuUploadType
              _basuBearerToken
              _basuCallback
              _basuFields
              (Just AltJSON)
              _basuPayload
              loggingService
          where go
                  = buildClient
                      (Proxy :: Proxy BillingAccountsSinksUpdateResource)
                      mempty
