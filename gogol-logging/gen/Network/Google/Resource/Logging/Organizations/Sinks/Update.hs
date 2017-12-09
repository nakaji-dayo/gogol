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
-- Module      : Network.Google.Resource.Logging.Organizations.Sinks.Update
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
-- /See:/ <https://cloud.google.com/logging/docs/ Stackdriver Logging API Reference> for @logging.organizations.sinks.update@.
module Network.Google.Resource.Logging.Organizations.Sinks.Update
    (
    -- * REST Resource
      OrganizationsSinksUpdateResource

    -- * Creating a Request
    , organizationsSinksUpdate
    , OrganizationsSinksUpdate

    -- * Request Lenses
    , osuXgafv
    , osuUniqueWriterIdentity
    , osuUploadProtocol
    , osuUpdateMask
    , osuPp
    , osuAccessToken
    , osuUploadType
    , osuPayload
    , osuBearerToken
    , osuSinkName
    , osuFields
    , osuCallback
    ) where

import Network.Google.Logging.Types
import Network.Google.Prelude

-- | A resource alias for @logging.organizations.sinks.update@ method which the
-- 'OrganizationsSinksUpdate' request conforms to.
type OrganizationsSinksUpdateResource =
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
-- /See:/ 'organizationsSinksUpdate' smart constructor.
data OrganizationsSinksUpdate = OrganizationsSinksUpdate'
    { _osuXgafv :: !(Maybe Xgafv)
    , _osuUniqueWriterIdentity :: !(Maybe Bool)
    , _osuUploadProtocol :: !(Maybe Text)
    , _osuUpdateMask :: !(Maybe FieldMask)
    , _osuPp :: !Bool
    , _osuAccessToken :: !(Maybe Text)
    , _osuUploadType :: !(Maybe Text)
    , _osuPayload :: !LogSink
    , _osuBearerToken :: !(Maybe Text)
    , _osuSinkName :: !Text
    , _osuFields :: !(Maybe Text)
    , _osuCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrganizationsSinksUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osuXgafv'
--
-- * 'osuUniqueWriterIdentity'
--
-- * 'osuUploadProtocol'
--
-- * 'osuUpdateMask'
--
-- * 'osuPp'
--
-- * 'osuAccessToken'
--
-- * 'osuUploadType'
--
-- * 'osuPayload'
--
-- * 'osuBearerToken'
--
-- * 'osuSinkName'
--
-- * 'osuFields'
--
-- * 'osuCallback'
organizationsSinksUpdate
    :: LogSink -- ^ 'osuPayload'
    -> Text -- ^ 'osuSinkName'
    -> OrganizationsSinksUpdate
organizationsSinksUpdate pOsuPayload_ pOsuSinkName_ = 
    OrganizationsSinksUpdate'
    { _osuXgafv = Nothing
    , _osuUniqueWriterIdentity = Nothing
    , _osuUploadProtocol = Nothing
    , _osuUpdateMask = Nothing
    , _osuPp = True
    , _osuAccessToken = Nothing
    , _osuUploadType = Nothing
    , _osuPayload = pOsuPayload_
    , _osuBearerToken = Nothing
    , _osuSinkName = pOsuSinkName_
    , _osuFields = Nothing
    , _osuCallback = Nothing
    }

-- | V1 error format.
osuXgafv :: Lens' OrganizationsSinksUpdate (Maybe Xgafv)
osuXgafv = lens _osuXgafv (\ s a -> s{_osuXgafv = a})

-- | Optional. See sinks.create for a description of this field. When
-- updating a sink, the effect of this field on the value of
-- writer_identity in the updated sink depends on both the old and new
-- values of this field: If the old and new values of this field are both
-- false or both true, then there is no change to the sink\'s
-- writer_identity. If the old value is false and the new value is true,
-- then writer_identity is changed to a unique service account. It is an
-- error if the old value is true and the new value is set to false or
-- defaulted to false.
osuUniqueWriterIdentity :: Lens' OrganizationsSinksUpdate (Maybe Bool)
osuUniqueWriterIdentity
  = lens _osuUniqueWriterIdentity
      (\ s a -> s{_osuUniqueWriterIdentity = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
osuUploadProtocol :: Lens' OrganizationsSinksUpdate (Maybe Text)
osuUploadProtocol
  = lens _osuUploadProtocol
      (\ s a -> s{_osuUploadProtocol = a})

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
osuUpdateMask :: Lens' OrganizationsSinksUpdate (Maybe FieldMask)
osuUpdateMask
  = lens _osuUpdateMask
      (\ s a -> s{_osuUpdateMask = a})

-- | Pretty-print response.
osuPp :: Lens' OrganizationsSinksUpdate Bool
osuPp = lens _osuPp (\ s a -> s{_osuPp = a})

-- | OAuth access token.
osuAccessToken :: Lens' OrganizationsSinksUpdate (Maybe Text)
osuAccessToken
  = lens _osuAccessToken
      (\ s a -> s{_osuAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
osuUploadType :: Lens' OrganizationsSinksUpdate (Maybe Text)
osuUploadType
  = lens _osuUploadType
      (\ s a -> s{_osuUploadType = a})

-- | Multipart request metadata.
osuPayload :: Lens' OrganizationsSinksUpdate LogSink
osuPayload
  = lens _osuPayload (\ s a -> s{_osuPayload = a})

-- | OAuth bearer token.
osuBearerToken :: Lens' OrganizationsSinksUpdate (Maybe Text)
osuBearerToken
  = lens _osuBearerToken
      (\ s a -> s{_osuBearerToken = a})

-- | Required. The full resource name of the sink to update, including the
-- parent resource and the sink identifier:
-- \"projects\/[PROJECT_ID]\/sinks\/[SINK_ID]\"
-- \"organizations\/[ORGANIZATION_ID]\/sinks\/[SINK_ID]\"
-- \"billingAccounts\/[BILLING_ACCOUNT_ID]\/sinks\/[SINK_ID]\"
-- \"folders\/[FOLDER_ID]\/sinks\/[SINK_ID]\" Example:
-- \"projects\/my-project-id\/sinks\/my-sink-id\".
osuSinkName :: Lens' OrganizationsSinksUpdate Text
osuSinkName
  = lens _osuSinkName (\ s a -> s{_osuSinkName = a})

-- | Selector specifying which fields to include in a partial response.
osuFields :: Lens' OrganizationsSinksUpdate (Maybe Text)
osuFields
  = lens _osuFields (\ s a -> s{_osuFields = a})

-- | JSONP
osuCallback :: Lens' OrganizationsSinksUpdate (Maybe Text)
osuCallback
  = lens _osuCallback (\ s a -> s{_osuCallback = a})

instance GoogleRequest OrganizationsSinksUpdate where
        type Rs OrganizationsSinksUpdate = LogSink
        type Scopes OrganizationsSinksUpdate =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/logging.admin"]
        requestClient OrganizationsSinksUpdate'{..}
          = go _osuSinkName _osuXgafv _osuUniqueWriterIdentity
              _osuUploadProtocol
              _osuUpdateMask
              (Just _osuPp)
              _osuAccessToken
              _osuUploadType
              _osuBearerToken
              _osuCallback
              _osuFields
              (Just AltJSON)
              _osuPayload
              loggingService
          where go
                  = buildClient
                      (Proxy :: Proxy OrganizationsSinksUpdateResource)
                      mempty
