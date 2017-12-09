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
-- Module      : Network.Google.Resource.Logging.BillingAccounts.Exclusions.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new exclusion in a specified parent resource. Only log entries
-- belonging to that resource can be excluded. You can have up to 10
-- exclusions in a resource.
--
-- /See:/ <https://cloud.google.com/logging/docs/ Stackdriver Logging API Reference> for @logging.billingAccounts.exclusions.create@.
module Network.Google.Resource.Logging.BillingAccounts.Exclusions.Create
    (
    -- * REST Resource
      BillingAccountsExclusionsCreateResource

    -- * Creating a Request
    , billingAccountsExclusionsCreate
    , BillingAccountsExclusionsCreate

    -- * Request Lenses
    , baecParent
    , baecXgafv
    , baecUploadProtocol
    , baecPp
    , baecAccessToken
    , baecUploadType
    , baecPayload
    , baecBearerToken
    , baecFields
    , baecCallback
    ) where

import Network.Google.Logging.Types
import Network.Google.Prelude

-- | A resource alias for @logging.billingAccounts.exclusions.create@ method which the
-- 'BillingAccountsExclusionsCreate' request conforms to.
type BillingAccountsExclusionsCreateResource =
     "v2" :>
       Capture "parent" Text :>
         "exclusions" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             ReqBody '[JSON] LogExclusion :>
                               Post '[JSON] LogExclusion

-- | Creates a new exclusion in a specified parent resource. Only log entries
-- belonging to that resource can be excluded. You can have up to 10
-- exclusions in a resource.
--
-- /See:/ 'billingAccountsExclusionsCreate' smart constructor.
data BillingAccountsExclusionsCreate = BillingAccountsExclusionsCreate'
    { _baecParent :: !Text
    , _baecXgafv :: !(Maybe Xgafv)
    , _baecUploadProtocol :: !(Maybe Text)
    , _baecPp :: !Bool
    , _baecAccessToken :: !(Maybe Text)
    , _baecUploadType :: !(Maybe Text)
    , _baecPayload :: !LogExclusion
    , _baecBearerToken :: !(Maybe Text)
    , _baecFields :: !(Maybe Text)
    , _baecCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BillingAccountsExclusionsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baecParent'
--
-- * 'baecXgafv'
--
-- * 'baecUploadProtocol'
--
-- * 'baecPp'
--
-- * 'baecAccessToken'
--
-- * 'baecUploadType'
--
-- * 'baecPayload'
--
-- * 'baecBearerToken'
--
-- * 'baecFields'
--
-- * 'baecCallback'
billingAccountsExclusionsCreate
    :: Text -- ^ 'baecParent'
    -> LogExclusion -- ^ 'baecPayload'
    -> BillingAccountsExclusionsCreate
billingAccountsExclusionsCreate pBaecParent_ pBaecPayload_ = 
    BillingAccountsExclusionsCreate'
    { _baecParent = pBaecParent_
    , _baecXgafv = Nothing
    , _baecUploadProtocol = Nothing
    , _baecPp = True
    , _baecAccessToken = Nothing
    , _baecUploadType = Nothing
    , _baecPayload = pBaecPayload_
    , _baecBearerToken = Nothing
    , _baecFields = Nothing
    , _baecCallback = Nothing
    }

-- | Required. The parent resource in which to create the exclusion:
-- \"projects\/[PROJECT_ID]\" \"organizations\/[ORGANIZATION_ID]\"
-- \"billingAccounts\/[BILLING_ACCOUNT_ID]\" \"folders\/[FOLDER_ID]\"
-- Examples: \"projects\/my-logging-project\",
-- \"organizations\/123456789\".
baecParent :: Lens' BillingAccountsExclusionsCreate Text
baecParent
  = lens _baecParent (\ s a -> s{_baecParent = a})

-- | V1 error format.
baecXgafv :: Lens' BillingAccountsExclusionsCreate (Maybe Xgafv)
baecXgafv
  = lens _baecXgafv (\ s a -> s{_baecXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
baecUploadProtocol :: Lens' BillingAccountsExclusionsCreate (Maybe Text)
baecUploadProtocol
  = lens _baecUploadProtocol
      (\ s a -> s{_baecUploadProtocol = a})

-- | Pretty-print response.
baecPp :: Lens' BillingAccountsExclusionsCreate Bool
baecPp = lens _baecPp (\ s a -> s{_baecPp = a})

-- | OAuth access token.
baecAccessToken :: Lens' BillingAccountsExclusionsCreate (Maybe Text)
baecAccessToken
  = lens _baecAccessToken
      (\ s a -> s{_baecAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
baecUploadType :: Lens' BillingAccountsExclusionsCreate (Maybe Text)
baecUploadType
  = lens _baecUploadType
      (\ s a -> s{_baecUploadType = a})

-- | Multipart request metadata.
baecPayload :: Lens' BillingAccountsExclusionsCreate LogExclusion
baecPayload
  = lens _baecPayload (\ s a -> s{_baecPayload = a})

-- | OAuth bearer token.
baecBearerToken :: Lens' BillingAccountsExclusionsCreate (Maybe Text)
baecBearerToken
  = lens _baecBearerToken
      (\ s a -> s{_baecBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
baecFields :: Lens' BillingAccountsExclusionsCreate (Maybe Text)
baecFields
  = lens _baecFields (\ s a -> s{_baecFields = a})

-- | JSONP
baecCallback :: Lens' BillingAccountsExclusionsCreate (Maybe Text)
baecCallback
  = lens _baecCallback (\ s a -> s{_baecCallback = a})

instance GoogleRequest
         BillingAccountsExclusionsCreate where
        type Rs BillingAccountsExclusionsCreate =
             LogExclusion
        type Scopes BillingAccountsExclusionsCreate =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/logging.admin"]
        requestClient BillingAccountsExclusionsCreate'{..}
          = go _baecParent _baecXgafv _baecUploadProtocol
              (Just _baecPp)
              _baecAccessToken
              _baecUploadType
              _baecBearerToken
              _baecCallback
              _baecFields
              (Just AltJSON)
              _baecPayload
              loggingService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy BillingAccountsExclusionsCreateResource)
                      mempty
