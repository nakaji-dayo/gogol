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
-- Module      : Network.Google.Resource.Logging.Organizations.Exclusions.Create
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
-- /See:/ <https://cloud.google.com/logging/docs/ Stackdriver Logging API Reference> for @logging.organizations.exclusions.create@.
module Network.Google.Resource.Logging.Organizations.Exclusions.Create
    (
    -- * REST Resource
      OrganizationsExclusionsCreateResource

    -- * Creating a Request
    , organizationsExclusionsCreate
    , OrganizationsExclusionsCreate

    -- * Request Lenses
    , oecParent
    , oecXgafv
    , oecUploadProtocol
    , oecPp
    , oecAccessToken
    , oecUploadType
    , oecPayload
    , oecBearerToken
    , oecFields
    , oecCallback
    ) where

import Network.Google.Logging.Types
import Network.Google.Prelude

-- | A resource alias for @logging.organizations.exclusions.create@ method which the
-- 'OrganizationsExclusionsCreate' request conforms to.
type OrganizationsExclusionsCreateResource =
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
-- /See:/ 'organizationsExclusionsCreate' smart constructor.
data OrganizationsExclusionsCreate = OrganizationsExclusionsCreate'
    { _oecParent :: !Text
    , _oecXgafv :: !(Maybe Xgafv)
    , _oecUploadProtocol :: !(Maybe Text)
    , _oecPp :: !Bool
    , _oecAccessToken :: !(Maybe Text)
    , _oecUploadType :: !(Maybe Text)
    , _oecPayload :: !LogExclusion
    , _oecBearerToken :: !(Maybe Text)
    , _oecFields :: !(Maybe Text)
    , _oecCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrganizationsExclusionsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oecParent'
--
-- * 'oecXgafv'
--
-- * 'oecUploadProtocol'
--
-- * 'oecPp'
--
-- * 'oecAccessToken'
--
-- * 'oecUploadType'
--
-- * 'oecPayload'
--
-- * 'oecBearerToken'
--
-- * 'oecFields'
--
-- * 'oecCallback'
organizationsExclusionsCreate
    :: Text -- ^ 'oecParent'
    -> LogExclusion -- ^ 'oecPayload'
    -> OrganizationsExclusionsCreate
organizationsExclusionsCreate pOecParent_ pOecPayload_ = 
    OrganizationsExclusionsCreate'
    { _oecParent = pOecParent_
    , _oecXgafv = Nothing
    , _oecUploadProtocol = Nothing
    , _oecPp = True
    , _oecAccessToken = Nothing
    , _oecUploadType = Nothing
    , _oecPayload = pOecPayload_
    , _oecBearerToken = Nothing
    , _oecFields = Nothing
    , _oecCallback = Nothing
    }

-- | Required. The parent resource in which to create the exclusion:
-- \"projects\/[PROJECT_ID]\" \"organizations\/[ORGANIZATION_ID]\"
-- \"billingAccounts\/[BILLING_ACCOUNT_ID]\" \"folders\/[FOLDER_ID]\"
-- Examples: \"projects\/my-logging-project\",
-- \"organizations\/123456789\".
oecParent :: Lens' OrganizationsExclusionsCreate Text
oecParent
  = lens _oecParent (\ s a -> s{_oecParent = a})

-- | V1 error format.
oecXgafv :: Lens' OrganizationsExclusionsCreate (Maybe Xgafv)
oecXgafv = lens _oecXgafv (\ s a -> s{_oecXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
oecUploadProtocol :: Lens' OrganizationsExclusionsCreate (Maybe Text)
oecUploadProtocol
  = lens _oecUploadProtocol
      (\ s a -> s{_oecUploadProtocol = a})

-- | Pretty-print response.
oecPp :: Lens' OrganizationsExclusionsCreate Bool
oecPp = lens _oecPp (\ s a -> s{_oecPp = a})

-- | OAuth access token.
oecAccessToken :: Lens' OrganizationsExclusionsCreate (Maybe Text)
oecAccessToken
  = lens _oecAccessToken
      (\ s a -> s{_oecAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
oecUploadType :: Lens' OrganizationsExclusionsCreate (Maybe Text)
oecUploadType
  = lens _oecUploadType
      (\ s a -> s{_oecUploadType = a})

-- | Multipart request metadata.
oecPayload :: Lens' OrganizationsExclusionsCreate LogExclusion
oecPayload
  = lens _oecPayload (\ s a -> s{_oecPayload = a})

-- | OAuth bearer token.
oecBearerToken :: Lens' OrganizationsExclusionsCreate (Maybe Text)
oecBearerToken
  = lens _oecBearerToken
      (\ s a -> s{_oecBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
oecFields :: Lens' OrganizationsExclusionsCreate (Maybe Text)
oecFields
  = lens _oecFields (\ s a -> s{_oecFields = a})

-- | JSONP
oecCallback :: Lens' OrganizationsExclusionsCreate (Maybe Text)
oecCallback
  = lens _oecCallback (\ s a -> s{_oecCallback = a})

instance GoogleRequest OrganizationsExclusionsCreate
         where
        type Rs OrganizationsExclusionsCreate = LogExclusion
        type Scopes OrganizationsExclusionsCreate =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/logging.admin"]
        requestClient OrganizationsExclusionsCreate'{..}
          = go _oecParent _oecXgafv _oecUploadProtocol
              (Just _oecPp)
              _oecAccessToken
              _oecUploadType
              _oecBearerToken
              _oecCallback
              _oecFields
              (Just AltJSON)
              _oecPayload
              loggingService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy OrganizationsExclusionsCreateResource)
                      mempty
