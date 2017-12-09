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
-- Module      : Network.Google.Resource.Logging.Organizations.Exclusions.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the description of an exclusion.
--
-- /See:/ <https://cloud.google.com/logging/docs/ Stackdriver Logging API Reference> for @logging.organizations.exclusions.get@.
module Network.Google.Resource.Logging.Organizations.Exclusions.Get
    (
    -- * REST Resource
      OrganizationsExclusionsGetResource

    -- * Creating a Request
    , organizationsExclusionsGet
    , OrganizationsExclusionsGet

    -- * Request Lenses
    , oegXgafv
    , oegUploadProtocol
    , oegPp
    , oegAccessToken
    , oegUploadType
    , oegBearerToken
    , oegName
    , oegFields
    , oegCallback
    ) where

import Network.Google.Logging.Types
import Network.Google.Prelude

-- | A resource alias for @logging.organizations.exclusions.get@ method which the
-- 'OrganizationsExclusionsGet' request conforms to.
type OrganizationsExclusionsGetResource =
     "v2" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :> Get '[JSON] LogExclusion

-- | Gets the description of an exclusion.
--
-- /See:/ 'organizationsExclusionsGet' smart constructor.
data OrganizationsExclusionsGet = OrganizationsExclusionsGet'
    { _oegXgafv :: !(Maybe Xgafv)
    , _oegUploadProtocol :: !(Maybe Text)
    , _oegPp :: !Bool
    , _oegAccessToken :: !(Maybe Text)
    , _oegUploadType :: !(Maybe Text)
    , _oegBearerToken :: !(Maybe Text)
    , _oegName :: !Text
    , _oegFields :: !(Maybe Text)
    , _oegCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrganizationsExclusionsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oegXgafv'
--
-- * 'oegUploadProtocol'
--
-- * 'oegPp'
--
-- * 'oegAccessToken'
--
-- * 'oegUploadType'
--
-- * 'oegBearerToken'
--
-- * 'oegName'
--
-- * 'oegFields'
--
-- * 'oegCallback'
organizationsExclusionsGet
    :: Text -- ^ 'oegName'
    -> OrganizationsExclusionsGet
organizationsExclusionsGet pOegName_ = 
    OrganizationsExclusionsGet'
    { _oegXgafv = Nothing
    , _oegUploadProtocol = Nothing
    , _oegPp = True
    , _oegAccessToken = Nothing
    , _oegUploadType = Nothing
    , _oegBearerToken = Nothing
    , _oegName = pOegName_
    , _oegFields = Nothing
    , _oegCallback = Nothing
    }

-- | V1 error format.
oegXgafv :: Lens' OrganizationsExclusionsGet (Maybe Xgafv)
oegXgafv = lens _oegXgafv (\ s a -> s{_oegXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
oegUploadProtocol :: Lens' OrganizationsExclusionsGet (Maybe Text)
oegUploadProtocol
  = lens _oegUploadProtocol
      (\ s a -> s{_oegUploadProtocol = a})

-- | Pretty-print response.
oegPp :: Lens' OrganizationsExclusionsGet Bool
oegPp = lens _oegPp (\ s a -> s{_oegPp = a})

-- | OAuth access token.
oegAccessToken :: Lens' OrganizationsExclusionsGet (Maybe Text)
oegAccessToken
  = lens _oegAccessToken
      (\ s a -> s{_oegAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
oegUploadType :: Lens' OrganizationsExclusionsGet (Maybe Text)
oegUploadType
  = lens _oegUploadType
      (\ s a -> s{_oegUploadType = a})

-- | OAuth bearer token.
oegBearerToken :: Lens' OrganizationsExclusionsGet (Maybe Text)
oegBearerToken
  = lens _oegBearerToken
      (\ s a -> s{_oegBearerToken = a})

-- | Required. The resource name of an existing exclusion:
-- \"projects\/[PROJECT_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"organizations\/[ORGANIZATION_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"billingAccounts\/[BILLING_ACCOUNT_ID]\/exclusions\/[EXCLUSION_ID]\"
-- \"folders\/[FOLDER_ID]\/exclusions\/[EXCLUSION_ID]\" Example:
-- \"projects\/my-project-id\/exclusions\/my-exclusion-id\".
oegName :: Lens' OrganizationsExclusionsGet Text
oegName = lens _oegName (\ s a -> s{_oegName = a})

-- | Selector specifying which fields to include in a partial response.
oegFields :: Lens' OrganizationsExclusionsGet (Maybe Text)
oegFields
  = lens _oegFields (\ s a -> s{_oegFields = a})

-- | JSONP
oegCallback :: Lens' OrganizationsExclusionsGet (Maybe Text)
oegCallback
  = lens _oegCallback (\ s a -> s{_oegCallback = a})

instance GoogleRequest OrganizationsExclusionsGet
         where
        type Rs OrganizationsExclusionsGet = LogExclusion
        type Scopes OrganizationsExclusionsGet =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/cloud-platform.read-only",
               "https://www.googleapis.com/auth/logging.admin",
               "https://www.googleapis.com/auth/logging.read"]
        requestClient OrganizationsExclusionsGet'{..}
          = go _oegName _oegXgafv _oegUploadProtocol
              (Just _oegPp)
              _oegAccessToken
              _oegUploadType
              _oegBearerToken
              _oegCallback
              _oegFields
              (Just AltJSON)
              loggingService
          where go
                  = buildClient
                      (Proxy :: Proxy OrganizationsExclusionsGetResource)
                      mempty
