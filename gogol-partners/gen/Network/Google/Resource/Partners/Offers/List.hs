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
-- Module      : Network.Google.Resource.Partners.Offers.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Offers available for the current user
--
-- /See:/ <https://developers.google.com/partners/ Google Partners API Reference> for @partners.offers.list@.
module Network.Google.Resource.Partners.Offers.List
    (
    -- * REST Resource
      OffersListResource

    -- * Creating a Request
    , offersList
    , OffersList

    -- * Request Lenses
    , olXgafv
    , olUploadProtocol
    , olPp
    , olAccessToken
    , olUploadType
    , olRequestMetadataPartnersSessionId
    , olBearerToken
    , olRequestMetadataLocale
    , olRequestMetadataExperimentIds
    , olRequestMetadataUserOverridesIPAddress
    , olRequestMetadataTrafficSourceTrafficSubId
    , olRequestMetadataUserOverridesUserId
    , olRequestMetadataTrafficSourceTrafficSourceId
    , olFields
    , olCallback
    ) where

import Network.Google.Partners.Types
import Network.Google.Prelude

-- | A resource alias for @partners.offers.list@ method which the
-- 'OffersList' request conforms to.
type OffersListResource =
     "v2" :>
       "offers" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "requestMetadata.partnersSessionId" Text
                     :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "requestMetadata.locale" Text :>
                         QueryParams "requestMetadata.experimentIds" Text :>
                           QueryParam "requestMetadata.userOverrides.ipAddress"
                             Text
                             :>
                             QueryParam
                               "requestMetadata.trafficSource.trafficSubId"
                               Text
                               :>
                               QueryParam "requestMetadata.userOverrides.userId"
                                 Text
                                 :>
                                 QueryParam
                                   "requestMetadata.trafficSource.trafficSourceId"
                                   Text
                                   :>
                                   QueryParam "callback" Text :>
                                     QueryParam "fields" Text :>
                                       QueryParam "alt" AltJSON :>
                                         Get '[JSON] ListOffersResponse

-- | Lists the Offers available for the current user
--
-- /See:/ 'offersList' smart constructor.
data OffersList = OffersList'
    { _olXgafv :: !(Maybe Xgafv)
    , _olUploadProtocol :: !(Maybe Text)
    , _olPp :: !Bool
    , _olAccessToken :: !(Maybe Text)
    , _olUploadType :: !(Maybe Text)
    , _olRequestMetadataPartnersSessionId :: !(Maybe Text)
    , _olBearerToken :: !(Maybe Text)
    , _olRequestMetadataLocale :: !(Maybe Text)
    , _olRequestMetadataExperimentIds :: !(Maybe [Text])
    , _olRequestMetadataUserOverridesIPAddress :: !(Maybe Text)
    , _olRequestMetadataTrafficSourceTrafficSubId :: !(Maybe Text)
    , _olRequestMetadataUserOverridesUserId :: !(Maybe Text)
    , _olRequestMetadataTrafficSourceTrafficSourceId :: !(Maybe Text)
    , _olFields :: !(Maybe Text)
    , _olCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OffersList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olXgafv'
--
-- * 'olUploadProtocol'
--
-- * 'olPp'
--
-- * 'olAccessToken'
--
-- * 'olUploadType'
--
-- * 'olRequestMetadataPartnersSessionId'
--
-- * 'olBearerToken'
--
-- * 'olRequestMetadataLocale'
--
-- * 'olRequestMetadataExperimentIds'
--
-- * 'olRequestMetadataUserOverridesIPAddress'
--
-- * 'olRequestMetadataTrafficSourceTrafficSubId'
--
-- * 'olRequestMetadataUserOverridesUserId'
--
-- * 'olRequestMetadataTrafficSourceTrafficSourceId'
--
-- * 'olFields'
--
-- * 'olCallback'
offersList
    :: OffersList
offersList = 
    OffersList'
    { _olXgafv = Nothing
    , _olUploadProtocol = Nothing
    , _olPp = True
    , _olAccessToken = Nothing
    , _olUploadType = Nothing
    , _olRequestMetadataPartnersSessionId = Nothing
    , _olBearerToken = Nothing
    , _olRequestMetadataLocale = Nothing
    , _olRequestMetadataExperimentIds = Nothing
    , _olRequestMetadataUserOverridesIPAddress = Nothing
    , _olRequestMetadataTrafficSourceTrafficSubId = Nothing
    , _olRequestMetadataUserOverridesUserId = Nothing
    , _olRequestMetadataTrafficSourceTrafficSourceId = Nothing
    , _olFields = Nothing
    , _olCallback = Nothing
    }

-- | V1 error format.
olXgafv :: Lens' OffersList (Maybe Xgafv)
olXgafv = lens _olXgafv (\ s a -> s{_olXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
olUploadProtocol :: Lens' OffersList (Maybe Text)
olUploadProtocol
  = lens _olUploadProtocol
      (\ s a -> s{_olUploadProtocol = a})

-- | Pretty-print response.
olPp :: Lens' OffersList Bool
olPp = lens _olPp (\ s a -> s{_olPp = a})

-- | OAuth access token.
olAccessToken :: Lens' OffersList (Maybe Text)
olAccessToken
  = lens _olAccessToken
      (\ s a -> s{_olAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
olUploadType :: Lens' OffersList (Maybe Text)
olUploadType
  = lens _olUploadType (\ s a -> s{_olUploadType = a})

-- | Google Partners session ID.
olRequestMetadataPartnersSessionId :: Lens' OffersList (Maybe Text)
olRequestMetadataPartnersSessionId
  = lens _olRequestMetadataPartnersSessionId
      (\ s a -> s{_olRequestMetadataPartnersSessionId = a})

-- | OAuth bearer token.
olBearerToken :: Lens' OffersList (Maybe Text)
olBearerToken
  = lens _olBearerToken
      (\ s a -> s{_olBearerToken = a})

-- | Locale to use for the current request.
olRequestMetadataLocale :: Lens' OffersList (Maybe Text)
olRequestMetadataLocale
  = lens _olRequestMetadataLocale
      (\ s a -> s{_olRequestMetadataLocale = a})

-- | Experiment IDs the current request belongs to.
olRequestMetadataExperimentIds :: Lens' OffersList [Text]
olRequestMetadataExperimentIds
  = lens _olRequestMetadataExperimentIds
      (\ s a -> s{_olRequestMetadataExperimentIds = a})
      . _Default
      . _Coerce

-- | IP address to use instead of the user\'s geo-located IP address.
olRequestMetadataUserOverridesIPAddress :: Lens' OffersList (Maybe Text)
olRequestMetadataUserOverridesIPAddress
  = lens _olRequestMetadataUserOverridesIPAddress
      (\ s a ->
         s{_olRequestMetadataUserOverridesIPAddress = a})

-- | Second level identifier to indicate where the traffic comes from. An
-- identifier has multiple letters created by a team which redirected the
-- traffic to us.
olRequestMetadataTrafficSourceTrafficSubId :: Lens' OffersList (Maybe Text)
olRequestMetadataTrafficSourceTrafficSubId
  = lens _olRequestMetadataTrafficSourceTrafficSubId
      (\ s a ->
         s{_olRequestMetadataTrafficSourceTrafficSubId = a})

-- | Logged-in user ID to impersonate instead of the user\'s ID.
olRequestMetadataUserOverridesUserId :: Lens' OffersList (Maybe Text)
olRequestMetadataUserOverridesUserId
  = lens _olRequestMetadataUserOverridesUserId
      (\ s a ->
         s{_olRequestMetadataUserOverridesUserId = a})

-- | Identifier to indicate where the traffic comes from. An identifier has
-- multiple letters created by a team which redirected the traffic to us.
olRequestMetadataTrafficSourceTrafficSourceId :: Lens' OffersList (Maybe Text)
olRequestMetadataTrafficSourceTrafficSourceId
  = lens _olRequestMetadataTrafficSourceTrafficSourceId
      (\ s a ->
         s{_olRequestMetadataTrafficSourceTrafficSourceId =
             a})

-- | Selector specifying which fields to include in a partial response.
olFields :: Lens' OffersList (Maybe Text)
olFields = lens _olFields (\ s a -> s{_olFields = a})

-- | JSONP
olCallback :: Lens' OffersList (Maybe Text)
olCallback
  = lens _olCallback (\ s a -> s{_olCallback = a})

instance GoogleRequest OffersList where
        type Rs OffersList = ListOffersResponse
        type Scopes OffersList = '[]
        requestClient OffersList'{..}
          = go _olXgafv _olUploadProtocol (Just _olPp)
              _olAccessToken
              _olUploadType
              _olRequestMetadataPartnersSessionId
              _olBearerToken
              _olRequestMetadataLocale
              (_olRequestMetadataExperimentIds ^. _Default)
              _olRequestMetadataUserOverridesIPAddress
              _olRequestMetadataTrafficSourceTrafficSubId
              _olRequestMetadataUserOverridesUserId
              _olRequestMetadataTrafficSourceTrafficSourceId
              _olCallback
              _olFields
              (Just AltJSON)
              partnersService
          where go
                  = buildClient (Proxy :: Proxy OffersListResource)
                      mempty
