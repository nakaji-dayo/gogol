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
-- Module      : Network.Google.Resource.Partners.Exams.GetToken
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Exam Token for a Partner\'s user to take an exam in the Exams
-- System
--
-- /See:/ <https://developers.google.com/partners/ Google Partners API Reference> for @partners.exams.getToken@.
module Network.Google.Resource.Partners.Exams.GetToken
    (
    -- * REST Resource
      ExamsGetTokenResource

    -- * Creating a Request
    , examsGetToken
    , ExamsGetToken

    -- * Request Lenses
    , egtXgafv
    , egtUploadProtocol
    , egtPp
    , egtAccessToken
    , egtUploadType
    , egtRequestMetadataPartnersSessionId
    , egtBearerToken
    , egtRequestMetadataLocale
    , egtRequestMetadataExperimentIds
    , egtRequestMetadataUserOverridesIPAddress
    , egtRequestMetadataTrafficSourceTrafficSubId
    , egtRequestMetadataUserOverridesUserId
    , egtRequestMetadataTrafficSourceTrafficSourceId
    , egtFields
    , egtCallback
    , egtExamType
    ) where

import Network.Google.Partners.Types
import Network.Google.Prelude

-- | A resource alias for @partners.exams.getToken@ method which the
-- 'ExamsGetToken' request conforms to.
type ExamsGetTokenResource =
     "v2" :>
       "exams" :>
         Capture "examType" Text :>
           "token" :>
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
                               QueryParam
                                 "requestMetadata.userOverrides.ipAddress"
                                 Text
                                 :>
                                 QueryParam
                                   "requestMetadata.trafficSource.trafficSubId"
                                   Text
                                   :>
                                   QueryParam
                                     "requestMetadata.userOverrides.userId"
                                     Text
                                     :>
                                     QueryParam
                                       "requestMetadata.trafficSource.trafficSourceId"
                                       Text
                                       :>
                                       QueryParam "callback" Text :>
                                         QueryParam "fields" Text :>
                                           QueryParam "alt" AltJSON :>
                                             Get '[JSON] ExamToken

-- | Gets an Exam Token for a Partner\'s user to take an exam in the Exams
-- System
--
-- /See:/ 'examsGetToken' smart constructor.
data ExamsGetToken = ExamsGetToken'
    { _egtXgafv :: !(Maybe Xgafv)
    , _egtUploadProtocol :: !(Maybe Text)
    , _egtPp :: !Bool
    , _egtAccessToken :: !(Maybe Text)
    , _egtUploadType :: !(Maybe Text)
    , _egtRequestMetadataPartnersSessionId :: !(Maybe Text)
    , _egtBearerToken :: !(Maybe Text)
    , _egtRequestMetadataLocale :: !(Maybe Text)
    , _egtRequestMetadataExperimentIds :: !(Maybe [Text])
    , _egtRequestMetadataUserOverridesIPAddress :: !(Maybe Text)
    , _egtRequestMetadataTrafficSourceTrafficSubId :: !(Maybe Text)
    , _egtRequestMetadataUserOverridesUserId :: !(Maybe Text)
    , _egtRequestMetadataTrafficSourceTrafficSourceId :: !(Maybe Text)
    , _egtFields :: !(Maybe Text)
    , _egtCallback :: !(Maybe Text)
    , _egtExamType :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExamsGetToken' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'egtXgafv'
--
-- * 'egtUploadProtocol'
--
-- * 'egtPp'
--
-- * 'egtAccessToken'
--
-- * 'egtUploadType'
--
-- * 'egtRequestMetadataPartnersSessionId'
--
-- * 'egtBearerToken'
--
-- * 'egtRequestMetadataLocale'
--
-- * 'egtRequestMetadataExperimentIds'
--
-- * 'egtRequestMetadataUserOverridesIPAddress'
--
-- * 'egtRequestMetadataTrafficSourceTrafficSubId'
--
-- * 'egtRequestMetadataUserOverridesUserId'
--
-- * 'egtRequestMetadataTrafficSourceTrafficSourceId'
--
-- * 'egtFields'
--
-- * 'egtCallback'
--
-- * 'egtExamType'
examsGetToken
    :: Text -- ^ 'egtExamType'
    -> ExamsGetToken
examsGetToken pEgtExamType_ = 
    ExamsGetToken'
    { _egtXgafv = Nothing
    , _egtUploadProtocol = Nothing
    , _egtPp = True
    , _egtAccessToken = Nothing
    , _egtUploadType = Nothing
    , _egtRequestMetadataPartnersSessionId = Nothing
    , _egtBearerToken = Nothing
    , _egtRequestMetadataLocale = Nothing
    , _egtRequestMetadataExperimentIds = Nothing
    , _egtRequestMetadataUserOverridesIPAddress = Nothing
    , _egtRequestMetadataTrafficSourceTrafficSubId = Nothing
    , _egtRequestMetadataUserOverridesUserId = Nothing
    , _egtRequestMetadataTrafficSourceTrafficSourceId = Nothing
    , _egtFields = Nothing
    , _egtCallback = Nothing
    , _egtExamType = pEgtExamType_
    }

-- | V1 error format.
egtXgafv :: Lens' ExamsGetToken (Maybe Xgafv)
egtXgafv = lens _egtXgafv (\ s a -> s{_egtXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
egtUploadProtocol :: Lens' ExamsGetToken (Maybe Text)
egtUploadProtocol
  = lens _egtUploadProtocol
      (\ s a -> s{_egtUploadProtocol = a})

-- | Pretty-print response.
egtPp :: Lens' ExamsGetToken Bool
egtPp = lens _egtPp (\ s a -> s{_egtPp = a})

-- | OAuth access token.
egtAccessToken :: Lens' ExamsGetToken (Maybe Text)
egtAccessToken
  = lens _egtAccessToken
      (\ s a -> s{_egtAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
egtUploadType :: Lens' ExamsGetToken (Maybe Text)
egtUploadType
  = lens _egtUploadType
      (\ s a -> s{_egtUploadType = a})

-- | Google Partners session ID.
egtRequestMetadataPartnersSessionId :: Lens' ExamsGetToken (Maybe Text)
egtRequestMetadataPartnersSessionId
  = lens _egtRequestMetadataPartnersSessionId
      (\ s a ->
         s{_egtRequestMetadataPartnersSessionId = a})

-- | OAuth bearer token.
egtBearerToken :: Lens' ExamsGetToken (Maybe Text)
egtBearerToken
  = lens _egtBearerToken
      (\ s a -> s{_egtBearerToken = a})

-- | Locale to use for the current request.
egtRequestMetadataLocale :: Lens' ExamsGetToken (Maybe Text)
egtRequestMetadataLocale
  = lens _egtRequestMetadataLocale
      (\ s a -> s{_egtRequestMetadataLocale = a})

-- | Experiment IDs the current request belongs to.
egtRequestMetadataExperimentIds :: Lens' ExamsGetToken [Text]
egtRequestMetadataExperimentIds
  = lens _egtRequestMetadataExperimentIds
      (\ s a -> s{_egtRequestMetadataExperimentIds = a})
      . _Default
      . _Coerce

-- | IP address to use instead of the user\'s geo-located IP address.
egtRequestMetadataUserOverridesIPAddress :: Lens' ExamsGetToken (Maybe Text)
egtRequestMetadataUserOverridesIPAddress
  = lens _egtRequestMetadataUserOverridesIPAddress
      (\ s a ->
         s{_egtRequestMetadataUserOverridesIPAddress = a})

-- | Second level identifier to indicate where the traffic comes from. An
-- identifier has multiple letters created by a team which redirected the
-- traffic to us.
egtRequestMetadataTrafficSourceTrafficSubId :: Lens' ExamsGetToken (Maybe Text)
egtRequestMetadataTrafficSourceTrafficSubId
  = lens _egtRequestMetadataTrafficSourceTrafficSubId
      (\ s a ->
         s{_egtRequestMetadataTrafficSourceTrafficSubId = a})

-- | Logged-in user ID to impersonate instead of the user\'s ID.
egtRequestMetadataUserOverridesUserId :: Lens' ExamsGetToken (Maybe Text)
egtRequestMetadataUserOverridesUserId
  = lens _egtRequestMetadataUserOverridesUserId
      (\ s a ->
         s{_egtRequestMetadataUserOverridesUserId = a})

-- | Identifier to indicate where the traffic comes from. An identifier has
-- multiple letters created by a team which redirected the traffic to us.
egtRequestMetadataTrafficSourceTrafficSourceId :: Lens' ExamsGetToken (Maybe Text)
egtRequestMetadataTrafficSourceTrafficSourceId
  = lens
      _egtRequestMetadataTrafficSourceTrafficSourceId
      (\ s a ->
         s{_egtRequestMetadataTrafficSourceTrafficSourceId =
             a})

-- | Selector specifying which fields to include in a partial response.
egtFields :: Lens' ExamsGetToken (Maybe Text)
egtFields
  = lens _egtFields (\ s a -> s{_egtFields = a})

-- | JSONP
egtCallback :: Lens' ExamsGetToken (Maybe Text)
egtCallback
  = lens _egtCallback (\ s a -> s{_egtCallback = a})

-- | The exam type we are requesting a token for.
egtExamType :: Lens' ExamsGetToken Text
egtExamType
  = lens _egtExamType (\ s a -> s{_egtExamType = a})

instance GoogleRequest ExamsGetToken where
        type Rs ExamsGetToken = ExamToken
        type Scopes ExamsGetToken = '[]
        requestClient ExamsGetToken'{..}
          = go _egtExamType _egtXgafv _egtUploadProtocol
              (Just _egtPp)
              _egtAccessToken
              _egtUploadType
              _egtRequestMetadataPartnersSessionId
              _egtBearerToken
              _egtRequestMetadataLocale
              (_egtRequestMetadataExperimentIds ^. _Default)
              _egtRequestMetadataUserOverridesIPAddress
              _egtRequestMetadataTrafficSourceTrafficSubId
              _egtRequestMetadataUserOverridesUserId
              _egtRequestMetadataTrafficSourceTrafficSourceId
              _egtCallback
              _egtFields
              (Just AltJSON)
              partnersService
          where go
                  = buildClient (Proxy :: Proxy ExamsGetTokenResource)
                      mempty
