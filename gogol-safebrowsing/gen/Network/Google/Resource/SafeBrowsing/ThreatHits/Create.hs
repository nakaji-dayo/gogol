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
-- Module      : Network.Google.Resource.SafeBrowsing.ThreatHits.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reports a Safe Browsing threat list hit to Google. Only projects with
-- TRUSTED_REPORTER visibility can use this method.
--
-- /See:/ <https://developers.google.com/safe-browsing/ Google Safe Browsing API Reference> for @safebrowsing.threatHits.create@.
module Network.Google.Resource.SafeBrowsing.ThreatHits.Create
    (
    -- * REST Resource
      ThreatHitsCreateResource

    -- * Creating a Request
    , threatHitsCreate
    , ThreatHitsCreate

    -- * Request Lenses
    , thcXgafv
    , thcUploadProtocol
    , thcPp
    , thcAccessToken
    , thcUploadType
    , thcPayload
    , thcBearerToken
    , thcFields
    , thcCallback
    ) where

import Network.Google.Prelude
import Network.Google.SafeBrowsing.Types

-- | A resource alias for @safebrowsing.threatHits.create@ method which the
-- 'ThreatHitsCreate' request conforms to.
type ThreatHitsCreateResource =
     "v4" :>
       "threatHits" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] ThreatHit :> Post '[JSON] Empty

-- | Reports a Safe Browsing threat list hit to Google. Only projects with
-- TRUSTED_REPORTER visibility can use this method.
--
-- /See:/ 'threatHitsCreate' smart constructor.
data ThreatHitsCreate = ThreatHitsCreate'
    { _thcXgafv :: !(Maybe Xgafv)
    , _thcUploadProtocol :: !(Maybe Text)
    , _thcPp :: !Bool
    , _thcAccessToken :: !(Maybe Text)
    , _thcUploadType :: !(Maybe Text)
    , _thcPayload :: !ThreatHit
    , _thcBearerToken :: !(Maybe Text)
    , _thcFields :: !(Maybe Text)
    , _thcCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ThreatHitsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'thcXgafv'
--
-- * 'thcUploadProtocol'
--
-- * 'thcPp'
--
-- * 'thcAccessToken'
--
-- * 'thcUploadType'
--
-- * 'thcPayload'
--
-- * 'thcBearerToken'
--
-- * 'thcFields'
--
-- * 'thcCallback'
threatHitsCreate
    :: ThreatHit -- ^ 'thcPayload'
    -> ThreatHitsCreate
threatHitsCreate pThcPayload_ = 
    ThreatHitsCreate'
    { _thcXgafv = Nothing
    , _thcUploadProtocol = Nothing
    , _thcPp = True
    , _thcAccessToken = Nothing
    , _thcUploadType = Nothing
    , _thcPayload = pThcPayload_
    , _thcBearerToken = Nothing
    , _thcFields = Nothing
    , _thcCallback = Nothing
    }

-- | V1 error format.
thcXgafv :: Lens' ThreatHitsCreate (Maybe Xgafv)
thcXgafv = lens _thcXgafv (\ s a -> s{_thcXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
thcUploadProtocol :: Lens' ThreatHitsCreate (Maybe Text)
thcUploadProtocol
  = lens _thcUploadProtocol
      (\ s a -> s{_thcUploadProtocol = a})

-- | Pretty-print response.
thcPp :: Lens' ThreatHitsCreate Bool
thcPp = lens _thcPp (\ s a -> s{_thcPp = a})

-- | OAuth access token.
thcAccessToken :: Lens' ThreatHitsCreate (Maybe Text)
thcAccessToken
  = lens _thcAccessToken
      (\ s a -> s{_thcAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
thcUploadType :: Lens' ThreatHitsCreate (Maybe Text)
thcUploadType
  = lens _thcUploadType
      (\ s a -> s{_thcUploadType = a})

-- | Multipart request metadata.
thcPayload :: Lens' ThreatHitsCreate ThreatHit
thcPayload
  = lens _thcPayload (\ s a -> s{_thcPayload = a})

-- | OAuth bearer token.
thcBearerToken :: Lens' ThreatHitsCreate (Maybe Text)
thcBearerToken
  = lens _thcBearerToken
      (\ s a -> s{_thcBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
thcFields :: Lens' ThreatHitsCreate (Maybe Text)
thcFields
  = lens _thcFields (\ s a -> s{_thcFields = a})

-- | JSONP
thcCallback :: Lens' ThreatHitsCreate (Maybe Text)
thcCallback
  = lens _thcCallback (\ s a -> s{_thcCallback = a})

instance GoogleRequest ThreatHitsCreate where
        type Rs ThreatHitsCreate = Empty
        type Scopes ThreatHitsCreate = '[]
        requestClient ThreatHitsCreate'{..}
          = go _thcXgafv _thcUploadProtocol (Just _thcPp)
              _thcAccessToken
              _thcUploadType
              _thcBearerToken
              _thcCallback
              _thcFields
              (Just AltJSON)
              _thcPayload
              safeBrowsingService
          where go
                  = buildClient
                      (Proxy :: Proxy ThreatHitsCreateResource)
                      mempty
