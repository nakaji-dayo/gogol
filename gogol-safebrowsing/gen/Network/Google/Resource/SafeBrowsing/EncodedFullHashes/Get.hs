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
-- Module      : Network.Google.Resource.SafeBrowsing.EncodedFullHashes.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- |
--
-- /See:/ <https://developers.google.com/safe-browsing/ Google Safe Browsing API Reference> for @safebrowsing.encodedFullHashes.get@.
module Network.Google.Resource.SafeBrowsing.EncodedFullHashes.Get
    (
    -- * REST Resource
      EncodedFullHashesGetResource

    -- * Creating a Request
    , encodedFullHashesGet
    , EncodedFullHashesGet

    -- * Request Lenses
    , efhgClientId
    , efhgXgafv
    , efhgUploadProtocol
    , efhgPp
    , efhgAccessToken
    , efhgEncodedRequest
    , efhgUploadType
    , efhgBearerToken
    , efhgClientVersion
    , efhgFields
    , efhgCallback
    ) where

import Network.Google.Prelude
import Network.Google.SafeBrowsing.Types

-- | A resource alias for @safebrowsing.encodedFullHashes.get@ method which the
-- 'EncodedFullHashesGet' request conforms to.
type EncodedFullHashesGetResource =
     "v4" :>
       "encodedFullHashes" :>
         Capture "encodedRequest" Bytes :>
           QueryParam "clientId" Text :>
             QueryParam "$.xgafv" Xgafv :>
               QueryParam "upload_protocol" Text :>
                 QueryParam "pp" Bool :>
                   QueryParam "access_token" Text :>
                     QueryParam "uploadType" Text :>
                       QueryParam "bearer_token" Text :>
                         QueryParam "clientVersion" Text :>
                           QueryParam "callback" Text :>
                             QueryParam "fields" Text :>
                               QueryParam "alt" AltJSON :>
                                 Get '[JSON] FindFullHashesResponse

-- |
--
-- /See:/ 'encodedFullHashesGet' smart constructor.
data EncodedFullHashesGet = EncodedFullHashesGet'
    { _efhgClientId :: !(Maybe Text)
    , _efhgXgafv :: !(Maybe Xgafv)
    , _efhgUploadProtocol :: !(Maybe Text)
    , _efhgPp :: !Bool
    , _efhgAccessToken :: !(Maybe Text)
    , _efhgEncodedRequest :: !Bytes
    , _efhgUploadType :: !(Maybe Text)
    , _efhgBearerToken :: !(Maybe Text)
    , _efhgClientVersion :: !(Maybe Text)
    , _efhgFields :: !(Maybe Text)
    , _efhgCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EncodedFullHashesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efhgClientId'
--
-- * 'efhgXgafv'
--
-- * 'efhgUploadProtocol'
--
-- * 'efhgPp'
--
-- * 'efhgAccessToken'
--
-- * 'efhgEncodedRequest'
--
-- * 'efhgUploadType'
--
-- * 'efhgBearerToken'
--
-- * 'efhgClientVersion'
--
-- * 'efhgFields'
--
-- * 'efhgCallback'
encodedFullHashesGet
    :: ByteString -- ^ 'efhgEncodedRequest'
    -> EncodedFullHashesGet
encodedFullHashesGet pEfhgEncodedRequest_ = 
    EncodedFullHashesGet'
    { _efhgClientId = Nothing
    , _efhgXgafv = Nothing
    , _efhgUploadProtocol = Nothing
    , _efhgPp = True
    , _efhgAccessToken = Nothing
    , _efhgEncodedRequest = _Bytes # pEfhgEncodedRequest_
    , _efhgUploadType = Nothing
    , _efhgBearerToken = Nothing
    , _efhgClientVersion = Nothing
    , _efhgFields = Nothing
    , _efhgCallback = Nothing
    }

-- | A client ID that (hopefully) uniquely identifies the client
-- implementation of the Safe Browsing API.
efhgClientId :: Lens' EncodedFullHashesGet (Maybe Text)
efhgClientId
  = lens _efhgClientId (\ s a -> s{_efhgClientId = a})

-- | V1 error format.
efhgXgafv :: Lens' EncodedFullHashesGet (Maybe Xgafv)
efhgXgafv
  = lens _efhgXgafv (\ s a -> s{_efhgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
efhgUploadProtocol :: Lens' EncodedFullHashesGet (Maybe Text)
efhgUploadProtocol
  = lens _efhgUploadProtocol
      (\ s a -> s{_efhgUploadProtocol = a})

-- | Pretty-print response.
efhgPp :: Lens' EncodedFullHashesGet Bool
efhgPp = lens _efhgPp (\ s a -> s{_efhgPp = a})

-- | OAuth access token.
efhgAccessToken :: Lens' EncodedFullHashesGet (Maybe Text)
efhgAccessToken
  = lens _efhgAccessToken
      (\ s a -> s{_efhgAccessToken = a})

-- | A serialized FindFullHashesRequest proto.
efhgEncodedRequest :: Lens' EncodedFullHashesGet ByteString
efhgEncodedRequest
  = lens _efhgEncodedRequest
      (\ s a -> s{_efhgEncodedRequest = a})
      . _Bytes

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
efhgUploadType :: Lens' EncodedFullHashesGet (Maybe Text)
efhgUploadType
  = lens _efhgUploadType
      (\ s a -> s{_efhgUploadType = a})

-- | OAuth bearer token.
efhgBearerToken :: Lens' EncodedFullHashesGet (Maybe Text)
efhgBearerToken
  = lens _efhgBearerToken
      (\ s a -> s{_efhgBearerToken = a})

-- | The version of the client implementation.
efhgClientVersion :: Lens' EncodedFullHashesGet (Maybe Text)
efhgClientVersion
  = lens _efhgClientVersion
      (\ s a -> s{_efhgClientVersion = a})

-- | Selector specifying which fields to include in a partial response.
efhgFields :: Lens' EncodedFullHashesGet (Maybe Text)
efhgFields
  = lens _efhgFields (\ s a -> s{_efhgFields = a})

-- | JSONP
efhgCallback :: Lens' EncodedFullHashesGet (Maybe Text)
efhgCallback
  = lens _efhgCallback (\ s a -> s{_efhgCallback = a})

instance GoogleRequest EncodedFullHashesGet where
        type Rs EncodedFullHashesGet = FindFullHashesResponse
        type Scopes EncodedFullHashesGet = '[]
        requestClient EncodedFullHashesGet'{..}
          = go _efhgEncodedRequest _efhgClientId _efhgXgafv
              _efhgUploadProtocol
              (Just _efhgPp)
              _efhgAccessToken
              _efhgUploadType
              _efhgBearerToken
              _efhgClientVersion
              _efhgCallback
              _efhgFields
              (Just AltJSON)
              safeBrowsingService
          where go
                  = buildClient
                      (Proxy :: Proxy EncodedFullHashesGetResource)
                      mempty
