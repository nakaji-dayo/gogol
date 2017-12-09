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
-- Module      : Network.Google.Resource.URLShortener.URL.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new short URL.
--
-- /See:/ <https://developers.google.com/url-shortener/v1/getting_started URL Shortener API Reference> for @urlshortener.url.insert@.
module Network.Google.Resource.URLShortener.URL.Insert
    (
    -- * REST Resource
      URLInsertResource

    -- * Creating a Request
    , urlInsert
    , URLInsert

    -- * Request Lenses
    , uiPayload
    , uiFields
    ) where

import Network.Google.Prelude
import Network.Google.URLShortener.Types

-- | A resource alias for @urlshortener.url.insert@ method which the
-- 'URLInsert' request conforms to.
type URLInsertResource =
     "urlshortener" :>
       "v1" :>
         "url" :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :>
               ReqBody '[JSON] URL :> Post '[JSON] URL

-- | Creates a new short URL.
--
-- /See:/ 'urlInsert' smart constructor.
data URLInsert = URLInsert'
    { _uiPayload :: !URL
    , _uiFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'URLInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiPayload'
--
-- * 'uiFields'
urlInsert
    :: URL -- ^ 'uiPayload'
    -> URLInsert
urlInsert pUiPayload_ = 
    URLInsert'
    { _uiPayload = pUiPayload_
    , _uiFields = Nothing
    }

-- | Multipart request metadata.
uiPayload :: Lens' URLInsert URL
uiPayload
  = lens _uiPayload (\ s a -> s{_uiPayload = a})

-- | Selector specifying which fields to include in a partial response.
uiFields :: Lens' URLInsert (Maybe Text)
uiFields = lens _uiFields (\ s a -> s{_uiFields = a})

instance GoogleRequest URLInsert where
        type Rs URLInsert = URL
        type Scopes URLInsert =
             '["https://www.googleapis.com/auth/urlshortener"]
        requestClient URLInsert'{..}
          = go _uiFields (Just AltJSON) _uiPayload
              uRLShortenerService
          where go
                  = buildClient (Proxy :: Proxy URLInsertResource)
                      mempty
