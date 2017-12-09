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
-- Module      : Network.Google.Resource.URLShortener.URL.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of URLs shortened by a user.
--
-- /See:/ <https://developers.google.com/url-shortener/v1/getting_started URL Shortener API Reference> for @urlshortener.url.list@.
module Network.Google.Resource.URLShortener.URL.List
    (
    -- * REST Resource
      URLListResource

    -- * Creating a Request
    , urlList
    , URLList

    -- * Request Lenses
    , ulStartToken
    , ulProjection
    , ulFields
    ) where

import Network.Google.Prelude
import Network.Google.URLShortener.Types

-- | A resource alias for @urlshortener.url.list@ method which the
-- 'URLList' request conforms to.
type URLListResource =
     "urlshortener" :>
       "v1" :>
         "url" :>
           "history" :>
             QueryParam "start-token" Text :>
               QueryParam "projection" URLListProjection :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] URLHistory

-- | Retrieves a list of URLs shortened by a user.
--
-- /See:/ 'urlList' smart constructor.
data URLList = URLList'
    { _ulStartToken :: !(Maybe Text)
    , _ulProjection :: !(Maybe URLListProjection)
    , _ulFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'URLList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulStartToken'
--
-- * 'ulProjection'
--
-- * 'ulFields'
urlList
    :: URLList
urlList = 
    URLList'
    { _ulStartToken = Nothing
    , _ulProjection = Nothing
    , _ulFields = Nothing
    }

-- | Token for requesting successive pages of results.
ulStartToken :: Lens' URLList (Maybe Text)
ulStartToken
  = lens _ulStartToken (\ s a -> s{_ulStartToken = a})

-- | Additional information to return.
ulProjection :: Lens' URLList (Maybe URLListProjection)
ulProjection
  = lens _ulProjection (\ s a -> s{_ulProjection = a})

-- | Selector specifying which fields to include in a partial response.
ulFields :: Lens' URLList (Maybe Text)
ulFields = lens _ulFields (\ s a -> s{_ulFields = a})

instance GoogleRequest URLList where
        type Rs URLList = URLHistory
        type Scopes URLList =
             '["https://www.googleapis.com/auth/urlshortener"]
        requestClient URLList'{..}
          = go _ulStartToken _ulProjection _ulFields
              (Just AltJSON)
              uRLShortenerService
          where go
                  = buildClient (Proxy :: Proxy URLListResource) mempty
