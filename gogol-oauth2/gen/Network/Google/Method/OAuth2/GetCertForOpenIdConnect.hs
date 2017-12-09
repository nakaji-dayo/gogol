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
-- Module      : Network.Google.Method.OAuth2.GetCertForOpenIdConnect
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /See:/ <https://developers.google.com/accounts/docs/OAuth2 Google OAuth2 API Reference> for @oauth2.getCertForOpenIdConnect@.
module Network.Google.Method.OAuth2.GetCertForOpenIdConnect
    (
    -- * REST Resource
      GetCertForOpenIdConnectMethod

    -- * Creating a Request
    , getCertForOpenIdConnect
    , GetCertForOpenIdConnect

    -- * Request Lenses
    , gcfoicFields
    ) where

import Network.Google.OAuth2.Types
import Network.Google.Prelude

-- | A resource alias for @oauth2.getCertForOpenIdConnect@ method which the
-- 'GetCertForOpenIdConnect' request conforms to.
type GetCertForOpenIdConnectMethod =
     "oauth2" :>
       "v2" :>
         "certs" :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :> Get '[JSON] JWK

--
-- /See:/ 'getCertForOpenIdConnect' smart constructor.
newtype GetCertForOpenIdConnect = GetCertForOpenIdConnect'
    { _gcfoicFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetCertForOpenIdConnect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfoicFields'
getCertForOpenIdConnect
    :: GetCertForOpenIdConnect
getCertForOpenIdConnect = 
    GetCertForOpenIdConnect'
    { _gcfoicFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
gcfoicFields :: Lens' GetCertForOpenIdConnect (Maybe Text)
gcfoicFields
  = lens _gcfoicFields (\ s a -> s{_gcfoicFields = a})

instance GoogleRequest GetCertForOpenIdConnect where
        type Rs GetCertForOpenIdConnect = JWK
        type Scopes GetCertForOpenIdConnect = '[]
        requestClient GetCertForOpenIdConnect'{..}
          = go _gcfoicFields (Just AltJSON) oAuth2Service
          where go
                  = buildClient
                      (Proxy :: Proxy GetCertForOpenIdConnectMethod)
                      mempty
