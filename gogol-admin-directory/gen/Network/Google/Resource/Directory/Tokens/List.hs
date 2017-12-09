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
-- Module      : Network.Google.Resource.Directory.Tokens.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the set of tokens specified user has issued to 3rd party
-- applications.
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.tokens.list@.
module Network.Google.Resource.Directory.Tokens.List
    (
    -- * REST Resource
      TokensListResource

    -- * Creating a Request
    , tokensList
    , TokensList

    -- * Request Lenses
    , tlUserKey
    , tlFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.tokens.list@ method which the
-- 'TokensList' request conforms to.
type TokensListResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "users" :>
             Capture "userKey" Text :>
               "tokens" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] Tokens

-- | Returns the set of tokens specified user has issued to 3rd party
-- applications.
--
-- /See:/ 'tokensList' smart constructor.
data TokensList = TokensList'
    { _tlUserKey :: !Text
    , _tlFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TokensList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlUserKey'
--
-- * 'tlFields'
tokensList
    :: Text -- ^ 'tlUserKey'
    -> TokensList
tokensList pTlUserKey_ = 
    TokensList'
    { _tlUserKey = pTlUserKey_
    , _tlFields = Nothing
    }

-- | Identifies the user in the API request. The value can be the user\'s
-- primary email address, alias email address, or unique user ID.
tlUserKey :: Lens' TokensList Text
tlUserKey
  = lens _tlUserKey (\ s a -> s{_tlUserKey = a})

-- | Selector specifying which fields to include in a partial response.
tlFields :: Lens' TokensList (Maybe Text)
tlFields = lens _tlFields (\ s a -> s{_tlFields = a})

instance GoogleRequest TokensList where
        type Rs TokensList = Tokens
        type Scopes TokensList =
             '["https://www.googleapis.com/auth/admin.directory.user.security"]
        requestClient TokensList'{..}
          = go _tlUserKey _tlFields (Just AltJSON)
              directoryService
          where go
                  = buildClient (Proxy :: Proxy TokensListResource)
                      mempty
