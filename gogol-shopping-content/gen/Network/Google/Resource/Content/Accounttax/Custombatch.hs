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
-- Module      : Network.Google.Resource.Content.Accounttax.Custombatch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves and updates tax settings of multiple accounts in a single
-- request.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.accounttax.custombatch@.
module Network.Google.Resource.Content.Accounttax.Custombatch
    (
    -- * REST Resource
      AccounttaxCustombatchResource

    -- * Creating a Request
    , accounttaxCustombatch
    , AccounttaxCustombatch

    -- * Request Lenses
    , acccPayload
    , acccDryRun
    , acccFields
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.accounttax.custombatch@ method which the
-- 'AccounttaxCustombatch' request conforms to.
type AccounttaxCustombatchResource =
     "content" :>
       "v2" :>
         "accounttax" :>
           "batch" :>
             QueryParam "dryRun" Bool :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] AccounttaxCustomBatchRequest :>
                     Post '[JSON] AccounttaxCustomBatchResponse

-- | Retrieves and updates tax settings of multiple accounts in a single
-- request.
--
-- /See:/ 'accounttaxCustombatch' smart constructor.
data AccounttaxCustombatch = AccounttaxCustombatch'
    { _acccPayload :: !AccounttaxCustomBatchRequest
    , _acccDryRun :: !(Maybe Bool)
    , _acccFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccounttaxCustombatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acccPayload'
--
-- * 'acccDryRun'
--
-- * 'acccFields'
accounttaxCustombatch
    :: AccounttaxCustomBatchRequest -- ^ 'acccPayload'
    -> AccounttaxCustombatch
accounttaxCustombatch pAcccPayload_ = 
    AccounttaxCustombatch'
    { _acccPayload = pAcccPayload_
    , _acccDryRun = Nothing
    , _acccFields = Nothing
    }

-- | Multipart request metadata.
acccPayload :: Lens' AccounttaxCustombatch AccounttaxCustomBatchRequest
acccPayload
  = lens _acccPayload (\ s a -> s{_acccPayload = a})

-- | Flag to run the request in dry-run mode.
acccDryRun :: Lens' AccounttaxCustombatch (Maybe Bool)
acccDryRun
  = lens _acccDryRun (\ s a -> s{_acccDryRun = a})

-- | Selector specifying which fields to include in a partial response.
acccFields :: Lens' AccounttaxCustombatch (Maybe Text)
acccFields
  = lens _acccFields (\ s a -> s{_acccFields = a})

instance GoogleRequest AccounttaxCustombatch where
        type Rs AccounttaxCustombatch =
             AccounttaxCustomBatchResponse
        type Scopes AccounttaxCustombatch =
             '["https://www.googleapis.com/auth/content"]
        requestClient AccounttaxCustombatch'{..}
          = go _acccDryRun _acccFields (Just AltJSON)
              _acccPayload
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy AccounttaxCustombatchResource)
                      mempty
