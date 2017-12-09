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
-- Module      : Network.Google.Resource.Content.Accountstatuses.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of a Merchant Center account.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.accountstatuses.get@.
module Network.Google.Resource.Content.Accountstatuses.Get
    (
    -- * REST Resource
      AccountstatusesGetResource

    -- * Creating a Request
    , accountstatusesGet
    , AccountstatusesGet

    -- * Request Lenses
    , aaMerchantId
    , aaAccountId
    , aaFields
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.accountstatuses.get@ method which the
-- 'AccountstatusesGet' request conforms to.
type AccountstatusesGetResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "accountstatuses" :>
             Capture "accountId" (Textual Word64) :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] AccountStatus

-- | Retrieves the status of a Merchant Center account.
--
-- /See:/ 'accountstatusesGet' smart constructor.
data AccountstatusesGet = AccountstatusesGet'
    { _aaMerchantId :: !(Textual Word64)
    , _aaAccountId :: !(Textual Word64)
    , _aaFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountstatusesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaMerchantId'
--
-- * 'aaAccountId'
--
-- * 'aaFields'
accountstatusesGet
    :: Word64 -- ^ 'aaMerchantId'
    -> Word64 -- ^ 'aaAccountId'
    -> AccountstatusesGet
accountstatusesGet pAaMerchantId_ pAaAccountId_ = 
    AccountstatusesGet'
    { _aaMerchantId = _Coerce # pAaMerchantId_
    , _aaAccountId = _Coerce # pAaAccountId_
    , _aaFields = Nothing
    }

-- | The ID of the managing account. If this parameter is not the same as
-- accountId, then this account must be a multi-client account and
-- accountId must be the ID of a sub-account of this account.
aaMerchantId :: Lens' AccountstatusesGet Word64
aaMerchantId
  = lens _aaMerchantId (\ s a -> s{_aaMerchantId = a})
      . _Coerce

-- | The ID of the account.
aaAccountId :: Lens' AccountstatusesGet Word64
aaAccountId
  = lens _aaAccountId (\ s a -> s{_aaAccountId = a}) .
      _Coerce

-- | Selector specifying which fields to include in a partial response.
aaFields :: Lens' AccountstatusesGet (Maybe Text)
aaFields = lens _aaFields (\ s a -> s{_aaFields = a})

instance GoogleRequest AccountstatusesGet where
        type Rs AccountstatusesGet = AccountStatus
        type Scopes AccountstatusesGet =
             '["https://www.googleapis.com/auth/content"]
        requestClient AccountstatusesGet'{..}
          = go _aaMerchantId _aaAccountId _aaFields
              (Just AltJSON)
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountstatusesGetResource)
                      mempty
