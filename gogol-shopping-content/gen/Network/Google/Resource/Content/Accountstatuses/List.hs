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
-- Module      : Network.Google.Resource.Content.Accountstatuses.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the statuses of the sub-accounts in your Merchant Center account.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.accountstatuses.list@.
module Network.Google.Resource.Content.Accountstatuses.List
    (
    -- * REST Resource
      AccountstatusesListResource

    -- * Creating a Request
    , accountstatusesList
    , AccountstatusesList

    -- * Request Lenses
    , al1MerchantId
    , al1PageToken
    , al1MaxResults
    , al1Fields
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.accountstatuses.list@ method which the
-- 'AccountstatusesList' request conforms to.
type AccountstatusesListResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "accountstatuses" :>
             QueryParam "pageToken" Text :>
               QueryParam "maxResults" (Textual Word32) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] AccountstatusesListResponse

-- | Lists the statuses of the sub-accounts in your Merchant Center account.
--
-- /See:/ 'accountstatusesList' smart constructor.
data AccountstatusesList = AccountstatusesList'
    { _al1MerchantId :: !(Textual Word64)
    , _al1PageToken :: !(Maybe Text)
    , _al1MaxResults :: !(Maybe (Textual Word32))
    , _al1Fields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountstatusesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'al1MerchantId'
--
-- * 'al1PageToken'
--
-- * 'al1MaxResults'
--
-- * 'al1Fields'
accountstatusesList
    :: Word64 -- ^ 'al1MerchantId'
    -> AccountstatusesList
accountstatusesList pAl1MerchantId_ = 
    AccountstatusesList'
    { _al1MerchantId = _Coerce # pAl1MerchantId_
    , _al1PageToken = Nothing
    , _al1MaxResults = Nothing
    , _al1Fields = Nothing
    }

-- | The ID of the managing account. This must be a multi-client account.
al1MerchantId :: Lens' AccountstatusesList Word64
al1MerchantId
  = lens _al1MerchantId
      (\ s a -> s{_al1MerchantId = a})
      . _Coerce

-- | The token returned by the previous request.
al1PageToken :: Lens' AccountstatusesList (Maybe Text)
al1PageToken
  = lens _al1PageToken (\ s a -> s{_al1PageToken = a})

-- | The maximum number of account statuses to return in the response, used
-- for paging.
al1MaxResults :: Lens' AccountstatusesList (Maybe Word32)
al1MaxResults
  = lens _al1MaxResults
      (\ s a -> s{_al1MaxResults = a})
      . mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
al1Fields :: Lens' AccountstatusesList (Maybe Text)
al1Fields
  = lens _al1Fields (\ s a -> s{_al1Fields = a})

instance GoogleRequest AccountstatusesList where
        type Rs AccountstatusesList =
             AccountstatusesListResponse
        type Scopes AccountstatusesList =
             '["https://www.googleapis.com/auth/content"]
        requestClient AccountstatusesList'{..}
          = go _al1MerchantId _al1PageToken _al1MaxResults
              _al1Fields
              (Just AltJSON)
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountstatusesListResource)
                      mempty
