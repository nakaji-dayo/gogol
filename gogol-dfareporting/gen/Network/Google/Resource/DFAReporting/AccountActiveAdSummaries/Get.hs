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
-- Module      : Network.Google.Resource.DFAReporting.AccountActiveAdSummaries.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the account\'s active ad summary by account ID.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.accountActiveAdSummaries.get@.
module Network.Google.Resource.DFAReporting.AccountActiveAdSummaries.Get
    (
    -- * REST Resource
      AccountActiveAdSummariesGetResource

    -- * Creating a Request
    , accountActiveAdSummariesGet
    , AccountActiveAdSummariesGet

    -- * Request Lenses
    , aaasgProFileId
    , aaasgSummaryAccountId
    , aaasgFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.accountActiveAdSummaries.get@ method which the
-- 'AccountActiveAdSummariesGet' request conforms to.
type AccountActiveAdSummariesGetResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "accountActiveAdSummaries" :>
               Capture "summaryAccountId" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] AccountActiveAdSummary

-- | Gets the account\'s active ad summary by account ID.
--
-- /See:/ 'accountActiveAdSummariesGet' smart constructor.
data AccountActiveAdSummariesGet = AccountActiveAdSummariesGet'
    { _aaasgProFileId :: !(Textual Int64)
    , _aaasgSummaryAccountId :: !(Textual Int64)
    , _aaasgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountActiveAdSummariesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaasgProFileId'
--
-- * 'aaasgSummaryAccountId'
--
-- * 'aaasgFields'
accountActiveAdSummariesGet
    :: Int64 -- ^ 'aaasgProFileId'
    -> Int64 -- ^ 'aaasgSummaryAccountId'
    -> AccountActiveAdSummariesGet
accountActiveAdSummariesGet pAaasgProFileId_ pAaasgSummaryAccountId_ = 
    AccountActiveAdSummariesGet'
    { _aaasgProFileId = _Coerce # pAaasgProFileId_
    , _aaasgSummaryAccountId = _Coerce # pAaasgSummaryAccountId_
    , _aaasgFields = Nothing
    }

-- | User profile ID associated with this request.
aaasgProFileId :: Lens' AccountActiveAdSummariesGet Int64
aaasgProFileId
  = lens _aaasgProFileId
      (\ s a -> s{_aaasgProFileId = a})
      . _Coerce

-- | Account ID.
aaasgSummaryAccountId :: Lens' AccountActiveAdSummariesGet Int64
aaasgSummaryAccountId
  = lens _aaasgSummaryAccountId
      (\ s a -> s{_aaasgSummaryAccountId = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
aaasgFields :: Lens' AccountActiveAdSummariesGet (Maybe Text)
aaasgFields
  = lens _aaasgFields (\ s a -> s{_aaasgFields = a})

instance GoogleRequest AccountActiveAdSummariesGet
         where
        type Rs AccountActiveAdSummariesGet =
             AccountActiveAdSummary
        type Scopes AccountActiveAdSummariesGet =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient AccountActiveAdSummariesGet'{..}
          = go _aaasgProFileId _aaasgSummaryAccountId
              _aaasgFields
              (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountActiveAdSummariesGetResource)
                      mempty
