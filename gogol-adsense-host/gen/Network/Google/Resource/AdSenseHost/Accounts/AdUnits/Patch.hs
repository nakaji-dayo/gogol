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
-- Module      : Network.Google.Resource.AdSenseHost.Accounts.AdUnits.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the supplied ad unit in the specified publisher AdSense account.
-- This method supports patch semantics.
--
-- /See:/ <https://developers.google.com/adsense/host/ AdSense Host API Reference> for @adsensehost.accounts.adunits.patch@.
module Network.Google.Resource.AdSenseHost.Accounts.AdUnits.Patch
    (
    -- * REST Resource
      AccountsAdUnitsPatchResource

    -- * Creating a Request
    , accountsAdUnitsPatch
    , AccountsAdUnitsPatch

    -- * Request Lenses
    , aaupAdUnitId
    , aaupPayload
    , aaupAdClientId
    , aaupAccountId
    , aaupFields
    ) where

import Network.Google.AdSenseHost.Types
import Network.Google.Prelude

-- | A resource alias for @adsensehost.accounts.adunits.patch@ method which the
-- 'AccountsAdUnitsPatch' request conforms to.
type AccountsAdUnitsPatchResource =
     "adsensehost" :>
       "v4.1" :>
         "accounts" :>
           Capture "accountId" Text :>
             "adclients" :>
               Capture "adClientId" Text :>
                 "adunits" :>
                   QueryParam "adUnitId" Text :>
                     QueryParam "fields" Text :>
                       QueryParam "alt" AltJSON :>
                         ReqBody '[JSON] AdUnit :> Patch '[JSON] AdUnit

-- | Update the supplied ad unit in the specified publisher AdSense account.
-- This method supports patch semantics.
--
-- /See:/ 'accountsAdUnitsPatch' smart constructor.
data AccountsAdUnitsPatch = AccountsAdUnitsPatch'
    { _aaupAdUnitId :: !Text
    , _aaupPayload :: !AdUnit
    , _aaupAdClientId :: !Text
    , _aaupAccountId :: !Text
    , _aaupFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountsAdUnitsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaupAdUnitId'
--
-- * 'aaupPayload'
--
-- * 'aaupAdClientId'
--
-- * 'aaupAccountId'
--
-- * 'aaupFields'
accountsAdUnitsPatch
    :: Text -- ^ 'aaupAdUnitId'
    -> AdUnit -- ^ 'aaupPayload'
    -> Text -- ^ 'aaupAdClientId'
    -> Text -- ^ 'aaupAccountId'
    -> AccountsAdUnitsPatch
accountsAdUnitsPatch pAaupAdUnitId_ pAaupPayload_ pAaupAdClientId_ pAaupAccountId_ = 
    AccountsAdUnitsPatch'
    { _aaupAdUnitId = pAaupAdUnitId_
    , _aaupPayload = pAaupPayload_
    , _aaupAdClientId = pAaupAdClientId_
    , _aaupAccountId = pAaupAccountId_
    , _aaupFields = Nothing
    }

-- | Ad unit to get.
aaupAdUnitId :: Lens' AccountsAdUnitsPatch Text
aaupAdUnitId
  = lens _aaupAdUnitId (\ s a -> s{_aaupAdUnitId = a})

-- | Multipart request metadata.
aaupPayload :: Lens' AccountsAdUnitsPatch AdUnit
aaupPayload
  = lens _aaupPayload (\ s a -> s{_aaupPayload = a})

-- | Ad client which contains the ad unit.
aaupAdClientId :: Lens' AccountsAdUnitsPatch Text
aaupAdClientId
  = lens _aaupAdClientId
      (\ s a -> s{_aaupAdClientId = a})

-- | Account which contains the ad client.
aaupAccountId :: Lens' AccountsAdUnitsPatch Text
aaupAccountId
  = lens _aaupAccountId
      (\ s a -> s{_aaupAccountId = a})

-- | Selector specifying which fields to include in a partial response.
aaupFields :: Lens' AccountsAdUnitsPatch (Maybe Text)
aaupFields
  = lens _aaupFields (\ s a -> s{_aaupFields = a})

instance GoogleRequest AccountsAdUnitsPatch where
        type Rs AccountsAdUnitsPatch = AdUnit
        type Scopes AccountsAdUnitsPatch =
             '["https://www.googleapis.com/auth/adsensehost"]
        requestClient AccountsAdUnitsPatch'{..}
          = go _aaupAccountId _aaupAdClientId
              (Just _aaupAdUnitId)
              _aaupFields
              (Just AltJSON)
              _aaupPayload
              adSenseHostService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountsAdUnitsPatchResource)
                      mempty
