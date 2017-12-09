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
-- Module      : Network.Google.Resource.DFAReporting.MobileCarriers.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets one mobile carrier by ID.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.mobileCarriers.get@.
module Network.Google.Resource.DFAReporting.MobileCarriers.Get
    (
    -- * REST Resource
      MobileCarriersGetResource

    -- * Creating a Request
    , mobileCarriersGet
    , MobileCarriersGet

    -- * Request Lenses
    , mcgProFileId
    , mcgId
    , mcgFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.mobileCarriers.get@ method which the
-- 'MobileCarriersGet' request conforms to.
type MobileCarriersGetResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "mobileCarriers" :>
               Capture "id" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] MobileCarrier

-- | Gets one mobile carrier by ID.
--
-- /See:/ 'mobileCarriersGet' smart constructor.
data MobileCarriersGet = MobileCarriersGet'
    { _mcgProFileId :: !(Textual Int64)
    , _mcgId :: !(Textual Int64)
    , _mcgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MobileCarriersGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcgProFileId'
--
-- * 'mcgId'
--
-- * 'mcgFields'
mobileCarriersGet
    :: Int64 -- ^ 'mcgProFileId'
    -> Int64 -- ^ 'mcgId'
    -> MobileCarriersGet
mobileCarriersGet pMcgProFileId_ pMcgId_ = 
    MobileCarriersGet'
    { _mcgProFileId = _Coerce # pMcgProFileId_
    , _mcgId = _Coerce # pMcgId_
    , _mcgFields = Nothing
    }

-- | User profile ID associated with this request.
mcgProFileId :: Lens' MobileCarriersGet Int64
mcgProFileId
  = lens _mcgProFileId (\ s a -> s{_mcgProFileId = a})
      . _Coerce

-- | Mobile carrier ID.
mcgId :: Lens' MobileCarriersGet Int64
mcgId
  = lens _mcgId (\ s a -> s{_mcgId = a}) . _Coerce

-- | Selector specifying which fields to include in a partial response.
mcgFields :: Lens' MobileCarriersGet (Maybe Text)
mcgFields
  = lens _mcgFields (\ s a -> s{_mcgFields = a})

instance GoogleRequest MobileCarriersGet where
        type Rs MobileCarriersGet = MobileCarrier
        type Scopes MobileCarriersGet =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient MobileCarriersGet'{..}
          = go _mcgProFileId _mcgId _mcgFields (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy MobileCarriersGetResource)
                      mempty
