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
-- Module      : Network.Google.Resource.DFAReporting.Campaigns.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets one campaign by ID.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.campaigns.get@.
module Network.Google.Resource.DFAReporting.Campaigns.Get
    (
    -- * REST Resource
      CampaignsGetResource

    -- * Creating a Request
    , campaignsGet
    , CampaignsGet

    -- * Request Lenses
    , cggProFileId
    , cggId
    , cggFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.campaigns.get@ method which the
-- 'CampaignsGet' request conforms to.
type CampaignsGetResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "campaigns" :>
               Capture "id" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] Campaign

-- | Gets one campaign by ID.
--
-- /See:/ 'campaignsGet' smart constructor.
data CampaignsGet = CampaignsGet'
    { _cggProFileId :: !(Textual Int64)
    , _cggId :: !(Textual Int64)
    , _cggFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CampaignsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cggProFileId'
--
-- * 'cggId'
--
-- * 'cggFields'
campaignsGet
    :: Int64 -- ^ 'cggProFileId'
    -> Int64 -- ^ 'cggId'
    -> CampaignsGet
campaignsGet pCggProFileId_ pCggId_ = 
    CampaignsGet'
    { _cggProFileId = _Coerce # pCggProFileId_
    , _cggId = _Coerce # pCggId_
    , _cggFields = Nothing
    }

-- | User profile ID associated with this request.
cggProFileId :: Lens' CampaignsGet Int64
cggProFileId
  = lens _cggProFileId (\ s a -> s{_cggProFileId = a})
      . _Coerce

-- | Campaign ID.
cggId :: Lens' CampaignsGet Int64
cggId
  = lens _cggId (\ s a -> s{_cggId = a}) . _Coerce

-- | Selector specifying which fields to include in a partial response.
cggFields :: Lens' CampaignsGet (Maybe Text)
cggFields
  = lens _cggFields (\ s a -> s{_cggFields = a})

instance GoogleRequest CampaignsGet where
        type Rs CampaignsGet = Campaign
        type Scopes CampaignsGet =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient CampaignsGet'{..}
          = go _cggProFileId _cggId _cggFields (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient (Proxy :: Proxy CampaignsGetResource)
                      mempty
