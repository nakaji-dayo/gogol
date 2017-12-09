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
-- Module      : Network.Google.Resource.DFAReporting.Ads.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing ad.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.ads.update@.
module Network.Google.Resource.DFAReporting.Ads.Update
    (
    -- * REST Resource
      AdsUpdateResource

    -- * Creating a Request
    , adsUpdate
    , AdsUpdate

    -- * Request Lenses
    , aProFileId
    , aPayload
    , aFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.ads.update@ method which the
-- 'AdsUpdate' request conforms to.
type AdsUpdateResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "ads" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] Ad :> Put '[JSON] Ad

-- | Updates an existing ad.
--
-- /See:/ 'adsUpdate' smart constructor.
data AdsUpdate = AdsUpdate'
    { _aProFileId :: !(Textual Int64)
    , _aPayload :: !Ad
    , _aFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AdsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aProFileId'
--
-- * 'aPayload'
--
-- * 'aFields'
adsUpdate
    :: Int64 -- ^ 'aProFileId'
    -> Ad -- ^ 'aPayload'
    -> AdsUpdate
adsUpdate pAProFileId_ pAPayload_ = 
    AdsUpdate'
    { _aProFileId = _Coerce # pAProFileId_
    , _aPayload = pAPayload_
    , _aFields = Nothing
    }

-- | User profile ID associated with this request.
aProFileId :: Lens' AdsUpdate Int64
aProFileId
  = lens _aProFileId (\ s a -> s{_aProFileId = a}) .
      _Coerce

-- | Multipart request metadata.
aPayload :: Lens' AdsUpdate Ad
aPayload = lens _aPayload (\ s a -> s{_aPayload = a})

-- | Selector specifying which fields to include in a partial response.
aFields :: Lens' AdsUpdate (Maybe Text)
aFields = lens _aFields (\ s a -> s{_aFields = a})

instance GoogleRequest AdsUpdate where
        type Rs AdsUpdate = Ad
        type Scopes AdsUpdate =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient AdsUpdate'{..}
          = go _aProFileId _aFields (Just AltJSON) _aPayload
              dFAReportingService
          where go
                  = buildClient (Proxy :: Proxy AdsUpdateResource)
                      mempty
