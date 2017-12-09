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
-- Module      : Network.Google.Resource.DFAReporting.Ads.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing ad. This method supports patch semantics.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.ads.patch@.
module Network.Google.Resource.DFAReporting.Ads.Patch
    (
    -- * REST Resource
      AdsPatchResource

    -- * Creating a Request
    , adsPatch
    , AdsPatch

    -- * Request Lenses
    , adsdProFileId
    , adsdPayload
    , adsdId
    , adsdFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.ads.patch@ method which the
-- 'AdsPatch' request conforms to.
type AdsPatchResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "ads" :>
               QueryParam "id" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] Ad :> Patch '[JSON] Ad

-- | Updates an existing ad. This method supports patch semantics.
--
-- /See:/ 'adsPatch' smart constructor.
data AdsPatch = AdsPatch'
    { _adsdProFileId :: !(Textual Int64)
    , _adsdPayload :: !Ad
    , _adsdId :: !(Textual Int64)
    , _adsdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AdsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adsdProFileId'
--
-- * 'adsdPayload'
--
-- * 'adsdId'
--
-- * 'adsdFields'
adsPatch
    :: Int64 -- ^ 'adsdProFileId'
    -> Ad -- ^ 'adsdPayload'
    -> Int64 -- ^ 'adsdId'
    -> AdsPatch
adsPatch pAdsdProFileId_ pAdsdPayload_ pAdsdId_ = 
    AdsPatch'
    { _adsdProFileId = _Coerce # pAdsdProFileId_
    , _adsdPayload = pAdsdPayload_
    , _adsdId = _Coerce # pAdsdId_
    , _adsdFields = Nothing
    }

-- | User profile ID associated with this request.
adsdProFileId :: Lens' AdsPatch Int64
adsdProFileId
  = lens _adsdProFileId
      (\ s a -> s{_adsdProFileId = a})
      . _Coerce

-- | Multipart request metadata.
adsdPayload :: Lens' AdsPatch Ad
adsdPayload
  = lens _adsdPayload (\ s a -> s{_adsdPayload = a})

-- | Ad ID.
adsdId :: Lens' AdsPatch Int64
adsdId
  = lens _adsdId (\ s a -> s{_adsdId = a}) . _Coerce

-- | Selector specifying which fields to include in a partial response.
adsdFields :: Lens' AdsPatch (Maybe Text)
adsdFields
  = lens _adsdFields (\ s a -> s{_adsdFields = a})

instance GoogleRequest AdsPatch where
        type Rs AdsPatch = Ad
        type Scopes AdsPatch =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient AdsPatch'{..}
          = go _adsdProFileId (Just _adsdId) _adsdFields
              (Just AltJSON)
              _adsdPayload
              dFAReportingService
          where go
                  = buildClient (Proxy :: Proxy AdsPatchResource)
                      mempty
