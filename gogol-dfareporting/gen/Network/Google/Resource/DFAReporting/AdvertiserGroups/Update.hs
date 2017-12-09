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
-- Module      : Network.Google.Resource.DFAReporting.AdvertiserGroups.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing advertiser group.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.advertiserGroups.update@.
module Network.Google.Resource.DFAReporting.AdvertiserGroups.Update
    (
    -- * REST Resource
      AdvertiserGroupsUpdateResource

    -- * Creating a Request
    , advertiserGroupsUpdate
    , AdvertiserGroupsUpdate

    -- * Request Lenses
    , aguProFileId
    , aguPayload
    , aguFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.advertiserGroups.update@ method which the
-- 'AdvertiserGroupsUpdate' request conforms to.
type AdvertiserGroupsUpdateResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "advertiserGroups" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] AdvertiserGroup :>
                     Put '[JSON] AdvertiserGroup

-- | Updates an existing advertiser group.
--
-- /See:/ 'advertiserGroupsUpdate' smart constructor.
data AdvertiserGroupsUpdate = AdvertiserGroupsUpdate'
    { _aguProFileId :: !(Textual Int64)
    , _aguPayload :: !AdvertiserGroup
    , _aguFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AdvertiserGroupsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aguProFileId'
--
-- * 'aguPayload'
--
-- * 'aguFields'
advertiserGroupsUpdate
    :: Int64 -- ^ 'aguProFileId'
    -> AdvertiserGroup -- ^ 'aguPayload'
    -> AdvertiserGroupsUpdate
advertiserGroupsUpdate pAguProFileId_ pAguPayload_ = 
    AdvertiserGroupsUpdate'
    { _aguProFileId = _Coerce # pAguProFileId_
    , _aguPayload = pAguPayload_
    , _aguFields = Nothing
    }

-- | User profile ID associated with this request.
aguProFileId :: Lens' AdvertiserGroupsUpdate Int64
aguProFileId
  = lens _aguProFileId (\ s a -> s{_aguProFileId = a})
      . _Coerce

-- | Multipart request metadata.
aguPayload :: Lens' AdvertiserGroupsUpdate AdvertiserGroup
aguPayload
  = lens _aguPayload (\ s a -> s{_aguPayload = a})

-- | Selector specifying which fields to include in a partial response.
aguFields :: Lens' AdvertiserGroupsUpdate (Maybe Text)
aguFields
  = lens _aguFields (\ s a -> s{_aguFields = a})

instance GoogleRequest AdvertiserGroupsUpdate where
        type Rs AdvertiserGroupsUpdate = AdvertiserGroup
        type Scopes AdvertiserGroupsUpdate =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient AdvertiserGroupsUpdate'{..}
          = go _aguProFileId _aguFields (Just AltJSON)
              _aguPayload
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy AdvertiserGroupsUpdateResource)
                      mempty
