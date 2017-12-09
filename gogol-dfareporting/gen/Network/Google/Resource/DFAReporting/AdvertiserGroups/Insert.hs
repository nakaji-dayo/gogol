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
-- Module      : Network.Google.Resource.DFAReporting.AdvertiserGroups.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts a new advertiser group.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.advertiserGroups.insert@.
module Network.Google.Resource.DFAReporting.AdvertiserGroups.Insert
    (
    -- * REST Resource
      AdvertiserGroupsInsertResource

    -- * Creating a Request
    , advertiserGroupsInsert
    , AdvertiserGroupsInsert

    -- * Request Lenses
    , agiProFileId
    , agiPayload
    , agiFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.advertiserGroups.insert@ method which the
-- 'AdvertiserGroupsInsert' request conforms to.
type AdvertiserGroupsInsertResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "advertiserGroups" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] AdvertiserGroup :>
                     Post '[JSON] AdvertiserGroup

-- | Inserts a new advertiser group.
--
-- /See:/ 'advertiserGroupsInsert' smart constructor.
data AdvertiserGroupsInsert = AdvertiserGroupsInsert'
    { _agiProFileId :: !(Textual Int64)
    , _agiPayload :: !AdvertiserGroup
    , _agiFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AdvertiserGroupsInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agiProFileId'
--
-- * 'agiPayload'
--
-- * 'agiFields'
advertiserGroupsInsert
    :: Int64 -- ^ 'agiProFileId'
    -> AdvertiserGroup -- ^ 'agiPayload'
    -> AdvertiserGroupsInsert
advertiserGroupsInsert pAgiProFileId_ pAgiPayload_ = 
    AdvertiserGroupsInsert'
    { _agiProFileId = _Coerce # pAgiProFileId_
    , _agiPayload = pAgiPayload_
    , _agiFields = Nothing
    }

-- | User profile ID associated with this request.
agiProFileId :: Lens' AdvertiserGroupsInsert Int64
agiProFileId
  = lens _agiProFileId (\ s a -> s{_agiProFileId = a})
      . _Coerce

-- | Multipart request metadata.
agiPayload :: Lens' AdvertiserGroupsInsert AdvertiserGroup
agiPayload
  = lens _agiPayload (\ s a -> s{_agiPayload = a})

-- | Selector specifying which fields to include in a partial response.
agiFields :: Lens' AdvertiserGroupsInsert (Maybe Text)
agiFields
  = lens _agiFields (\ s a -> s{_agiFields = a})

instance GoogleRequest AdvertiserGroupsInsert where
        type Rs AdvertiserGroupsInsert = AdvertiserGroup
        type Scopes AdvertiserGroupsInsert =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient AdvertiserGroupsInsert'{..}
          = go _agiProFileId _agiFields (Just AltJSON)
              _agiPayload
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy AdvertiserGroupsInsertResource)
                      mempty
