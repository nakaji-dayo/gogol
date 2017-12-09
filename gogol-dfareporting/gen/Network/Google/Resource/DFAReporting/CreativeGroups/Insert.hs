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
-- Module      : Network.Google.Resource.DFAReporting.CreativeGroups.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts a new creative group.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.creativeGroups.insert@.
module Network.Google.Resource.DFAReporting.CreativeGroups.Insert
    (
    -- * REST Resource
      CreativeGroupsInsertResource

    -- * Creating a Request
    , creativeGroupsInsert
    , CreativeGroupsInsert

    -- * Request Lenses
    , cgiProFileId
    , cgiPayload
    , cgiFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.creativeGroups.insert@ method which the
-- 'CreativeGroupsInsert' request conforms to.
type CreativeGroupsInsertResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "creativeGroups" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] CreativeGroup :>
                     Post '[JSON] CreativeGroup

-- | Inserts a new creative group.
--
-- /See:/ 'creativeGroupsInsert' smart constructor.
data CreativeGroupsInsert = CreativeGroupsInsert'
    { _cgiProFileId :: !(Textual Int64)
    , _cgiPayload :: !CreativeGroup
    , _cgiFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreativeGroupsInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgiProFileId'
--
-- * 'cgiPayload'
--
-- * 'cgiFields'
creativeGroupsInsert
    :: Int64 -- ^ 'cgiProFileId'
    -> CreativeGroup -- ^ 'cgiPayload'
    -> CreativeGroupsInsert
creativeGroupsInsert pCgiProFileId_ pCgiPayload_ = 
    CreativeGroupsInsert'
    { _cgiProFileId = _Coerce # pCgiProFileId_
    , _cgiPayload = pCgiPayload_
    , _cgiFields = Nothing
    }

-- | User profile ID associated with this request.
cgiProFileId :: Lens' CreativeGroupsInsert Int64
cgiProFileId
  = lens _cgiProFileId (\ s a -> s{_cgiProFileId = a})
      . _Coerce

-- | Multipart request metadata.
cgiPayload :: Lens' CreativeGroupsInsert CreativeGroup
cgiPayload
  = lens _cgiPayload (\ s a -> s{_cgiPayload = a})

-- | Selector specifying which fields to include in a partial response.
cgiFields :: Lens' CreativeGroupsInsert (Maybe Text)
cgiFields
  = lens _cgiFields (\ s a -> s{_cgiFields = a})

instance GoogleRequest CreativeGroupsInsert where
        type Rs CreativeGroupsInsert = CreativeGroup
        type Scopes CreativeGroupsInsert =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient CreativeGroupsInsert'{..}
          = go _cgiProFileId _cgiFields (Just AltJSON)
              _cgiPayload
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy CreativeGroupsInsertResource)
                      mempty
