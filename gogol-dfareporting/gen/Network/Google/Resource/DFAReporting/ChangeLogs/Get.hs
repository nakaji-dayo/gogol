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
-- Module      : Network.Google.Resource.DFAReporting.ChangeLogs.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets one change log by ID.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.changeLogs.get@.
module Network.Google.Resource.DFAReporting.ChangeLogs.Get
    (
    -- * REST Resource
      ChangeLogsGetResource

    -- * Creating a Request
    , changeLogsGet
    , ChangeLogsGet

    -- * Request Lenses
    , clgProFileId
    , clgId
    , clgFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.changeLogs.get@ method which the
-- 'ChangeLogsGet' request conforms to.
type ChangeLogsGetResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "changeLogs" :>
               Capture "id" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] ChangeLog

-- | Gets one change log by ID.
--
-- /See:/ 'changeLogsGet' smart constructor.
data ChangeLogsGet = ChangeLogsGet'
    { _clgProFileId :: !(Textual Int64)
    , _clgId :: !(Textual Int64)
    , _clgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChangeLogsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clgProFileId'
--
-- * 'clgId'
--
-- * 'clgFields'
changeLogsGet
    :: Int64 -- ^ 'clgProFileId'
    -> Int64 -- ^ 'clgId'
    -> ChangeLogsGet
changeLogsGet pClgProFileId_ pClgId_ = 
    ChangeLogsGet'
    { _clgProFileId = _Coerce # pClgProFileId_
    , _clgId = _Coerce # pClgId_
    , _clgFields = Nothing
    }

-- | User profile ID associated with this request.
clgProFileId :: Lens' ChangeLogsGet Int64
clgProFileId
  = lens _clgProFileId (\ s a -> s{_clgProFileId = a})
      . _Coerce

-- | Change log ID.
clgId :: Lens' ChangeLogsGet Int64
clgId
  = lens _clgId (\ s a -> s{_clgId = a}) . _Coerce

-- | Selector specifying which fields to include in a partial response.
clgFields :: Lens' ChangeLogsGet (Maybe Text)
clgFields
  = lens _clgFields (\ s a -> s{_clgFields = a})

instance GoogleRequest ChangeLogsGet where
        type Rs ChangeLogsGet = ChangeLog
        type Scopes ChangeLogsGet =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient ChangeLogsGet'{..}
          = go _clgProFileId _clgId _clgFields (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient (Proxy :: Proxy ChangeLogsGetResource)
                      mempty
