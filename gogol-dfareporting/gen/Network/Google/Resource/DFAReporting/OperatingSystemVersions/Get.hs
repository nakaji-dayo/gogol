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
-- Module      : Network.Google.Resource.DFAReporting.OperatingSystemVersions.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets one operating system version by ID.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.operatingSystemVersions.get@.
module Network.Google.Resource.DFAReporting.OperatingSystemVersions.Get
    (
    -- * REST Resource
      OperatingSystemVersionsGetResource

    -- * Creating a Request
    , operatingSystemVersionsGet
    , OperatingSystemVersionsGet

    -- * Request Lenses
    , osvgProFileId
    , osvgId
    , osvgFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.operatingSystemVersions.get@ method which the
-- 'OperatingSystemVersionsGet' request conforms to.
type OperatingSystemVersionsGetResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "operatingSystemVersions" :>
               Capture "id" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] OperatingSystemVersion

-- | Gets one operating system version by ID.
--
-- /See:/ 'operatingSystemVersionsGet' smart constructor.
data OperatingSystemVersionsGet = OperatingSystemVersionsGet'
    { _osvgProFileId :: !(Textual Int64)
    , _osvgId :: !(Textual Int64)
    , _osvgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperatingSystemVersionsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osvgProFileId'
--
-- * 'osvgId'
--
-- * 'osvgFields'
operatingSystemVersionsGet
    :: Int64 -- ^ 'osvgProFileId'
    -> Int64 -- ^ 'osvgId'
    -> OperatingSystemVersionsGet
operatingSystemVersionsGet pOsvgProFileId_ pOsvgId_ = 
    OperatingSystemVersionsGet'
    { _osvgProFileId = _Coerce # pOsvgProFileId_
    , _osvgId = _Coerce # pOsvgId_
    , _osvgFields = Nothing
    }

-- | User profile ID associated with this request.
osvgProFileId :: Lens' OperatingSystemVersionsGet Int64
osvgProFileId
  = lens _osvgProFileId
      (\ s a -> s{_osvgProFileId = a})
      . _Coerce

-- | Operating system version ID.
osvgId :: Lens' OperatingSystemVersionsGet Int64
osvgId
  = lens _osvgId (\ s a -> s{_osvgId = a}) . _Coerce

-- | Selector specifying which fields to include in a partial response.
osvgFields :: Lens' OperatingSystemVersionsGet (Maybe Text)
osvgFields
  = lens _osvgFields (\ s a -> s{_osvgFields = a})

instance GoogleRequest OperatingSystemVersionsGet
         where
        type Rs OperatingSystemVersionsGet =
             OperatingSystemVersion
        type Scopes OperatingSystemVersionsGet =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient OperatingSystemVersionsGet'{..}
          = go _osvgProFileId _osvgId _osvgFields
              (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy OperatingSystemVersionsGetResource)
                      mempty
