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
-- Module      : Network.Google.Resource.DFAReporting.PlatformTypes.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of platform types.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.platformTypes.list@.
module Network.Google.Resource.DFAReporting.PlatformTypes.List
    (
    -- * REST Resource
      PlatformTypesListResource

    -- * Creating a Request
    , platformTypesList
    , PlatformTypesList

    -- * Request Lenses
    , ptlProFileId
    , ptlFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.platformTypes.list@ method which the
-- 'PlatformTypesList' request conforms to.
type PlatformTypesListResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "platformTypes" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   Get '[JSON] PlatformTypesListResponse

-- | Retrieves a list of platform types.
--
-- /See:/ 'platformTypesList' smart constructor.
data PlatformTypesList = PlatformTypesList'
    { _ptlProFileId :: !(Textual Int64)
    , _ptlFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PlatformTypesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptlProFileId'
--
-- * 'ptlFields'
platformTypesList
    :: Int64 -- ^ 'ptlProFileId'
    -> PlatformTypesList
platformTypesList pPtlProFileId_ = 
    PlatformTypesList'
    { _ptlProFileId = _Coerce # pPtlProFileId_
    , _ptlFields = Nothing
    }

-- | User profile ID associated with this request.
ptlProFileId :: Lens' PlatformTypesList Int64
ptlProFileId
  = lens _ptlProFileId (\ s a -> s{_ptlProFileId = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
ptlFields :: Lens' PlatformTypesList (Maybe Text)
ptlFields
  = lens _ptlFields (\ s a -> s{_ptlFields = a})

instance GoogleRequest PlatformTypesList where
        type Rs PlatformTypesList = PlatformTypesListResponse
        type Scopes PlatformTypesList =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient PlatformTypesList'{..}
          = go _ptlProFileId _ptlFields (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy PlatformTypesListResource)
                      mempty
