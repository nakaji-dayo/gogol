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
-- Module      : Network.Google.Resource.DFAReporting.PlatformTypes.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets one platform type by ID.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.platformTypes.get@.
module Network.Google.Resource.DFAReporting.PlatformTypes.Get
    (
    -- * REST Resource
      PlatformTypesGetResource

    -- * Creating a Request
    , platformTypesGet
    , PlatformTypesGet

    -- * Request Lenses
    , ptgProFileId
    , ptgId
    , ptgFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.platformTypes.get@ method which the
-- 'PlatformTypesGet' request conforms to.
type PlatformTypesGetResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "platformTypes" :>
               Capture "id" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] PlatformType

-- | Gets one platform type by ID.
--
-- /See:/ 'platformTypesGet' smart constructor.
data PlatformTypesGet = PlatformTypesGet'
    { _ptgProFileId :: !(Textual Int64)
    , _ptgId :: !(Textual Int64)
    , _ptgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PlatformTypesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptgProFileId'
--
-- * 'ptgId'
--
-- * 'ptgFields'
platformTypesGet
    :: Int64 -- ^ 'ptgProFileId'
    -> Int64 -- ^ 'ptgId'
    -> PlatformTypesGet
platformTypesGet pPtgProFileId_ pPtgId_ = 
    PlatformTypesGet'
    { _ptgProFileId = _Coerce # pPtgProFileId_
    , _ptgId = _Coerce # pPtgId_
    , _ptgFields = Nothing
    }

-- | User profile ID associated with this request.
ptgProFileId :: Lens' PlatformTypesGet Int64
ptgProFileId
  = lens _ptgProFileId (\ s a -> s{_ptgProFileId = a})
      . _Coerce

-- | Platform type ID.
ptgId :: Lens' PlatformTypesGet Int64
ptgId
  = lens _ptgId (\ s a -> s{_ptgId = a}) . _Coerce

-- | Selector specifying which fields to include in a partial response.
ptgFields :: Lens' PlatformTypesGet (Maybe Text)
ptgFields
  = lens _ptgFields (\ s a -> s{_ptgFields = a})

instance GoogleRequest PlatformTypesGet where
        type Rs PlatformTypesGet = PlatformType
        type Scopes PlatformTypesGet =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient PlatformTypesGet'{..}
          = go _ptgProFileId _ptgId _ptgFields (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy PlatformTypesGetResource)
                      mempty
