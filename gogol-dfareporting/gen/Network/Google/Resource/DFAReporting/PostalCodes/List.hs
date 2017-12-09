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
-- Module      : Network.Google.Resource.DFAReporting.PostalCodes.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of postal codes.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.postalCodes.list@.
module Network.Google.Resource.DFAReporting.PostalCodes.List
    (
    -- * REST Resource
      PostalCodesListResource

    -- * Creating a Request
    , postalCodesList
    , PostalCodesList

    -- * Request Lenses
    , pclProFileId
    , pclFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.postalCodes.list@ method which the
-- 'PostalCodesList' request conforms to.
type PostalCodesListResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "postalCodes" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   Get '[JSON] PostalCodesListResponse

-- | Retrieves a list of postal codes.
--
-- /See:/ 'postalCodesList' smart constructor.
data PostalCodesList = PostalCodesList'
    { _pclProFileId :: !(Textual Int64)
    , _pclFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PostalCodesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pclProFileId'
--
-- * 'pclFields'
postalCodesList
    :: Int64 -- ^ 'pclProFileId'
    -> PostalCodesList
postalCodesList pPclProFileId_ = 
    PostalCodesList'
    { _pclProFileId = _Coerce # pPclProFileId_
    , _pclFields = Nothing
    }

-- | User profile ID associated with this request.
pclProFileId :: Lens' PostalCodesList Int64
pclProFileId
  = lens _pclProFileId (\ s a -> s{_pclProFileId = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
pclFields :: Lens' PostalCodesList (Maybe Text)
pclFields
  = lens _pclFields (\ s a -> s{_pclFields = a})

instance GoogleRequest PostalCodesList where
        type Rs PostalCodesList = PostalCodesListResponse
        type Scopes PostalCodesList =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient PostalCodesList'{..}
          = go _pclProFileId _pclFields (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy PostalCodesListResource)
                      mempty
