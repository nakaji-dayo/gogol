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
-- Module      : Network.Google.Resource.DFAReporting.CreativeFields.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets one creative field by ID.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.creativeFields.get@.
module Network.Google.Resource.DFAReporting.CreativeFields.Get
    (
    -- * REST Resource
      CreativeFieldsGetResource

    -- * Creating a Request
    , creativeFieldsGet
    , CreativeFieldsGet

    -- * Request Lenses
    , cfgProFileId
    , cfgId
    , cfgFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.creativeFields.get@ method which the
-- 'CreativeFieldsGet' request conforms to.
type CreativeFieldsGetResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "creativeFields" :>
               Capture "id" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] CreativeField

-- | Gets one creative field by ID.
--
-- /See:/ 'creativeFieldsGet' smart constructor.
data CreativeFieldsGet = CreativeFieldsGet'
    { _cfgProFileId :: !(Textual Int64)
    , _cfgId :: !(Textual Int64)
    , _cfgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreativeFieldsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfgProFileId'
--
-- * 'cfgId'
--
-- * 'cfgFields'
creativeFieldsGet
    :: Int64 -- ^ 'cfgProFileId'
    -> Int64 -- ^ 'cfgId'
    -> CreativeFieldsGet
creativeFieldsGet pCfgProFileId_ pCfgId_ = 
    CreativeFieldsGet'
    { _cfgProFileId = _Coerce # pCfgProFileId_
    , _cfgId = _Coerce # pCfgId_
    , _cfgFields = Nothing
    }

-- | User profile ID associated with this request.
cfgProFileId :: Lens' CreativeFieldsGet Int64
cfgProFileId
  = lens _cfgProFileId (\ s a -> s{_cfgProFileId = a})
      . _Coerce

-- | Creative Field ID
cfgId :: Lens' CreativeFieldsGet Int64
cfgId
  = lens _cfgId (\ s a -> s{_cfgId = a}) . _Coerce

-- | Selector specifying which fields to include in a partial response.
cfgFields :: Lens' CreativeFieldsGet (Maybe Text)
cfgFields
  = lens _cfgFields (\ s a -> s{_cfgFields = a})

instance GoogleRequest CreativeFieldsGet where
        type Rs CreativeFieldsGet = CreativeField
        type Scopes CreativeFieldsGet =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient CreativeFieldsGet'{..}
          = go _cfgProFileId _cfgId _cfgFields (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy CreativeFieldsGetResource)
                      mempty
