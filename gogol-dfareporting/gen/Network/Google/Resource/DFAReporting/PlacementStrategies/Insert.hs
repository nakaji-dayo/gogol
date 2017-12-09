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
-- Module      : Network.Google.Resource.DFAReporting.PlacementStrategies.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts a new placement strategy.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.placementStrategies.insert@.
module Network.Google.Resource.DFAReporting.PlacementStrategies.Insert
    (
    -- * REST Resource
      PlacementStrategiesInsertResource

    -- * Creating a Request
    , placementStrategiesInsert
    , PlacementStrategiesInsert

    -- * Request Lenses
    , psiProFileId
    , psiPayload
    , psiFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.placementStrategies.insert@ method which the
-- 'PlacementStrategiesInsert' request conforms to.
type PlacementStrategiesInsertResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "placementStrategies" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] PlacementStrategy :>
                     Post '[JSON] PlacementStrategy

-- | Inserts a new placement strategy.
--
-- /See:/ 'placementStrategiesInsert' smart constructor.
data PlacementStrategiesInsert = PlacementStrategiesInsert'
    { _psiProFileId :: !(Textual Int64)
    , _psiPayload :: !PlacementStrategy
    , _psiFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PlacementStrategiesInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psiProFileId'
--
-- * 'psiPayload'
--
-- * 'psiFields'
placementStrategiesInsert
    :: Int64 -- ^ 'psiProFileId'
    -> PlacementStrategy -- ^ 'psiPayload'
    -> PlacementStrategiesInsert
placementStrategiesInsert pPsiProFileId_ pPsiPayload_ = 
    PlacementStrategiesInsert'
    { _psiProFileId = _Coerce # pPsiProFileId_
    , _psiPayload = pPsiPayload_
    , _psiFields = Nothing
    }

-- | User profile ID associated with this request.
psiProFileId :: Lens' PlacementStrategiesInsert Int64
psiProFileId
  = lens _psiProFileId (\ s a -> s{_psiProFileId = a})
      . _Coerce

-- | Multipart request metadata.
psiPayload :: Lens' PlacementStrategiesInsert PlacementStrategy
psiPayload
  = lens _psiPayload (\ s a -> s{_psiPayload = a})

-- | Selector specifying which fields to include in a partial response.
psiFields :: Lens' PlacementStrategiesInsert (Maybe Text)
psiFields
  = lens _psiFields (\ s a -> s{_psiFields = a})

instance GoogleRequest PlacementStrategiesInsert
         where
        type Rs PlacementStrategiesInsert = PlacementStrategy
        type Scopes PlacementStrategiesInsert =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient PlacementStrategiesInsert'{..}
          = go _psiProFileId _psiFields (Just AltJSON)
              _psiPayload
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy PlacementStrategiesInsertResource)
                      mempty
