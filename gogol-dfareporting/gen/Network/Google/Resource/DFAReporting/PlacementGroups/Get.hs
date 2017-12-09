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
-- Module      : Network.Google.Resource.DFAReporting.PlacementGroups.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets one placement group by ID.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.placementGroups.get@.
module Network.Google.Resource.DFAReporting.PlacementGroups.Get
    (
    -- * REST Resource
      PlacementGroupsGetResource

    -- * Creating a Request
    , placementGroupsGet
    , PlacementGroupsGet

    -- * Request Lenses
    , pggProFileId
    , pggId
    , pggFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.placementGroups.get@ method which the
-- 'PlacementGroupsGet' request conforms to.
type PlacementGroupsGetResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "placementGroups" :>
               Capture "id" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] PlacementGroup

-- | Gets one placement group by ID.
--
-- /See:/ 'placementGroupsGet' smart constructor.
data PlacementGroupsGet = PlacementGroupsGet'
    { _pggProFileId :: !(Textual Int64)
    , _pggId :: !(Textual Int64)
    , _pggFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PlacementGroupsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pggProFileId'
--
-- * 'pggId'
--
-- * 'pggFields'
placementGroupsGet
    :: Int64 -- ^ 'pggProFileId'
    -> Int64 -- ^ 'pggId'
    -> PlacementGroupsGet
placementGroupsGet pPggProFileId_ pPggId_ = 
    PlacementGroupsGet'
    { _pggProFileId = _Coerce # pPggProFileId_
    , _pggId = _Coerce # pPggId_
    , _pggFields = Nothing
    }

-- | User profile ID associated with this request.
pggProFileId :: Lens' PlacementGroupsGet Int64
pggProFileId
  = lens _pggProFileId (\ s a -> s{_pggProFileId = a})
      . _Coerce

-- | Placement group ID.
pggId :: Lens' PlacementGroupsGet Int64
pggId
  = lens _pggId (\ s a -> s{_pggId = a}) . _Coerce

-- | Selector specifying which fields to include in a partial response.
pggFields :: Lens' PlacementGroupsGet (Maybe Text)
pggFields
  = lens _pggFields (\ s a -> s{_pggFields = a})

instance GoogleRequest PlacementGroupsGet where
        type Rs PlacementGroupsGet = PlacementGroup
        type Scopes PlacementGroupsGet =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient PlacementGroupsGet'{..}
          = go _pggProFileId _pggId _pggFields (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy PlacementGroupsGetResource)
                      mempty
