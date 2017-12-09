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
-- Module      : Network.Google.Resource.DFAReporting.TargetableRemarketingLists.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets one remarketing list by ID.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.targetableRemarketingLists.get@.
module Network.Google.Resource.DFAReporting.TargetableRemarketingLists.Get
    (
    -- * REST Resource
      TargetableRemarketingListsGetResource

    -- * Creating a Request
    , targetableRemarketingListsGet
    , TargetableRemarketingListsGet

    -- * Request Lenses
    , trlgProFileId
    , trlgId
    , trlgFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.targetableRemarketingLists.get@ method which the
-- 'TargetableRemarketingListsGet' request conforms to.
type TargetableRemarketingListsGetResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "targetableRemarketingLists" :>
               Capture "id" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] TargetableRemarketingList

-- | Gets one remarketing list by ID.
--
-- /See:/ 'targetableRemarketingListsGet' smart constructor.
data TargetableRemarketingListsGet = TargetableRemarketingListsGet'
    { _trlgProFileId :: !(Textual Int64)
    , _trlgId :: !(Textual Int64)
    , _trlgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TargetableRemarketingListsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trlgProFileId'
--
-- * 'trlgId'
--
-- * 'trlgFields'
targetableRemarketingListsGet
    :: Int64 -- ^ 'trlgProFileId'
    -> Int64 -- ^ 'trlgId'
    -> TargetableRemarketingListsGet
targetableRemarketingListsGet pTrlgProFileId_ pTrlgId_ = 
    TargetableRemarketingListsGet'
    { _trlgProFileId = _Coerce # pTrlgProFileId_
    , _trlgId = _Coerce # pTrlgId_
    , _trlgFields = Nothing
    }

-- | User profile ID associated with this request.
trlgProFileId :: Lens' TargetableRemarketingListsGet Int64
trlgProFileId
  = lens _trlgProFileId
      (\ s a -> s{_trlgProFileId = a})
      . _Coerce

-- | Remarketing list ID.
trlgId :: Lens' TargetableRemarketingListsGet Int64
trlgId
  = lens _trlgId (\ s a -> s{_trlgId = a}) . _Coerce

-- | Selector specifying which fields to include in a partial response.
trlgFields :: Lens' TargetableRemarketingListsGet (Maybe Text)
trlgFields
  = lens _trlgFields (\ s a -> s{_trlgFields = a})

instance GoogleRequest TargetableRemarketingListsGet
         where
        type Rs TargetableRemarketingListsGet =
             TargetableRemarketingList
        type Scopes TargetableRemarketingListsGet =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient TargetableRemarketingListsGet'{..}
          = go _trlgProFileId _trlgId _trlgFields
              (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy TargetableRemarketingListsGetResource)
                      mempty
