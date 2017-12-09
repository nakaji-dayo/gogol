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
-- Module      : Network.Google.Resource.DFAReporting.RemarketingLists.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing remarketing list.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.remarketingLists.update@.
module Network.Google.Resource.DFAReporting.RemarketingLists.Update
    (
    -- * REST Resource
      RemarketingListsUpdateResource

    -- * Creating a Request
    , remarketingListsUpdate
    , RemarketingListsUpdate

    -- * Request Lenses
    , rluProFileId
    , rluPayload
    , rluFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.remarketingLists.update@ method which the
-- 'RemarketingListsUpdate' request conforms to.
type RemarketingListsUpdateResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "remarketingLists" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] RemarketingList :>
                     Put '[JSON] RemarketingList

-- | Updates an existing remarketing list.
--
-- /See:/ 'remarketingListsUpdate' smart constructor.
data RemarketingListsUpdate = RemarketingListsUpdate'
    { _rluProFileId :: !(Textual Int64)
    , _rluPayload :: !RemarketingList
    , _rluFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemarketingListsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rluProFileId'
--
-- * 'rluPayload'
--
-- * 'rluFields'
remarketingListsUpdate
    :: Int64 -- ^ 'rluProFileId'
    -> RemarketingList -- ^ 'rluPayload'
    -> RemarketingListsUpdate
remarketingListsUpdate pRluProFileId_ pRluPayload_ = 
    RemarketingListsUpdate'
    { _rluProFileId = _Coerce # pRluProFileId_
    , _rluPayload = pRluPayload_
    , _rluFields = Nothing
    }

-- | User profile ID associated with this request.
rluProFileId :: Lens' RemarketingListsUpdate Int64
rluProFileId
  = lens _rluProFileId (\ s a -> s{_rluProFileId = a})
      . _Coerce

-- | Multipart request metadata.
rluPayload :: Lens' RemarketingListsUpdate RemarketingList
rluPayload
  = lens _rluPayload (\ s a -> s{_rluPayload = a})

-- | Selector specifying which fields to include in a partial response.
rluFields :: Lens' RemarketingListsUpdate (Maybe Text)
rluFields
  = lens _rluFields (\ s a -> s{_rluFields = a})

instance GoogleRequest RemarketingListsUpdate where
        type Rs RemarketingListsUpdate = RemarketingList
        type Scopes RemarketingListsUpdate =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient RemarketingListsUpdate'{..}
          = go _rluProFileId _rluFields (Just AltJSON)
              _rluPayload
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy RemarketingListsUpdateResource)
                      mempty
