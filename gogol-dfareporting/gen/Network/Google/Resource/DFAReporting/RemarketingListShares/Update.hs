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
-- Module      : Network.Google.Resource.DFAReporting.RemarketingListShares.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing remarketing list share.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.remarketingListShares.update@.
module Network.Google.Resource.DFAReporting.RemarketingListShares.Update
    (
    -- * REST Resource
      RemarketingListSharesUpdateResource

    -- * Creating a Request
    , remarketingListSharesUpdate
    , RemarketingListSharesUpdate

    -- * Request Lenses
    , rlsuProFileId
    , rlsuPayload
    , rlsuFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.remarketingListShares.update@ method which the
-- 'RemarketingListSharesUpdate' request conforms to.
type RemarketingListSharesUpdateResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "remarketingListShares" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] RemarketingListShare :>
                     Put '[JSON] RemarketingListShare

-- | Updates an existing remarketing list share.
--
-- /See:/ 'remarketingListSharesUpdate' smart constructor.
data RemarketingListSharesUpdate = RemarketingListSharesUpdate'
    { _rlsuProFileId :: !(Textual Int64)
    , _rlsuPayload :: !RemarketingListShare
    , _rlsuFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemarketingListSharesUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlsuProFileId'
--
-- * 'rlsuPayload'
--
-- * 'rlsuFields'
remarketingListSharesUpdate
    :: Int64 -- ^ 'rlsuProFileId'
    -> RemarketingListShare -- ^ 'rlsuPayload'
    -> RemarketingListSharesUpdate
remarketingListSharesUpdate pRlsuProFileId_ pRlsuPayload_ = 
    RemarketingListSharesUpdate'
    { _rlsuProFileId = _Coerce # pRlsuProFileId_
    , _rlsuPayload = pRlsuPayload_
    , _rlsuFields = Nothing
    }

-- | User profile ID associated with this request.
rlsuProFileId :: Lens' RemarketingListSharesUpdate Int64
rlsuProFileId
  = lens _rlsuProFileId
      (\ s a -> s{_rlsuProFileId = a})
      . _Coerce

-- | Multipart request metadata.
rlsuPayload :: Lens' RemarketingListSharesUpdate RemarketingListShare
rlsuPayload
  = lens _rlsuPayload (\ s a -> s{_rlsuPayload = a})

-- | Selector specifying which fields to include in a partial response.
rlsuFields :: Lens' RemarketingListSharesUpdate (Maybe Text)
rlsuFields
  = lens _rlsuFields (\ s a -> s{_rlsuFields = a})

instance GoogleRequest RemarketingListSharesUpdate
         where
        type Rs RemarketingListSharesUpdate =
             RemarketingListShare
        type Scopes RemarketingListSharesUpdate =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient RemarketingListSharesUpdate'{..}
          = go _rlsuProFileId _rlsuFields (Just AltJSON)
              _rlsuPayload
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy RemarketingListSharesUpdateResource)
                      mempty
