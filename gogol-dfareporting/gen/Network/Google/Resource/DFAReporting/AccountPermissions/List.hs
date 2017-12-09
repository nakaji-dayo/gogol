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
-- Module      : Network.Google.Resource.DFAReporting.AccountPermissions.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of account permissions.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.accountPermissions.list@.
module Network.Google.Resource.DFAReporting.AccountPermissions.List
    (
    -- * REST Resource
      AccountPermissionsListResource

    -- * Creating a Request
    , accountPermissionsList
    , AccountPermissionsList

    -- * Request Lenses
    , aplProFileId
    , aplFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.accountPermissions.list@ method which the
-- 'AccountPermissionsList' request conforms to.
type AccountPermissionsListResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "accountPermissions" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   Get '[JSON] AccountPermissionsListResponse

-- | Retrieves the list of account permissions.
--
-- /See:/ 'accountPermissionsList' smart constructor.
data AccountPermissionsList = AccountPermissionsList'
    { _aplProFileId :: !(Textual Int64)
    , _aplFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountPermissionsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aplProFileId'
--
-- * 'aplFields'
accountPermissionsList
    :: Int64 -- ^ 'aplProFileId'
    -> AccountPermissionsList
accountPermissionsList pAplProFileId_ = 
    AccountPermissionsList'
    { _aplProFileId = _Coerce # pAplProFileId_
    , _aplFields = Nothing
    }

-- | User profile ID associated with this request.
aplProFileId :: Lens' AccountPermissionsList Int64
aplProFileId
  = lens _aplProFileId (\ s a -> s{_aplProFileId = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
aplFields :: Lens' AccountPermissionsList (Maybe Text)
aplFields
  = lens _aplFields (\ s a -> s{_aplFields = a})

instance GoogleRequest AccountPermissionsList where
        type Rs AccountPermissionsList =
             AccountPermissionsListResponse
        type Scopes AccountPermissionsList =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient AccountPermissionsList'{..}
          = go _aplProFileId _aplFields (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy AccountPermissionsListResource)
                      mempty
