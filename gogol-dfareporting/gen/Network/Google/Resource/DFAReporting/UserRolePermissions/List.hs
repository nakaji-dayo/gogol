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
-- Module      : Network.Google.Resource.DFAReporting.UserRolePermissions.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of user role permissions, possibly filtered.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.userRolePermissions.list@.
module Network.Google.Resource.DFAReporting.UserRolePermissions.List
    (
    -- * REST Resource
      UserRolePermissionsListResource

    -- * Creating a Request
    , userRolePermissionsList
    , UserRolePermissionsList

    -- * Request Lenses
    , urplIds
    , urplProFileId
    , urplFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.userRolePermissions.list@ method which the
-- 'UserRolePermissionsList' request conforms to.
type UserRolePermissionsListResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "userRolePermissions" :>
               QueryParams "ids" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON] UserRolePermissionsListResponse

-- | Gets a list of user role permissions, possibly filtered.
--
-- /See:/ 'userRolePermissionsList' smart constructor.
data UserRolePermissionsList = UserRolePermissionsList'
    { _urplIds :: !(Maybe [Textual Int64])
    , _urplProFileId :: !(Textual Int64)
    , _urplFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserRolePermissionsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urplIds'
--
-- * 'urplProFileId'
--
-- * 'urplFields'
userRolePermissionsList
    :: Int64 -- ^ 'urplProFileId'
    -> UserRolePermissionsList
userRolePermissionsList pUrplProFileId_ = 
    UserRolePermissionsList'
    { _urplIds = Nothing
    , _urplProFileId = _Coerce # pUrplProFileId_
    , _urplFields = Nothing
    }

-- | Select only user role permissions with these IDs.
urplIds :: Lens' UserRolePermissionsList [Int64]
urplIds
  = lens _urplIds (\ s a -> s{_urplIds = a}) . _Default
      . _Coerce

-- | User profile ID associated with this request.
urplProFileId :: Lens' UserRolePermissionsList Int64
urplProFileId
  = lens _urplProFileId
      (\ s a -> s{_urplProFileId = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
urplFields :: Lens' UserRolePermissionsList (Maybe Text)
urplFields
  = lens _urplFields (\ s a -> s{_urplFields = a})

instance GoogleRequest UserRolePermissionsList where
        type Rs UserRolePermissionsList =
             UserRolePermissionsListResponse
        type Scopes UserRolePermissionsList =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient UserRolePermissionsList'{..}
          = go _urplProFileId (_urplIds ^. _Default)
              _urplFields
              (Just AltJSON)
              dFAReportingService
          where go
                  = buildClient
                      (Proxy :: Proxy UserRolePermissionsListResource)
                      mempty
