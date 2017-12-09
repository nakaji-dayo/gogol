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
-- Module      : Network.Google.Resource.Directory.Groups.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete Group
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.groups.delete@.
module Network.Google.Resource.Directory.Groups.Delete
    (
    -- * REST Resource
      GroupsDeleteResource

    -- * Creating a Request
    , groupsDelete
    , GroupsDelete

    -- * Request Lenses
    , gdGroupKey
    , gdFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.groups.delete@ method which the
-- 'GroupsDelete' request conforms to.
type GroupsDeleteResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "groups" :>
             Capture "groupKey" Text :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Delete Group
--
-- /See:/ 'groupsDelete' smart constructor.
data GroupsDelete = GroupsDelete'
    { _gdGroupKey :: !Text
    , _gdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdGroupKey'
--
-- * 'gdFields'
groupsDelete
    :: Text -- ^ 'gdGroupKey'
    -> GroupsDelete
groupsDelete pGdGroupKey_ = 
    GroupsDelete'
    { _gdGroupKey = pGdGroupKey_
    , _gdFields = Nothing
    }

-- | Email or immutable ID of the group
gdGroupKey :: Lens' GroupsDelete Text
gdGroupKey
  = lens _gdGroupKey (\ s a -> s{_gdGroupKey = a})

-- | Selector specifying which fields to include in a partial response.
gdFields :: Lens' GroupsDelete (Maybe Text)
gdFields = lens _gdFields (\ s a -> s{_gdFields = a})

instance GoogleRequest GroupsDelete where
        type Rs GroupsDelete = ()
        type Scopes GroupsDelete =
             '["https://www.googleapis.com/auth/admin.directory.group"]
        requestClient GroupsDelete'{..}
          = go _gdGroupKey _gdFields (Just AltJSON)
              directoryService
          where go
                  = buildClient (Proxy :: Proxy GroupsDeleteResource)
                      mempty
