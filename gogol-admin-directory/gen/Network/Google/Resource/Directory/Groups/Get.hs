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
-- Module      : Network.Google.Resource.Directory.Groups.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve Group
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.groups.get@.
module Network.Google.Resource.Directory.Groups.Get
    (
    -- * REST Resource
      GroupsGetResource

    -- * Creating a Request
    , groupsGet
    , GroupsGet

    -- * Request Lenses
    , ggGroupKey
    , ggFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.groups.get@ method which the
-- 'GroupsGet' request conforms to.
type GroupsGetResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "groups" :>
             Capture "groupKey" Text :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] Group

-- | Retrieve Group
--
-- /See:/ 'groupsGet' smart constructor.
data GroupsGet = GroupsGet'
    { _ggGroupKey :: !Text
    , _ggFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GroupsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggGroupKey'
--
-- * 'ggFields'
groupsGet
    :: Text -- ^ 'ggGroupKey'
    -> GroupsGet
groupsGet pGgGroupKey_ = 
    GroupsGet'
    { _ggGroupKey = pGgGroupKey_
    , _ggFields = Nothing
    }

-- | Email or immutable ID of the group
ggGroupKey :: Lens' GroupsGet Text
ggGroupKey
  = lens _ggGroupKey (\ s a -> s{_ggGroupKey = a})

-- | Selector specifying which fields to include in a partial response.
ggFields :: Lens' GroupsGet (Maybe Text)
ggFields = lens _ggFields (\ s a -> s{_ggFields = a})

instance GoogleRequest GroupsGet where
        type Rs GroupsGet = Group
        type Scopes GroupsGet =
             '["https://www.googleapis.com/auth/admin.directory.group",
               "https://www.googleapis.com/auth/admin.directory.group.readonly"]
        requestClient GroupsGet'{..}
          = go _ggGroupKey _ggFields (Just AltJSON)
              directoryService
          where go
                  = buildClient (Proxy :: Proxy GroupsGetResource)
                      mempty
