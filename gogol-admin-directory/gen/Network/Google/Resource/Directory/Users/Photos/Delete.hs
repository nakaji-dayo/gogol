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
-- Module      : Network.Google.Resource.Directory.Users.Photos.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove photos for the user
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.users.photos.delete@.
module Network.Google.Resource.Directory.Users.Photos.Delete
    (
    -- * REST Resource
      UsersPhotosDeleteResource

    -- * Creating a Request
    , usersPhotosDelete
    , UsersPhotosDelete

    -- * Request Lenses
    , updUserKey
    , updFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.users.photos.delete@ method which the
-- 'UsersPhotosDelete' request conforms to.
type UsersPhotosDeleteResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "users" :>
             Capture "userKey" Text :>
               "photos" :>
                 "thumbnail" :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Remove photos for the user
--
-- /See:/ 'usersPhotosDelete' smart constructor.
data UsersPhotosDelete = UsersPhotosDelete'
    { _updUserKey :: !Text
    , _updFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UsersPhotosDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updUserKey'
--
-- * 'updFields'
usersPhotosDelete
    :: Text -- ^ 'updUserKey'
    -> UsersPhotosDelete
usersPhotosDelete pUpdUserKey_ = 
    UsersPhotosDelete'
    { _updUserKey = pUpdUserKey_
    , _updFields = Nothing
    }

-- | Email or immutable ID of the user
updUserKey :: Lens' UsersPhotosDelete Text
updUserKey
  = lens _updUserKey (\ s a -> s{_updUserKey = a})

-- | Selector specifying which fields to include in a partial response.
updFields :: Lens' UsersPhotosDelete (Maybe Text)
updFields
  = lens _updFields (\ s a -> s{_updFields = a})

instance GoogleRequest UsersPhotosDelete where
        type Rs UsersPhotosDelete = ()
        type Scopes UsersPhotosDelete =
             '["https://www.googleapis.com/auth/admin.directory.user"]
        requestClient UsersPhotosDelete'{..}
          = go _updUserKey _updFields (Just AltJSON)
              directoryService
          where go
                  = buildClient
                      (Proxy :: Proxy UsersPhotosDeleteResource)
                      mempty
