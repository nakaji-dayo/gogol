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
-- Module      : Network.Google.Resource.Games.Rooms.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns invitations to join rooms.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.rooms.list@.
module Network.Google.Resource.Games.Rooms.List
    (
    -- * REST Resource
      RoomsListResource

    -- * Creating a Request
    , roomsList
    , RoomsList

    -- * Request Lenses
    , rConsistencyToken
    , rLanguage
    , rPageToken
    , rMaxResults
    , rFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.rooms.list@ method which the
-- 'RoomsList' request conforms to.
type RoomsListResource =
     "games" :>
       "v1" :>
         "rooms" :>
           QueryParam "consistencyToken" (Textual Int64) :>
             QueryParam "language" Text :>
               QueryParam "pageToken" Text :>
                 QueryParam "maxResults" (Textual Int32) :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :> Get '[JSON] RoomList

-- | Returns invitations to join rooms.
--
-- /See:/ 'roomsList' smart constructor.
data RoomsList = RoomsList'
    { _rConsistencyToken :: !(Maybe (Textual Int64))
    , _rLanguage :: !(Maybe Text)
    , _rPageToken :: !(Maybe Text)
    , _rMaxResults :: !(Maybe (Textual Int32))
    , _rFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RoomsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rConsistencyToken'
--
-- * 'rLanguage'
--
-- * 'rPageToken'
--
-- * 'rMaxResults'
--
-- * 'rFields'
roomsList
    :: RoomsList
roomsList = 
    RoomsList'
    { _rConsistencyToken = Nothing
    , _rLanguage = Nothing
    , _rPageToken = Nothing
    , _rMaxResults = Nothing
    , _rFields = Nothing
    }

-- | The last-seen mutation timestamp.
rConsistencyToken :: Lens' RoomsList (Maybe Int64)
rConsistencyToken
  = lens _rConsistencyToken
      (\ s a -> s{_rConsistencyToken = a})
      . mapping _Coerce

-- | The preferred language to use for strings returned by this method.
rLanguage :: Lens' RoomsList (Maybe Text)
rLanguage
  = lens _rLanguage (\ s a -> s{_rLanguage = a})

-- | The token returned by the previous request.
rPageToken :: Lens' RoomsList (Maybe Text)
rPageToken
  = lens _rPageToken (\ s a -> s{_rPageToken = a})

-- | The maximum number of rooms to return in the response, used for paging.
-- For any response, the actual number of rooms to return may be less than
-- the specified maxResults.
rMaxResults :: Lens' RoomsList (Maybe Int32)
rMaxResults
  = lens _rMaxResults (\ s a -> s{_rMaxResults = a}) .
      mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
rFields :: Lens' RoomsList (Maybe Text)
rFields = lens _rFields (\ s a -> s{_rFields = a})

instance GoogleRequest RoomsList where
        type Rs RoomsList = RoomList
        type Scopes RoomsList =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient RoomsList'{..}
          = go _rConsistencyToken _rLanguage _rPageToken
              _rMaxResults
              _rFields
              (Just AltJSON)
              gamesService
          where go
                  = buildClient (Proxy :: Proxy RoomsListResource)
                      mempty
