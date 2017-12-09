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
-- Module      : Network.Google.Resource.Games.Rooms.Leave
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Leave a room. For internal use by the Games SDK only. Calling this
-- method directly is unsupported.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.rooms.leave@.
module Network.Google.Resource.Games.Rooms.Leave
    (
    -- * REST Resource
      RoomsLeaveResource

    -- * Creating a Request
    , roomsLeave
    , RoomsLeave

    -- * Request Lenses
    , rlConsistencyToken
    , rlPayload
    , rlRoomId
    , rlLanguage
    , rlFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.rooms.leave@ method which the
-- 'RoomsLeave' request conforms to.
type RoomsLeaveResource =
     "games" :>
       "v1" :>
         "rooms" :>
           Capture "roomId" Text :>
             "leave" :>
               QueryParam "consistencyToken" (Textual Int64) :>
                 QueryParam "language" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON] RoomLeaveRequest :> Post '[JSON] Room

-- | Leave a room. For internal use by the Games SDK only. Calling this
-- method directly is unsupported.
--
-- /See:/ 'roomsLeave' smart constructor.
data RoomsLeave = RoomsLeave'
    { _rlConsistencyToken :: !(Maybe (Textual Int64))
    , _rlPayload :: !RoomLeaveRequest
    , _rlRoomId :: !Text
    , _rlLanguage :: !(Maybe Text)
    , _rlFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RoomsLeave' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlConsistencyToken'
--
-- * 'rlPayload'
--
-- * 'rlRoomId'
--
-- * 'rlLanguage'
--
-- * 'rlFields'
roomsLeave
    :: RoomLeaveRequest -- ^ 'rlPayload'
    -> Text -- ^ 'rlRoomId'
    -> RoomsLeave
roomsLeave pRlPayload_ pRlRoomId_ = 
    RoomsLeave'
    { _rlConsistencyToken = Nothing
    , _rlPayload = pRlPayload_
    , _rlRoomId = pRlRoomId_
    , _rlLanguage = Nothing
    , _rlFields = Nothing
    }

-- | The last-seen mutation timestamp.
rlConsistencyToken :: Lens' RoomsLeave (Maybe Int64)
rlConsistencyToken
  = lens _rlConsistencyToken
      (\ s a -> s{_rlConsistencyToken = a})
      . mapping _Coerce

-- | Multipart request metadata.
rlPayload :: Lens' RoomsLeave RoomLeaveRequest
rlPayload
  = lens _rlPayload (\ s a -> s{_rlPayload = a})

-- | The ID of the room.
rlRoomId :: Lens' RoomsLeave Text
rlRoomId = lens _rlRoomId (\ s a -> s{_rlRoomId = a})

-- | The preferred language to use for strings returned by this method.
rlLanguage :: Lens' RoomsLeave (Maybe Text)
rlLanguage
  = lens _rlLanguage (\ s a -> s{_rlLanguage = a})

-- | Selector specifying which fields to include in a partial response.
rlFields :: Lens' RoomsLeave (Maybe Text)
rlFields = lens _rlFields (\ s a -> s{_rlFields = a})

instance GoogleRequest RoomsLeave where
        type Rs RoomsLeave = Room
        type Scopes RoomsLeave =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient RoomsLeave'{..}
          = go _rlRoomId _rlConsistencyToken _rlLanguage
              _rlFields
              (Just AltJSON)
              _rlPayload
              gamesService
          where go
                  = buildClient (Proxy :: Proxy RoomsLeaveResource)
                      mempty
