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
-- Module      : Network.Google.Resource.Games.Events.ListByPlayer
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list showing the current progress on events in this
-- application for the currently authenticated user.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.events.listByPlayer@.
module Network.Google.Resource.Games.Events.ListByPlayer
    (
    -- * REST Resource
      EventsListByPlayerResource

    -- * Creating a Request
    , eventsListByPlayer
    , EventsListByPlayer

    -- * Request Lenses
    , elbpConsistencyToken
    , elbpLanguage
    , elbpPageToken
    , elbpMaxResults
    , elbpFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.events.listByPlayer@ method which the
-- 'EventsListByPlayer' request conforms to.
type EventsListByPlayerResource =
     "games" :>
       "v1" :>
         "events" :>
           QueryParam "consistencyToken" (Textual Int64) :>
             QueryParam "language" Text :>
               QueryParam "pageToken" Text :>
                 QueryParam "maxResults" (Textual Int32) :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       Get '[JSON] PlayerEventListResponse

-- | Returns a list showing the current progress on events in this
-- application for the currently authenticated user.
--
-- /See:/ 'eventsListByPlayer' smart constructor.
data EventsListByPlayer = EventsListByPlayer'
    { _elbpConsistencyToken :: !(Maybe (Textual Int64))
    , _elbpLanguage :: !(Maybe Text)
    , _elbpPageToken :: !(Maybe Text)
    , _elbpMaxResults :: !(Maybe (Textual Int32))
    , _elbpFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EventsListByPlayer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elbpConsistencyToken'
--
-- * 'elbpLanguage'
--
-- * 'elbpPageToken'
--
-- * 'elbpMaxResults'
--
-- * 'elbpFields'
eventsListByPlayer
    :: EventsListByPlayer
eventsListByPlayer = 
    EventsListByPlayer'
    { _elbpConsistencyToken = Nothing
    , _elbpLanguage = Nothing
    , _elbpPageToken = Nothing
    , _elbpMaxResults = Nothing
    , _elbpFields = Nothing
    }

-- | The last-seen mutation timestamp.
elbpConsistencyToken :: Lens' EventsListByPlayer (Maybe Int64)
elbpConsistencyToken
  = lens _elbpConsistencyToken
      (\ s a -> s{_elbpConsistencyToken = a})
      . mapping _Coerce

-- | The preferred language to use for strings returned by this method.
elbpLanguage :: Lens' EventsListByPlayer (Maybe Text)
elbpLanguage
  = lens _elbpLanguage (\ s a -> s{_elbpLanguage = a})

-- | The token returned by the previous request.
elbpPageToken :: Lens' EventsListByPlayer (Maybe Text)
elbpPageToken
  = lens _elbpPageToken
      (\ s a -> s{_elbpPageToken = a})

-- | The maximum number of events to return in the response, used for paging.
-- For any response, the actual number of events to return may be less than
-- the specified maxResults.
elbpMaxResults :: Lens' EventsListByPlayer (Maybe Int32)
elbpMaxResults
  = lens _elbpMaxResults
      (\ s a -> s{_elbpMaxResults = a})
      . mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
elbpFields :: Lens' EventsListByPlayer (Maybe Text)
elbpFields
  = lens _elbpFields (\ s a -> s{_elbpFields = a})

instance GoogleRequest EventsListByPlayer where
        type Rs EventsListByPlayer = PlayerEventListResponse
        type Scopes EventsListByPlayer =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient EventsListByPlayer'{..}
          = go _elbpConsistencyToken _elbpLanguage
              _elbpPageToken
              _elbpMaxResults
              _elbpFields
              (Just AltJSON)
              gamesService
          where go
                  = buildClient
                      (Proxy :: Proxy EventsListByPlayerResource)
                      mempty
