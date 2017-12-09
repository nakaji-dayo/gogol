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
-- Module      : Network.Google.Resource.GamesManagement.Events.Reset
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets all player progress on the event with the given ID for the
-- currently authenticated player. This method is only accessible to
-- whitelisted tester accounts for your application. All quests for this
-- player that use the event will also be reset.
--
-- /See:/ <https://developers.google.com/games/services Google Play Game Services Management API Reference> for @gamesManagement.events.reset@.
module Network.Google.Resource.GamesManagement.Events.Reset
    (
    -- * REST Resource
      EventsResetResource

    -- * Creating a Request
    , eventsReset
    , EventsReset

    -- * Request Lenses
    , erEventId
    , erFields
    ) where

import Network.Google.GamesManagement.Types
import Network.Google.Prelude

-- | A resource alias for @gamesManagement.events.reset@ method which the
-- 'EventsReset' request conforms to.
type EventsResetResource =
     "games" :>
       "v1management" :>
         "events" :>
           Capture "eventId" Text :>
             "reset" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Post '[JSON] ()

-- | Resets all player progress on the event with the given ID for the
-- currently authenticated player. This method is only accessible to
-- whitelisted tester accounts for your application. All quests for this
-- player that use the event will also be reset.
--
-- /See:/ 'eventsReset' smart constructor.
data EventsReset = EventsReset'
    { _erEventId :: !Text
    , _erFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EventsReset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erEventId'
--
-- * 'erFields'
eventsReset
    :: Text -- ^ 'erEventId'
    -> EventsReset
eventsReset pErEventId_ = 
    EventsReset'
    { _erEventId = pErEventId_
    , _erFields = Nothing
    }

-- | The ID of the event.
erEventId :: Lens' EventsReset Text
erEventId
  = lens _erEventId (\ s a -> s{_erEventId = a})

-- | Selector specifying which fields to include in a partial response.
erFields :: Lens' EventsReset (Maybe Text)
erFields = lens _erFields (\ s a -> s{_erFields = a})

instance GoogleRequest EventsReset where
        type Rs EventsReset = ()
        type Scopes EventsReset =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient EventsReset'{..}
          = go _erEventId _erFields (Just AltJSON)
              gamesManagementService
          where go
                  = buildClient (Proxy :: Proxy EventsResetResource)
                      mempty
