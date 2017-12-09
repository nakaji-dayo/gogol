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
-- Module      : Network.Google.Resource.Games.Events.Record
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a batch of changes to the number of times events have occurred
-- for the currently authenticated user of this application.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.events.record@.
module Network.Google.Resource.Games.Events.Record
    (
    -- * REST Resource
      EventsRecordResource

    -- * Creating a Request
    , eventsRecord
    , EventsRecord

    -- * Request Lenses
    , erConsistencyToken
    , erPayload
    , erLanguage
    , erFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.events.record@ method which the
-- 'EventsRecord' request conforms to.
type EventsRecordResource =
     "games" :>
       "v1" :>
         "events" :>
           QueryParam "consistencyToken" (Textual Int64) :>
             QueryParam "language" Text :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] EventRecordRequest :>
                     Post '[JSON] EventUpdateResponse

-- | Records a batch of changes to the number of times events have occurred
-- for the currently authenticated user of this application.
--
-- /See:/ 'eventsRecord' smart constructor.
data EventsRecord = EventsRecord'
    { _erConsistencyToken :: !(Maybe (Textual Int64))
    , _erPayload :: !EventRecordRequest
    , _erLanguage :: !(Maybe Text)
    , _erFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EventsRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erConsistencyToken'
--
-- * 'erPayload'
--
-- * 'erLanguage'
--
-- * 'erFields'
eventsRecord
    :: EventRecordRequest -- ^ 'erPayload'
    -> EventsRecord
eventsRecord pErPayload_ = 
    EventsRecord'
    { _erConsistencyToken = Nothing
    , _erPayload = pErPayload_
    , _erLanguage = Nothing
    , _erFields = Nothing
    }

-- | The last-seen mutation timestamp.
erConsistencyToken :: Lens' EventsRecord (Maybe Int64)
erConsistencyToken
  = lens _erConsistencyToken
      (\ s a -> s{_erConsistencyToken = a})
      . mapping _Coerce

-- | Multipart request metadata.
erPayload :: Lens' EventsRecord EventRecordRequest
erPayload
  = lens _erPayload (\ s a -> s{_erPayload = a})

-- | The preferred language to use for strings returned by this method.
erLanguage :: Lens' EventsRecord (Maybe Text)
erLanguage
  = lens _erLanguage (\ s a -> s{_erLanguage = a})

-- | Selector specifying which fields to include in a partial response.
erFields :: Lens' EventsRecord (Maybe Text)
erFields = lens _erFields (\ s a -> s{_erFields = a})

instance GoogleRequest EventsRecord where
        type Rs EventsRecord = EventUpdateResponse
        type Scopes EventsRecord =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient EventsRecord'{..}
          = go _erConsistencyToken _erLanguage _erFields
              (Just AltJSON)
              _erPayload
              gamesService
          where go
                  = buildClient (Proxy :: Proxy EventsRecordResource)
                      mempty
