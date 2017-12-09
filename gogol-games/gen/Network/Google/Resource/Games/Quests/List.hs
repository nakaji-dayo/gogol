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
-- Module      : Network.Google.Resource.Games.Quests.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of quests for your application and the currently
-- authenticated player.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.quests.list@.
module Network.Google.Resource.Games.Quests.List
    (
    -- * REST Resource
      QuestsListResource

    -- * Creating a Request
    , questsList
    , QuestsList

    -- * Request Lenses
    , qlConsistencyToken
    , qlLanguage
    , qlPageToken
    , qlPlayerId
    , qlMaxResults
    , qlFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.quests.list@ method which the
-- 'QuestsList' request conforms to.
type QuestsListResource =
     "games" :>
       "v1" :>
         "players" :>
           Capture "playerId" Text :>
             "quests" :>
               QueryParam "consistencyToken" (Textual Int64) :>
                 QueryParam "language" Text :>
                   QueryParam "pageToken" Text :>
                     QueryParam "maxResults" (Textual Int32) :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON] QuestListResponse

-- | Get a list of quests for your application and the currently
-- authenticated player.
--
-- /See:/ 'questsList' smart constructor.
data QuestsList = QuestsList'
    { _qlConsistencyToken :: !(Maybe (Textual Int64))
    , _qlLanguage :: !(Maybe Text)
    , _qlPageToken :: !(Maybe Text)
    , _qlPlayerId :: !Text
    , _qlMaxResults :: !(Maybe (Textual Int32))
    , _qlFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'QuestsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qlConsistencyToken'
--
-- * 'qlLanguage'
--
-- * 'qlPageToken'
--
-- * 'qlPlayerId'
--
-- * 'qlMaxResults'
--
-- * 'qlFields'
questsList
    :: Text -- ^ 'qlPlayerId'
    -> QuestsList
questsList pQlPlayerId_ = 
    QuestsList'
    { _qlConsistencyToken = Nothing
    , _qlLanguage = Nothing
    , _qlPageToken = Nothing
    , _qlPlayerId = pQlPlayerId_
    , _qlMaxResults = Nothing
    , _qlFields = Nothing
    }

-- | The last-seen mutation timestamp.
qlConsistencyToken :: Lens' QuestsList (Maybe Int64)
qlConsistencyToken
  = lens _qlConsistencyToken
      (\ s a -> s{_qlConsistencyToken = a})
      . mapping _Coerce

-- | The preferred language to use for strings returned by this method.
qlLanguage :: Lens' QuestsList (Maybe Text)
qlLanguage
  = lens _qlLanguage (\ s a -> s{_qlLanguage = a})

-- | The token returned by the previous request.
qlPageToken :: Lens' QuestsList (Maybe Text)
qlPageToken
  = lens _qlPageToken (\ s a -> s{_qlPageToken = a})

-- | A player ID. A value of me may be used in place of the authenticated
-- player\'s ID.
qlPlayerId :: Lens' QuestsList Text
qlPlayerId
  = lens _qlPlayerId (\ s a -> s{_qlPlayerId = a})

-- | The maximum number of quest resources to return in the response, used
-- for paging. For any response, the actual number of quest resources
-- returned may be less than the specified maxResults. Acceptable values
-- are 1 to 50, inclusive. (Default: 50).
qlMaxResults :: Lens' QuestsList (Maybe Int32)
qlMaxResults
  = lens _qlMaxResults (\ s a -> s{_qlMaxResults = a})
      . mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
qlFields :: Lens' QuestsList (Maybe Text)
qlFields = lens _qlFields (\ s a -> s{_qlFields = a})

instance GoogleRequest QuestsList where
        type Rs QuestsList = QuestListResponse
        type Scopes QuestsList =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient QuestsList'{..}
          = go _qlPlayerId _qlConsistencyToken _qlLanguage
              _qlPageToken
              _qlMaxResults
              _qlFields
              (Just AltJSON)
              gamesService
          where go
                  = buildClient (Proxy :: Proxy QuestsListResource)
                      mempty
