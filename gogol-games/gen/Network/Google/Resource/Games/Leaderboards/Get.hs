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
-- Module      : Network.Google.Resource.Games.Leaderboards.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata of the leaderboard with the given ID.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.leaderboards.get@.
module Network.Google.Resource.Games.Leaderboards.Get
    (
    -- * REST Resource
      LeaderboardsGetResource

    -- * Creating a Request
    , leaderboardsGet
    , LeaderboardsGet

    -- * Request Lenses
    , lgConsistencyToken
    , lgLeaderboardId
    , lgLanguage
    , lgFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.leaderboards.get@ method which the
-- 'LeaderboardsGet' request conforms to.
type LeaderboardsGetResource =
     "games" :>
       "v1" :>
         "leaderboards" :>
           Capture "leaderboardId" Text :>
             QueryParam "consistencyToken" (Textual Int64) :>
               QueryParam "language" Text :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] Leaderboard

-- | Retrieves the metadata of the leaderboard with the given ID.
--
-- /See:/ 'leaderboardsGet' smart constructor.
data LeaderboardsGet = LeaderboardsGet'
    { _lgConsistencyToken :: !(Maybe (Textual Int64))
    , _lgLeaderboardId :: !Text
    , _lgLanguage :: !(Maybe Text)
    , _lgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LeaderboardsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgConsistencyToken'
--
-- * 'lgLeaderboardId'
--
-- * 'lgLanguage'
--
-- * 'lgFields'
leaderboardsGet
    :: Text -- ^ 'lgLeaderboardId'
    -> LeaderboardsGet
leaderboardsGet pLgLeaderboardId_ = 
    LeaderboardsGet'
    { _lgConsistencyToken = Nothing
    , _lgLeaderboardId = pLgLeaderboardId_
    , _lgLanguage = Nothing
    , _lgFields = Nothing
    }

-- | The last-seen mutation timestamp.
lgConsistencyToken :: Lens' LeaderboardsGet (Maybe Int64)
lgConsistencyToken
  = lens _lgConsistencyToken
      (\ s a -> s{_lgConsistencyToken = a})
      . mapping _Coerce

-- | The ID of the leaderboard.
lgLeaderboardId :: Lens' LeaderboardsGet Text
lgLeaderboardId
  = lens _lgLeaderboardId
      (\ s a -> s{_lgLeaderboardId = a})

-- | The preferred language to use for strings returned by this method.
lgLanguage :: Lens' LeaderboardsGet (Maybe Text)
lgLanguage
  = lens _lgLanguage (\ s a -> s{_lgLanguage = a})

-- | Selector specifying which fields to include in a partial response.
lgFields :: Lens' LeaderboardsGet (Maybe Text)
lgFields = lens _lgFields (\ s a -> s{_lgFields = a})

instance GoogleRequest LeaderboardsGet where
        type Rs LeaderboardsGet = Leaderboard
        type Scopes LeaderboardsGet =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient LeaderboardsGet'{..}
          = go _lgLeaderboardId _lgConsistencyToken _lgLanguage
              _lgFields
              (Just AltJSON)
              gamesService
          where go
                  = buildClient
                      (Proxy :: Proxy LeaderboardsGetResource)
                      mempty
