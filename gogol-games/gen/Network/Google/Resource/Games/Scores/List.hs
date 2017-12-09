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
-- Module      : Network.Google.Resource.Games.Scores.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the scores in a leaderboard, starting from the top.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.scores.list@.
module Network.Google.Resource.Games.Scores.List
    (
    -- * REST Resource
      ScoresListResource

    -- * Creating a Request
    , scoresList
    , ScoresList

    -- * Request Lenses
    , sllConsistencyToken
    , sllCollection
    , sllTimeSpan
    , sllLeaderboardId
    , sllLanguage
    , sllPageToken
    , sllMaxResults
    , sllFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.scores.list@ method which the
-- 'ScoresList' request conforms to.
type ScoresListResource =
     "games" :>
       "v1" :>
         "leaderboards" :>
           Capture "leaderboardId" Text :>
             "scores" :>
               Capture "collection" ScoresListCollection :>
                 QueryParam "timeSpan" ScoresListTimeSpan :>
                   QueryParam "consistencyToken" (Textual Int64) :>
                     QueryParam "language" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "maxResults" (Textual Int32) :>
                           QueryParam "fields" Text :>
                             QueryParam "alt" AltJSON :>
                               Get '[JSON] LeaderboardScores

-- | Lists the scores in a leaderboard, starting from the top.
--
-- /See:/ 'scoresList' smart constructor.
data ScoresList = ScoresList'
    { _sllConsistencyToken :: !(Maybe (Textual Int64))
    , _sllCollection :: !ScoresListCollection
    , _sllTimeSpan :: !ScoresListTimeSpan
    , _sllLeaderboardId :: !Text
    , _sllLanguage :: !(Maybe Text)
    , _sllPageToken :: !(Maybe Text)
    , _sllMaxResults :: !(Maybe (Textual Int32))
    , _sllFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScoresList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sllConsistencyToken'
--
-- * 'sllCollection'
--
-- * 'sllTimeSpan'
--
-- * 'sllLeaderboardId'
--
-- * 'sllLanguage'
--
-- * 'sllPageToken'
--
-- * 'sllMaxResults'
--
-- * 'sllFields'
scoresList
    :: ScoresListCollection -- ^ 'sllCollection'
    -> ScoresListTimeSpan -- ^ 'sllTimeSpan'
    -> Text -- ^ 'sllLeaderboardId'
    -> ScoresList
scoresList pSllCollection_ pSllTimeSpan_ pSllLeaderboardId_ = 
    ScoresList'
    { _sllConsistencyToken = Nothing
    , _sllCollection = pSllCollection_
    , _sllTimeSpan = pSllTimeSpan_
    , _sllLeaderboardId = pSllLeaderboardId_
    , _sllLanguage = Nothing
    , _sllPageToken = Nothing
    , _sllMaxResults = Nothing
    , _sllFields = Nothing
    }

-- | The last-seen mutation timestamp.
sllConsistencyToken :: Lens' ScoresList (Maybe Int64)
sllConsistencyToken
  = lens _sllConsistencyToken
      (\ s a -> s{_sllConsistencyToken = a})
      . mapping _Coerce

-- | The collection of scores you\'re requesting.
sllCollection :: Lens' ScoresList ScoresListCollection
sllCollection
  = lens _sllCollection
      (\ s a -> s{_sllCollection = a})

-- | The time span for the scores and ranks you\'re requesting.
sllTimeSpan :: Lens' ScoresList ScoresListTimeSpan
sllTimeSpan
  = lens _sllTimeSpan (\ s a -> s{_sllTimeSpan = a})

-- | The ID of the leaderboard.
sllLeaderboardId :: Lens' ScoresList Text
sllLeaderboardId
  = lens _sllLeaderboardId
      (\ s a -> s{_sllLeaderboardId = a})

-- | The preferred language to use for strings returned by this method.
sllLanguage :: Lens' ScoresList (Maybe Text)
sllLanguage
  = lens _sllLanguage (\ s a -> s{_sllLanguage = a})

-- | The token returned by the previous request.
sllPageToken :: Lens' ScoresList (Maybe Text)
sllPageToken
  = lens _sllPageToken (\ s a -> s{_sllPageToken = a})

-- | The maximum number of leaderboard scores to return in the response. For
-- any response, the actual number of leaderboard scores returned may be
-- less than the specified maxResults.
sllMaxResults :: Lens' ScoresList (Maybe Int32)
sllMaxResults
  = lens _sllMaxResults
      (\ s a -> s{_sllMaxResults = a})
      . mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
sllFields :: Lens' ScoresList (Maybe Text)
sllFields
  = lens _sllFields (\ s a -> s{_sllFields = a})

instance GoogleRequest ScoresList where
        type Rs ScoresList = LeaderboardScores
        type Scopes ScoresList =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient ScoresList'{..}
          = go _sllLeaderboardId _sllCollection
              (Just _sllTimeSpan)
              _sllConsistencyToken
              _sllLanguage
              _sllPageToken
              _sllMaxResults
              _sllFields
              (Just AltJSON)
              gamesService
          where go
                  = buildClient (Proxy :: Proxy ScoresListResource)
                      mempty
