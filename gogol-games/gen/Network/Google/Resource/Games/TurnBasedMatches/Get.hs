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
-- Module      : Network.Google.Resource.Games.TurnBasedMatches.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the data for a turn-based match.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.turnBasedMatches.get@.
module Network.Google.Resource.Games.TurnBasedMatches.Get
    (
    -- * REST Resource
      TurnBasedMatchesGetResource

    -- * Creating a Request
    , turnBasedMatchesGet
    , TurnBasedMatchesGet

    -- * Request Lenses
    , tbmgConsistencyToken
    , tbmgIncludeMatchData
    , tbmgLanguage
    , tbmgMatchId
    , tbmgFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.turnBasedMatches.get@ method which the
-- 'TurnBasedMatchesGet' request conforms to.
type TurnBasedMatchesGetResource =
     "games" :>
       "v1" :>
         "turnbasedmatches" :>
           Capture "matchId" Text :>
             QueryParam "consistencyToken" (Textual Int64) :>
               QueryParam "includeMatchData" Bool :>
                 QueryParam "language" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       Get '[JSON] TurnBasedMatch

-- | Get the data for a turn-based match.
--
-- /See:/ 'turnBasedMatchesGet' smart constructor.
data TurnBasedMatchesGet = TurnBasedMatchesGet'
    { _tbmgConsistencyToken :: !(Maybe (Textual Int64))
    , _tbmgIncludeMatchData :: !(Maybe Bool)
    , _tbmgLanguage :: !(Maybe Text)
    , _tbmgMatchId :: !Text
    , _tbmgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TurnBasedMatchesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbmgConsistencyToken'
--
-- * 'tbmgIncludeMatchData'
--
-- * 'tbmgLanguage'
--
-- * 'tbmgMatchId'
--
-- * 'tbmgFields'
turnBasedMatchesGet
    :: Text -- ^ 'tbmgMatchId'
    -> TurnBasedMatchesGet
turnBasedMatchesGet pTbmgMatchId_ = 
    TurnBasedMatchesGet'
    { _tbmgConsistencyToken = Nothing
    , _tbmgIncludeMatchData = Nothing
    , _tbmgLanguage = Nothing
    , _tbmgMatchId = pTbmgMatchId_
    , _tbmgFields = Nothing
    }

-- | The last-seen mutation timestamp.
tbmgConsistencyToken :: Lens' TurnBasedMatchesGet (Maybe Int64)
tbmgConsistencyToken
  = lens _tbmgConsistencyToken
      (\ s a -> s{_tbmgConsistencyToken = a})
      . mapping _Coerce

-- | Get match data along with metadata.
tbmgIncludeMatchData :: Lens' TurnBasedMatchesGet (Maybe Bool)
tbmgIncludeMatchData
  = lens _tbmgIncludeMatchData
      (\ s a -> s{_tbmgIncludeMatchData = a})

-- | The preferred language to use for strings returned by this method.
tbmgLanguage :: Lens' TurnBasedMatchesGet (Maybe Text)
tbmgLanguage
  = lens _tbmgLanguage (\ s a -> s{_tbmgLanguage = a})

-- | The ID of the match.
tbmgMatchId :: Lens' TurnBasedMatchesGet Text
tbmgMatchId
  = lens _tbmgMatchId (\ s a -> s{_tbmgMatchId = a})

-- | Selector specifying which fields to include in a partial response.
tbmgFields :: Lens' TurnBasedMatchesGet (Maybe Text)
tbmgFields
  = lens _tbmgFields (\ s a -> s{_tbmgFields = a})

instance GoogleRequest TurnBasedMatchesGet where
        type Rs TurnBasedMatchesGet = TurnBasedMatch
        type Scopes TurnBasedMatchesGet =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient TurnBasedMatchesGet'{..}
          = go _tbmgMatchId _tbmgConsistencyToken
              _tbmgIncludeMatchData
              _tbmgLanguage
              _tbmgFields
              (Just AltJSON)
              gamesService
          where go
                  = buildClient
                      (Proxy :: Proxy TurnBasedMatchesGetResource)
                      mempty
