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
-- Module      : Network.Google.Resource.Games.TurnBasedMatches.Join
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Join a turn-based match.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.turnBasedMatches.join@.
module Network.Google.Resource.Games.TurnBasedMatches.Join
    (
    -- * REST Resource
      TurnBasedMatchesJoinResource

    -- * Creating a Request
    , turnBasedMatchesJoin
    , TurnBasedMatchesJoin

    -- * Request Lenses
    , tbmjConsistencyToken
    , tbmjLanguage
    , tbmjMatchId
    , tbmjFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.turnBasedMatches.join@ method which the
-- 'TurnBasedMatchesJoin' request conforms to.
type TurnBasedMatchesJoinResource =
     "games" :>
       "v1" :>
         "turnbasedmatches" :>
           Capture "matchId" Text :>
             "join" :>
               QueryParam "consistencyToken" (Textual Int64) :>
                 QueryParam "language" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       Put '[JSON] TurnBasedMatch

-- | Join a turn-based match.
--
-- /See:/ 'turnBasedMatchesJoin' smart constructor.
data TurnBasedMatchesJoin = TurnBasedMatchesJoin'
    { _tbmjConsistencyToken :: !(Maybe (Textual Int64))
    , _tbmjLanguage :: !(Maybe Text)
    , _tbmjMatchId :: !Text
    , _tbmjFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TurnBasedMatchesJoin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbmjConsistencyToken'
--
-- * 'tbmjLanguage'
--
-- * 'tbmjMatchId'
--
-- * 'tbmjFields'
turnBasedMatchesJoin
    :: Text -- ^ 'tbmjMatchId'
    -> TurnBasedMatchesJoin
turnBasedMatchesJoin pTbmjMatchId_ = 
    TurnBasedMatchesJoin'
    { _tbmjConsistencyToken = Nothing
    , _tbmjLanguage = Nothing
    , _tbmjMatchId = pTbmjMatchId_
    , _tbmjFields = Nothing
    }

-- | The last-seen mutation timestamp.
tbmjConsistencyToken :: Lens' TurnBasedMatchesJoin (Maybe Int64)
tbmjConsistencyToken
  = lens _tbmjConsistencyToken
      (\ s a -> s{_tbmjConsistencyToken = a})
      . mapping _Coerce

-- | The preferred language to use for strings returned by this method.
tbmjLanguage :: Lens' TurnBasedMatchesJoin (Maybe Text)
tbmjLanguage
  = lens _tbmjLanguage (\ s a -> s{_tbmjLanguage = a})

-- | The ID of the match.
tbmjMatchId :: Lens' TurnBasedMatchesJoin Text
tbmjMatchId
  = lens _tbmjMatchId (\ s a -> s{_tbmjMatchId = a})

-- | Selector specifying which fields to include in a partial response.
tbmjFields :: Lens' TurnBasedMatchesJoin (Maybe Text)
tbmjFields
  = lens _tbmjFields (\ s a -> s{_tbmjFields = a})

instance GoogleRequest TurnBasedMatchesJoin where
        type Rs TurnBasedMatchesJoin = TurnBasedMatch
        type Scopes TurnBasedMatchesJoin =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient TurnBasedMatchesJoin'{..}
          = go _tbmjMatchId _tbmjConsistencyToken _tbmjLanguage
              _tbmjFields
              (Just AltJSON)
              gamesService
          where go
                  = buildClient
                      (Proxy :: Proxy TurnBasedMatchesJoinResource)
                      mempty
