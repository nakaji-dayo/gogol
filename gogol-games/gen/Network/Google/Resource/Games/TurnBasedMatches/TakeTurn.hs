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
-- Module      : Network.Google.Resource.Games.TurnBasedMatches.TakeTurn
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Commit the results of a player turn.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.turnBasedMatches.takeTurn@.
module Network.Google.Resource.Games.TurnBasedMatches.TakeTurn
    (
    -- * REST Resource
      TurnBasedMatchesTakeTurnResource

    -- * Creating a Request
    , turnBasedMatchesTakeTurn
    , TurnBasedMatchesTakeTurn

    -- * Request Lenses
    , tbmttConsistencyToken
    , tbmttPayload
    , tbmttLanguage
    , tbmttMatchId
    , tbmttFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.turnBasedMatches.takeTurn@ method which the
-- 'TurnBasedMatchesTakeTurn' request conforms to.
type TurnBasedMatchesTakeTurnResource =
     "games" :>
       "v1" :>
         "turnbasedmatches" :>
           Capture "matchId" Text :>
             "turn" :>
               QueryParam "consistencyToken" (Textual Int64) :>
                 QueryParam "language" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       ReqBody '[JSON] TurnBasedMatchTurn :>
                         Put '[JSON] TurnBasedMatch

-- | Commit the results of a player turn.
--
-- /See:/ 'turnBasedMatchesTakeTurn' smart constructor.
data TurnBasedMatchesTakeTurn = TurnBasedMatchesTakeTurn'
    { _tbmttConsistencyToken :: !(Maybe (Textual Int64))
    , _tbmttPayload :: !TurnBasedMatchTurn
    , _tbmttLanguage :: !(Maybe Text)
    , _tbmttMatchId :: !Text
    , _tbmttFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TurnBasedMatchesTakeTurn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbmttConsistencyToken'
--
-- * 'tbmttPayload'
--
-- * 'tbmttLanguage'
--
-- * 'tbmttMatchId'
--
-- * 'tbmttFields'
turnBasedMatchesTakeTurn
    :: TurnBasedMatchTurn -- ^ 'tbmttPayload'
    -> Text -- ^ 'tbmttMatchId'
    -> TurnBasedMatchesTakeTurn
turnBasedMatchesTakeTurn pTbmttPayload_ pTbmttMatchId_ = 
    TurnBasedMatchesTakeTurn'
    { _tbmttConsistencyToken = Nothing
    , _tbmttPayload = pTbmttPayload_
    , _tbmttLanguage = Nothing
    , _tbmttMatchId = pTbmttMatchId_
    , _tbmttFields = Nothing
    }

-- | The last-seen mutation timestamp.
tbmttConsistencyToken :: Lens' TurnBasedMatchesTakeTurn (Maybe Int64)
tbmttConsistencyToken
  = lens _tbmttConsistencyToken
      (\ s a -> s{_tbmttConsistencyToken = a})
      . mapping _Coerce

-- | Multipart request metadata.
tbmttPayload :: Lens' TurnBasedMatchesTakeTurn TurnBasedMatchTurn
tbmttPayload
  = lens _tbmttPayload (\ s a -> s{_tbmttPayload = a})

-- | The preferred language to use for strings returned by this method.
tbmttLanguage :: Lens' TurnBasedMatchesTakeTurn (Maybe Text)
tbmttLanguage
  = lens _tbmttLanguage
      (\ s a -> s{_tbmttLanguage = a})

-- | The ID of the match.
tbmttMatchId :: Lens' TurnBasedMatchesTakeTurn Text
tbmttMatchId
  = lens _tbmttMatchId (\ s a -> s{_tbmttMatchId = a})

-- | Selector specifying which fields to include in a partial response.
tbmttFields :: Lens' TurnBasedMatchesTakeTurn (Maybe Text)
tbmttFields
  = lens _tbmttFields (\ s a -> s{_tbmttFields = a})

instance GoogleRequest TurnBasedMatchesTakeTurn where
        type Rs TurnBasedMatchesTakeTurn = TurnBasedMatch
        type Scopes TurnBasedMatchesTakeTurn =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient TurnBasedMatchesTakeTurn'{..}
          = go _tbmttMatchId _tbmttConsistencyToken
              _tbmttLanguage
              _tbmttFields
              (Just AltJSON)
              _tbmttPayload
              gamesService
          where go
                  = buildClient
                      (Proxy :: Proxy TurnBasedMatchesTakeTurnResource)
                      mempty
