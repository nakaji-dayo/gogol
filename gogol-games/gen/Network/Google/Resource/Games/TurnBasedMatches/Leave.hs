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
-- Module      : Network.Google.Resource.Games.TurnBasedMatches.Leave
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Leave a turn-based match when it is not the current player\'s turn,
-- without canceling the match.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.turnBasedMatches.leave@.
module Network.Google.Resource.Games.TurnBasedMatches.Leave
    (
    -- * REST Resource
      TurnBasedMatchesLeaveResource

    -- * Creating a Request
    , turnBasedMatchesLeave
    , TurnBasedMatchesLeave

    -- * Request Lenses
    , tbmlConsistencyToken
    , tbmlLanguage
    , tbmlMatchId
    , tbmlFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.turnBasedMatches.leave@ method which the
-- 'TurnBasedMatchesLeave' request conforms to.
type TurnBasedMatchesLeaveResource =
     "games" :>
       "v1" :>
         "turnbasedmatches" :>
           Capture "matchId" Text :>
             "leave" :>
               QueryParam "consistencyToken" (Textual Int64) :>
                 QueryParam "language" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       Put '[JSON] TurnBasedMatch

-- | Leave a turn-based match when it is not the current player\'s turn,
-- without canceling the match.
--
-- /See:/ 'turnBasedMatchesLeave' smart constructor.
data TurnBasedMatchesLeave = TurnBasedMatchesLeave'
    { _tbmlConsistencyToken :: !(Maybe (Textual Int64))
    , _tbmlLanguage :: !(Maybe Text)
    , _tbmlMatchId :: !Text
    , _tbmlFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TurnBasedMatchesLeave' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbmlConsistencyToken'
--
-- * 'tbmlLanguage'
--
-- * 'tbmlMatchId'
--
-- * 'tbmlFields'
turnBasedMatchesLeave
    :: Text -- ^ 'tbmlMatchId'
    -> TurnBasedMatchesLeave
turnBasedMatchesLeave pTbmlMatchId_ = 
    TurnBasedMatchesLeave'
    { _tbmlConsistencyToken = Nothing
    , _tbmlLanguage = Nothing
    , _tbmlMatchId = pTbmlMatchId_
    , _tbmlFields = Nothing
    }

-- | The last-seen mutation timestamp.
tbmlConsistencyToken :: Lens' TurnBasedMatchesLeave (Maybe Int64)
tbmlConsistencyToken
  = lens _tbmlConsistencyToken
      (\ s a -> s{_tbmlConsistencyToken = a})
      . mapping _Coerce

-- | The preferred language to use for strings returned by this method.
tbmlLanguage :: Lens' TurnBasedMatchesLeave (Maybe Text)
tbmlLanguage
  = lens _tbmlLanguage (\ s a -> s{_tbmlLanguage = a})

-- | The ID of the match.
tbmlMatchId :: Lens' TurnBasedMatchesLeave Text
tbmlMatchId
  = lens _tbmlMatchId (\ s a -> s{_tbmlMatchId = a})

-- | Selector specifying which fields to include in a partial response.
tbmlFields :: Lens' TurnBasedMatchesLeave (Maybe Text)
tbmlFields
  = lens _tbmlFields (\ s a -> s{_tbmlFields = a})

instance GoogleRequest TurnBasedMatchesLeave where
        type Rs TurnBasedMatchesLeave = TurnBasedMatch
        type Scopes TurnBasedMatchesLeave =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient TurnBasedMatchesLeave'{..}
          = go _tbmlMatchId _tbmlConsistencyToken _tbmlLanguage
              _tbmlFields
              (Just AltJSON)
              gamesService
          where go
                  = buildClient
                      (Proxy :: Proxy TurnBasedMatchesLeaveResource)
                      mempty
