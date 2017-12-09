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
-- Module      : Network.Google.Resource.Games.TurnBasedMatches.Decline
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decline an invitation to play a turn-based match.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.turnBasedMatches.decline@.
module Network.Google.Resource.Games.TurnBasedMatches.Decline
    (
    -- * REST Resource
      TurnBasedMatchesDeclineResource

    -- * Creating a Request
    , turnBasedMatchesDecline
    , TurnBasedMatchesDecline

    -- * Request Lenses
    , tbmdConsistencyToken
    , tbmdLanguage
    , tbmdMatchId
    , tbmdFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.turnBasedMatches.decline@ method which the
-- 'TurnBasedMatchesDecline' request conforms to.
type TurnBasedMatchesDeclineResource =
     "games" :>
       "v1" :>
         "turnbasedmatches" :>
           Capture "matchId" Text :>
             "decline" :>
               QueryParam "consistencyToken" (Textual Int64) :>
                 QueryParam "language" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       Put '[JSON] TurnBasedMatch

-- | Decline an invitation to play a turn-based match.
--
-- /See:/ 'turnBasedMatchesDecline' smart constructor.
data TurnBasedMatchesDecline = TurnBasedMatchesDecline'
    { _tbmdConsistencyToken :: !(Maybe (Textual Int64))
    , _tbmdLanguage :: !(Maybe Text)
    , _tbmdMatchId :: !Text
    , _tbmdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TurnBasedMatchesDecline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbmdConsistencyToken'
--
-- * 'tbmdLanguage'
--
-- * 'tbmdMatchId'
--
-- * 'tbmdFields'
turnBasedMatchesDecline
    :: Text -- ^ 'tbmdMatchId'
    -> TurnBasedMatchesDecline
turnBasedMatchesDecline pTbmdMatchId_ = 
    TurnBasedMatchesDecline'
    { _tbmdConsistencyToken = Nothing
    , _tbmdLanguage = Nothing
    , _tbmdMatchId = pTbmdMatchId_
    , _tbmdFields = Nothing
    }

-- | The last-seen mutation timestamp.
tbmdConsistencyToken :: Lens' TurnBasedMatchesDecline (Maybe Int64)
tbmdConsistencyToken
  = lens _tbmdConsistencyToken
      (\ s a -> s{_tbmdConsistencyToken = a})
      . mapping _Coerce

-- | The preferred language to use for strings returned by this method.
tbmdLanguage :: Lens' TurnBasedMatchesDecline (Maybe Text)
tbmdLanguage
  = lens _tbmdLanguage (\ s a -> s{_tbmdLanguage = a})

-- | The ID of the match.
tbmdMatchId :: Lens' TurnBasedMatchesDecline Text
tbmdMatchId
  = lens _tbmdMatchId (\ s a -> s{_tbmdMatchId = a})

-- | Selector specifying which fields to include in a partial response.
tbmdFields :: Lens' TurnBasedMatchesDecline (Maybe Text)
tbmdFields
  = lens _tbmdFields (\ s a -> s{_tbmdFields = a})

instance GoogleRequest TurnBasedMatchesDecline where
        type Rs TurnBasedMatchesDecline = TurnBasedMatch
        type Scopes TurnBasedMatchesDecline =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient TurnBasedMatchesDecline'{..}
          = go _tbmdMatchId _tbmdConsistencyToken _tbmdLanguage
              _tbmdFields
              (Just AltJSON)
              gamesService
          where go
                  = buildClient
                      (Proxy :: Proxy TurnBasedMatchesDeclineResource)
                      mempty
