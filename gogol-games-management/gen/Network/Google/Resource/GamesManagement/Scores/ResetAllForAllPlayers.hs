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
-- Module      : Network.Google.Resource.GamesManagement.Scores.ResetAllForAllPlayers
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets scores for all draft leaderboards for all players. This method is
-- only available to user accounts for your developer console.
--
-- /See:/ <https://developers.google.com/games/services Google Play Game Services Management API Reference> for @gamesManagement.scores.resetAllForAllPlayers@.
module Network.Google.Resource.GamesManagement.Scores.ResetAllForAllPlayers
    (
    -- * REST Resource
      ScoresResetAllForAllPlayersResource

    -- * Creating a Request
    , scoresResetAllForAllPlayers
    , ScoresResetAllForAllPlayers

    -- * Request Lenses
    , srafapFields
    ) where

import Network.Google.GamesManagement.Types
import Network.Google.Prelude

-- | A resource alias for @gamesManagement.scores.resetAllForAllPlayers@ method which the
-- 'ScoresResetAllForAllPlayers' request conforms to.
type ScoresResetAllForAllPlayersResource =
     "games" :>
       "v1management" :>
         "scores" :>
           "resetAllForAllPlayers" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Post '[JSON] ()

-- | Resets scores for all draft leaderboards for all players. This method is
-- only available to user accounts for your developer console.
--
-- /See:/ 'scoresResetAllForAllPlayers' smart constructor.
newtype ScoresResetAllForAllPlayers = ScoresResetAllForAllPlayers'
    { _srafapFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScoresResetAllForAllPlayers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srafapFields'
scoresResetAllForAllPlayers
    :: ScoresResetAllForAllPlayers
scoresResetAllForAllPlayers = 
    ScoresResetAllForAllPlayers'
    { _srafapFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
srafapFields :: Lens' ScoresResetAllForAllPlayers (Maybe Text)
srafapFields
  = lens _srafapFields (\ s a -> s{_srafapFields = a})

instance GoogleRequest ScoresResetAllForAllPlayers
         where
        type Rs ScoresResetAllForAllPlayers = ()
        type Scopes ScoresResetAllForAllPlayers =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient ScoresResetAllForAllPlayers'{..}
          = go _srafapFields (Just AltJSON)
              gamesManagementService
          where go
                  = buildClient
                      (Proxy :: Proxy ScoresResetAllForAllPlayersResource)
                      mempty
