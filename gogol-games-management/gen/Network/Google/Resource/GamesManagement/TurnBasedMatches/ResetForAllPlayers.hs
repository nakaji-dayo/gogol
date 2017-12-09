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
-- Module      : Network.Google.Resource.GamesManagement.TurnBasedMatches.ResetForAllPlayers
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes turn-based matches where the only match participants are from
-- whitelisted tester accounts for your application. This method is only
-- available to user accounts for your developer console.
--
-- /See:/ <https://developers.google.com/games/services Google Play Game Services Management API Reference> for @gamesManagement.turnBasedMatches.resetForAllPlayers@.
module Network.Google.Resource.GamesManagement.TurnBasedMatches.ResetForAllPlayers
    (
    -- * REST Resource
      TurnBasedMatchesResetForAllPlayersResource

    -- * Creating a Request
    , turnBasedMatchesResetForAllPlayers
    , TurnBasedMatchesResetForAllPlayers

    -- * Request Lenses
    , tbmrfapFields
    ) where

import Network.Google.GamesManagement.Types
import Network.Google.Prelude

-- | A resource alias for @gamesManagement.turnBasedMatches.resetForAllPlayers@ method which the
-- 'TurnBasedMatchesResetForAllPlayers' request conforms to.
type TurnBasedMatchesResetForAllPlayersResource =
     "games" :>
       "v1management" :>
         "turnbasedmatches" :>
           "resetForAllPlayers" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Post '[JSON] ()

-- | Deletes turn-based matches where the only match participants are from
-- whitelisted tester accounts for your application. This method is only
-- available to user accounts for your developer console.
--
-- /See:/ 'turnBasedMatchesResetForAllPlayers' smart constructor.
newtype TurnBasedMatchesResetForAllPlayers = TurnBasedMatchesResetForAllPlayers'
    { _tbmrfapFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TurnBasedMatchesResetForAllPlayers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbmrfapFields'
turnBasedMatchesResetForAllPlayers
    :: TurnBasedMatchesResetForAllPlayers
turnBasedMatchesResetForAllPlayers = 
    TurnBasedMatchesResetForAllPlayers'
    { _tbmrfapFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
tbmrfapFields :: Lens' TurnBasedMatchesResetForAllPlayers (Maybe Text)
tbmrfapFields
  = lens _tbmrfapFields
      (\ s a -> s{_tbmrfapFields = a})

instance GoogleRequest
         TurnBasedMatchesResetForAllPlayers where
        type Rs TurnBasedMatchesResetForAllPlayers = ()
        type Scopes TurnBasedMatchesResetForAllPlayers =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient TurnBasedMatchesResetForAllPlayers'{..}
          = go _tbmrfapFields (Just AltJSON)
              gamesManagementService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy TurnBasedMatchesResetForAllPlayersResource)
                      mempty
