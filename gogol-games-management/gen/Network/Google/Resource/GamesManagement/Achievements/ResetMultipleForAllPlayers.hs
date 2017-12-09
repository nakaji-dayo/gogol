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
-- Module      : Network.Google.Resource.GamesManagement.Achievements.ResetMultipleForAllPlayers
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets achievements with the given IDs for all players. This method is
-- only available to user accounts for your developer console. Only draft
-- achievements may be reset.
--
-- /See:/ <https://developers.google.com/games/services Google Play Game Services Management API Reference> for @gamesManagement.achievements.resetMultipleForAllPlayers@.
module Network.Google.Resource.GamesManagement.Achievements.ResetMultipleForAllPlayers
    (
    -- * REST Resource
      AchievementsResetMultipleForAllPlayersResource

    -- * Creating a Request
    , achievementsResetMultipleForAllPlayers
    , AchievementsResetMultipleForAllPlayers

    -- * Request Lenses
    , armfapPayload
    , armfapFields
    ) where

import Network.Google.GamesManagement.Types
import Network.Google.Prelude

-- | A resource alias for @gamesManagement.achievements.resetMultipleForAllPlayers@ method which the
-- 'AchievementsResetMultipleForAllPlayers' request conforms to.
type AchievementsResetMultipleForAllPlayersResource =
     "games" :>
       "v1management" :>
         "achievements" :>
           "resetMultipleForAllPlayers" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] AchievementResetMultipleForAllRequest
                   :> Post '[JSON] ()

-- | Resets achievements with the given IDs for all players. This method is
-- only available to user accounts for your developer console. Only draft
-- achievements may be reset.
--
-- /See:/ 'achievementsResetMultipleForAllPlayers' smart constructor.
data AchievementsResetMultipleForAllPlayers = AchievementsResetMultipleForAllPlayers'
    { _armfapPayload :: !AchievementResetMultipleForAllRequest
    , _armfapFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AchievementsResetMultipleForAllPlayers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'armfapPayload'
--
-- * 'armfapFields'
achievementsResetMultipleForAllPlayers
    :: AchievementResetMultipleForAllRequest -- ^ 'armfapPayload'
    -> AchievementsResetMultipleForAllPlayers
achievementsResetMultipleForAllPlayers pArmfapPayload_ = 
    AchievementsResetMultipleForAllPlayers'
    { _armfapPayload = pArmfapPayload_
    , _armfapFields = Nothing
    }

-- | Multipart request metadata.
armfapPayload :: Lens' AchievementsResetMultipleForAllPlayers AchievementResetMultipleForAllRequest
armfapPayload
  = lens _armfapPayload
      (\ s a -> s{_armfapPayload = a})

-- | Selector specifying which fields to include in a partial response.
armfapFields :: Lens' AchievementsResetMultipleForAllPlayers (Maybe Text)
armfapFields
  = lens _armfapFields (\ s a -> s{_armfapFields = a})

instance GoogleRequest
         AchievementsResetMultipleForAllPlayers where
        type Rs AchievementsResetMultipleForAllPlayers = ()
        type Scopes AchievementsResetMultipleForAllPlayers =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient
          AchievementsResetMultipleForAllPlayers'{..}
          = go _armfapFields (Just AltJSON) _armfapPayload
              gamesManagementService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AchievementsResetMultipleForAllPlayersResource)
                      mempty
