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
-- Module      : Network.Google.Resource.GamesConfiguration.LeaderboardConfigurations.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the leaderboard configuration with the given ID.
--
-- /See:/ <https://developers.google.com/games/services Google Play Game Services Publishing API Reference> for @gamesConfiguration.leaderboardConfigurations.delete@.
module Network.Google.Resource.GamesConfiguration.LeaderboardConfigurations.Delete
    (
    -- * REST Resource
      LeaderboardConfigurationsDeleteResource

    -- * Creating a Request
    , leaderboardConfigurationsDelete
    , LeaderboardConfigurationsDelete

    -- * Request Lenses
    , lcdLeaderboardId
    , lcdFields
    ) where

import Network.Google.GamesConfiguration.Types
import Network.Google.Prelude

-- | A resource alias for @gamesConfiguration.leaderboardConfigurations.delete@ method which the
-- 'LeaderboardConfigurationsDelete' request conforms to.
type LeaderboardConfigurationsDeleteResource =
     "games" :>
       "v1configuration" :>
         "leaderboards" :>
           Capture "leaderboardId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Delete the leaderboard configuration with the given ID.
--
-- /See:/ 'leaderboardConfigurationsDelete' smart constructor.
data LeaderboardConfigurationsDelete = LeaderboardConfigurationsDelete'
    { _lcdLeaderboardId :: !Text
    , _lcdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LeaderboardConfigurationsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcdLeaderboardId'
--
-- * 'lcdFields'
leaderboardConfigurationsDelete
    :: Text -- ^ 'lcdLeaderboardId'
    -> LeaderboardConfigurationsDelete
leaderboardConfigurationsDelete pLcdLeaderboardId_ = 
    LeaderboardConfigurationsDelete'
    { _lcdLeaderboardId = pLcdLeaderboardId_
    , _lcdFields = Nothing
    }

-- | The ID of the leaderboard.
lcdLeaderboardId :: Lens' LeaderboardConfigurationsDelete Text
lcdLeaderboardId
  = lens _lcdLeaderboardId
      (\ s a -> s{_lcdLeaderboardId = a})

-- | Selector specifying which fields to include in a partial response.
lcdFields :: Lens' LeaderboardConfigurationsDelete (Maybe Text)
lcdFields
  = lens _lcdFields (\ s a -> s{_lcdFields = a})

instance GoogleRequest
         LeaderboardConfigurationsDelete where
        type Rs LeaderboardConfigurationsDelete = ()
        type Scopes LeaderboardConfigurationsDelete =
             '["https://www.googleapis.com/auth/androidpublisher"]
        requestClient LeaderboardConfigurationsDelete'{..}
          = go _lcdLeaderboardId _lcdFields (Just AltJSON)
              gamesConfigurationService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy LeaderboardConfigurationsDeleteResource)
                      mempty
