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
-- Module      : Network.Google.Resource.GamesManagement.Achievements.ResetAll
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets all achievements for the currently authenticated player for your
-- application. This method is only accessible to whitelisted tester
-- accounts for your application.
--
-- /See:/ <https://developers.google.com/games/services Google Play Game Services Management API Reference> for @gamesManagement.achievements.resetAll@.
module Network.Google.Resource.GamesManagement.Achievements.ResetAll
    (
    -- * REST Resource
      AchievementsResetAllResource

    -- * Creating a Request
    , achievementsResetAll
    , AchievementsResetAll

    -- * Request Lenses
    , araFields
    ) where

import Network.Google.GamesManagement.Types
import Network.Google.Prelude

-- | A resource alias for @gamesManagement.achievements.resetAll@ method which the
-- 'AchievementsResetAll' request conforms to.
type AchievementsResetAllResource =
     "games" :>
       "v1management" :>
         "achievements" :>
           "reset" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 Post '[JSON] AchievementResetAllResponse

-- | Resets all achievements for the currently authenticated player for your
-- application. This method is only accessible to whitelisted tester
-- accounts for your application.
--
-- /See:/ 'achievementsResetAll' smart constructor.
newtype AchievementsResetAll = AchievementsResetAll'
    { _araFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AchievementsResetAll' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'araFields'
achievementsResetAll
    :: AchievementsResetAll
achievementsResetAll = 
    AchievementsResetAll'
    { _araFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
araFields :: Lens' AchievementsResetAll (Maybe Text)
araFields
  = lens _araFields (\ s a -> s{_araFields = a})

instance GoogleRequest AchievementsResetAll where
        type Rs AchievementsResetAll =
             AchievementResetAllResponse
        type Scopes AchievementsResetAll =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient AchievementsResetAll'{..}
          = go _araFields (Just AltJSON) gamesManagementService
          where go
                  = buildClient
                      (Proxy :: Proxy AchievementsResetAllResource)
                      mempty
