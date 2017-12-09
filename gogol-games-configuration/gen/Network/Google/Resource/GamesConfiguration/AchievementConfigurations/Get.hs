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
-- Module      : Network.Google.Resource.GamesConfiguration.AchievementConfigurations.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata of the achievement configuration with the given
-- ID.
--
-- /See:/ <https://developers.google.com/games/services Google Play Game Services Publishing API Reference> for @gamesConfiguration.achievementConfigurations.get@.
module Network.Google.Resource.GamesConfiguration.AchievementConfigurations.Get
    (
    -- * REST Resource
      AchievementConfigurationsGetResource

    -- * Creating a Request
    , achievementConfigurationsGet
    , AchievementConfigurationsGet

    -- * Request Lenses
    , acgAchievementId
    , acgFields
    ) where

import Network.Google.GamesConfiguration.Types
import Network.Google.Prelude

-- | A resource alias for @gamesConfiguration.achievementConfigurations.get@ method which the
-- 'AchievementConfigurationsGet' request conforms to.
type AchievementConfigurationsGetResource =
     "games" :>
       "v1configuration" :>
         "achievements" :>
           Capture "achievementId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 Get '[JSON] AchievementConfiguration

-- | Retrieves the metadata of the achievement configuration with the given
-- ID.
--
-- /See:/ 'achievementConfigurationsGet' smart constructor.
data AchievementConfigurationsGet = AchievementConfigurationsGet'
    { _acgAchievementId :: !Text
    , _acgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AchievementConfigurationsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acgAchievementId'
--
-- * 'acgFields'
achievementConfigurationsGet
    :: Text -- ^ 'acgAchievementId'
    -> AchievementConfigurationsGet
achievementConfigurationsGet pAcgAchievementId_ = 
    AchievementConfigurationsGet'
    { _acgAchievementId = pAcgAchievementId_
    , _acgFields = Nothing
    }

-- | The ID of the achievement used by this method.
acgAchievementId :: Lens' AchievementConfigurationsGet Text
acgAchievementId
  = lens _acgAchievementId
      (\ s a -> s{_acgAchievementId = a})

-- | Selector specifying which fields to include in a partial response.
acgFields :: Lens' AchievementConfigurationsGet (Maybe Text)
acgFields
  = lens _acgFields (\ s a -> s{_acgFields = a})

instance GoogleRequest AchievementConfigurationsGet
         where
        type Rs AchievementConfigurationsGet =
             AchievementConfiguration
        type Scopes AchievementConfigurationsGet =
             '["https://www.googleapis.com/auth/androidpublisher"]
        requestClient AchievementConfigurationsGet'{..}
          = go _acgAchievementId _acgFields (Just AltJSON)
              gamesConfigurationService
          where go
                  = buildClient
                      (Proxy :: Proxy AchievementConfigurationsGetResource)
                      mempty
