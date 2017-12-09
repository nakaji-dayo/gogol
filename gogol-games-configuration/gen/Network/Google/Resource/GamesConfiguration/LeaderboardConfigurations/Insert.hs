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
-- Module      : Network.Google.Resource.GamesConfiguration.LeaderboardConfigurations.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Insert a new leaderboard configuration in this application.
--
-- /See:/ <https://developers.google.com/games/services Google Play Game Services Publishing API Reference> for @gamesConfiguration.leaderboardConfigurations.insert@.
module Network.Google.Resource.GamesConfiguration.LeaderboardConfigurations.Insert
    (
    -- * REST Resource
      LeaderboardConfigurationsInsertResource

    -- * Creating a Request
    , leaderboardConfigurationsInsert
    , LeaderboardConfigurationsInsert

    -- * Request Lenses
    , lciPayload
    , lciApplicationId
    , lciFields
    ) where

import Network.Google.GamesConfiguration.Types
import Network.Google.Prelude

-- | A resource alias for @gamesConfiguration.leaderboardConfigurations.insert@ method which the
-- 'LeaderboardConfigurationsInsert' request conforms to.
type LeaderboardConfigurationsInsertResource =
     "games" :>
       "v1configuration" :>
         "applications" :>
           Capture "applicationId" Text :>
             "leaderboards" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] LeaderboardConfiguration :>
                     Post '[JSON] LeaderboardConfiguration

-- | Insert a new leaderboard configuration in this application.
--
-- /See:/ 'leaderboardConfigurationsInsert' smart constructor.
data LeaderboardConfigurationsInsert = LeaderboardConfigurationsInsert'
    { _lciPayload :: !LeaderboardConfiguration
    , _lciApplicationId :: !Text
    , _lciFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LeaderboardConfigurationsInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lciPayload'
--
-- * 'lciApplicationId'
--
-- * 'lciFields'
leaderboardConfigurationsInsert
    :: LeaderboardConfiguration -- ^ 'lciPayload'
    -> Text -- ^ 'lciApplicationId'
    -> LeaderboardConfigurationsInsert
leaderboardConfigurationsInsert pLciPayload_ pLciApplicationId_ = 
    LeaderboardConfigurationsInsert'
    { _lciPayload = pLciPayload_
    , _lciApplicationId = pLciApplicationId_
    , _lciFields = Nothing
    }

-- | Multipart request metadata.
lciPayload :: Lens' LeaderboardConfigurationsInsert LeaderboardConfiguration
lciPayload
  = lens _lciPayload (\ s a -> s{_lciPayload = a})

-- | The application ID from the Google Play developer console.
lciApplicationId :: Lens' LeaderboardConfigurationsInsert Text
lciApplicationId
  = lens _lciApplicationId
      (\ s a -> s{_lciApplicationId = a})

-- | Selector specifying which fields to include in a partial response.
lciFields :: Lens' LeaderboardConfigurationsInsert (Maybe Text)
lciFields
  = lens _lciFields (\ s a -> s{_lciFields = a})

instance GoogleRequest
         LeaderboardConfigurationsInsert where
        type Rs LeaderboardConfigurationsInsert =
             LeaderboardConfiguration
        type Scopes LeaderboardConfigurationsInsert =
             '["https://www.googleapis.com/auth/androidpublisher"]
        requestClient LeaderboardConfigurationsInsert'{..}
          = go _lciApplicationId _lciFields (Just AltJSON)
              _lciPayload
              gamesConfigurationService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy LeaderboardConfigurationsInsertResource)
                      mempty
