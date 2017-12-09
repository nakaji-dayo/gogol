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
-- Module      : Network.Google.Resource.GamesManagement.Quests.Reset
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets all player progress on the quest with the given ID for the
-- currently authenticated player. This method is only accessible to
-- whitelisted tester accounts for your application.
--
-- /See:/ <https://developers.google.com/games/services Google Play Game Services Management API Reference> for @gamesManagement.quests.reset@.
module Network.Google.Resource.GamesManagement.Quests.Reset
    (
    -- * REST Resource
      QuestsResetResource

    -- * Creating a Request
    , questsReset
    , QuestsReset

    -- * Request Lenses
    , qrQuestId
    , qrFields
    ) where

import Network.Google.GamesManagement.Types
import Network.Google.Prelude

-- | A resource alias for @gamesManagement.quests.reset@ method which the
-- 'QuestsReset' request conforms to.
type QuestsResetResource =
     "games" :>
       "v1management" :>
         "quests" :>
           Capture "questId" Text :>
             "reset" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Post '[JSON] ()

-- | Resets all player progress on the quest with the given ID for the
-- currently authenticated player. This method is only accessible to
-- whitelisted tester accounts for your application.
--
-- /See:/ 'questsReset' smart constructor.
data QuestsReset = QuestsReset'
    { _qrQuestId :: !Text
    , _qrFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'QuestsReset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qrQuestId'
--
-- * 'qrFields'
questsReset
    :: Text -- ^ 'qrQuestId'
    -> QuestsReset
questsReset pQrQuestId_ = 
    QuestsReset'
    { _qrQuestId = pQrQuestId_
    , _qrFields = Nothing
    }

-- | The ID of the quest.
qrQuestId :: Lens' QuestsReset Text
qrQuestId
  = lens _qrQuestId (\ s a -> s{_qrQuestId = a})

-- | Selector specifying which fields to include in a partial response.
qrFields :: Lens' QuestsReset (Maybe Text)
qrFields = lens _qrFields (\ s a -> s{_qrFields = a})

instance GoogleRequest QuestsReset where
        type Rs QuestsReset = ()
        type Scopes QuestsReset =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient QuestsReset'{..}
          = go _qrQuestId _qrFields (Just AltJSON)
              gamesManagementService
          where go
                  = buildClient (Proxy :: Proxy QuestsResetResource)
                      mempty
