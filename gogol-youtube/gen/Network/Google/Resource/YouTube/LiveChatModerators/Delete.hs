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
-- Module      : Network.Google.Resource.YouTube.LiveChatModerators.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a chat moderator.
--
-- /See:/ <https://developers.google.com/youtube/v3 YouTube Data API Reference> for @youtube.liveChatModerators.delete@.
module Network.Google.Resource.YouTube.LiveChatModerators.Delete
    (
    -- * REST Resource
      LiveChatModeratorsDeleteResource

    -- * Creating a Request
    , liveChatModeratorsDelete
    , LiveChatModeratorsDelete

    -- * Request Lenses
    , lcmdId
    , lcmdFields
    ) where

import Network.Google.Prelude
import Network.Google.YouTube.Types

-- | A resource alias for @youtube.liveChatModerators.delete@ method which the
-- 'LiveChatModeratorsDelete' request conforms to.
type LiveChatModeratorsDeleteResource =
     "youtube" :>
       "v3" :>
         "liveChat" :>
           "moderators" :>
             QueryParam "id" Text :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Removes a chat moderator.
--
-- /See:/ 'liveChatModeratorsDelete' smart constructor.
data LiveChatModeratorsDelete = LiveChatModeratorsDelete'
    { _lcmdId :: !Text
    , _lcmdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LiveChatModeratorsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcmdId'
--
-- * 'lcmdFields'
liveChatModeratorsDelete
    :: Text -- ^ 'lcmdId'
    -> LiveChatModeratorsDelete
liveChatModeratorsDelete pLcmdId_ = 
    LiveChatModeratorsDelete'
    { _lcmdId = pLcmdId_
    , _lcmdFields = Nothing
    }

-- | The id parameter identifies the chat moderator to remove. The value
-- uniquely identifies both the moderator and the chat.
lcmdId :: Lens' LiveChatModeratorsDelete Text
lcmdId = lens _lcmdId (\ s a -> s{_lcmdId = a})

-- | Selector specifying which fields to include in a partial response.
lcmdFields :: Lens' LiveChatModeratorsDelete (Maybe Text)
lcmdFields
  = lens _lcmdFields (\ s a -> s{_lcmdFields = a})

instance GoogleRequest LiveChatModeratorsDelete where
        type Rs LiveChatModeratorsDelete = ()
        type Scopes LiveChatModeratorsDelete =
             '["https://www.googleapis.com/auth/youtube",
               "https://www.googleapis.com/auth/youtube.force-ssl"]
        requestClient LiveChatModeratorsDelete'{..}
          = go (Just _lcmdId) _lcmdFields (Just AltJSON)
              youTubeService
          where go
                  = buildClient
                      (Proxy :: Proxy LiveChatModeratorsDeleteResource)
                      mempty
