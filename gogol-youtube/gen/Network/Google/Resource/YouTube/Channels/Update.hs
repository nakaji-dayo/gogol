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
-- Module      : Network.Google.Resource.YouTube.Channels.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a channel\'s metadata. Note that this method currently only
-- supports updates to the channel resource\'s brandingSettings and
-- invideoPromotion objects and their child properties.
--
-- /See:/ <https://developers.google.com/youtube/v3 YouTube Data API Reference> for @youtube.channels.update@.
module Network.Google.Resource.YouTube.Channels.Update
    (
    -- * REST Resource
      ChannelsUpdateResource

    -- * Creating a Request
    , channelsUpdate
    , ChannelsUpdate

    -- * Request Lenses
    , chaPart
    , chaPayload
    , chaOnBehalfOfContentOwner
    , chaFields
    ) where

import Network.Google.Prelude
import Network.Google.YouTube.Types

-- | A resource alias for @youtube.channels.update@ method which the
-- 'ChannelsUpdate' request conforms to.
type ChannelsUpdateResource =
     "youtube" :>
       "v3" :>
         "channels" :>
           QueryParam "part" Text :>
             QueryParam "onBehalfOfContentOwner" Text :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] Channel :> Put '[JSON] Channel

-- | Updates a channel\'s metadata. Note that this method currently only
-- supports updates to the channel resource\'s brandingSettings and
-- invideoPromotion objects and their child properties.
--
-- /See:/ 'channelsUpdate' smart constructor.
data ChannelsUpdate = ChannelsUpdate'
    { _chaPart :: !Text
    , _chaPayload :: !Channel
    , _chaOnBehalfOfContentOwner :: !(Maybe Text)
    , _chaFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChannelsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chaPart'
--
-- * 'chaPayload'
--
-- * 'chaOnBehalfOfContentOwner'
--
-- * 'chaFields'
channelsUpdate
    :: Text -- ^ 'chaPart'
    -> Channel -- ^ 'chaPayload'
    -> ChannelsUpdate
channelsUpdate pChaPart_ pChaPayload_ = 
    ChannelsUpdate'
    { _chaPart = pChaPart_
    , _chaPayload = pChaPayload_
    , _chaOnBehalfOfContentOwner = Nothing
    , _chaFields = Nothing
    }

-- | The part parameter serves two purposes in this operation. It identifies
-- the properties that the write operation will set as well as the
-- properties that the API response will include. The API currently only
-- allows the parameter value to be set to either brandingSettings or
-- invideoPromotion. (You cannot update both of those parts with a single
-- request.) Note that this method overrides the existing values for all of
-- the mutable properties that are contained in any parts that the
-- parameter value specifies.
chaPart :: Lens' ChannelsUpdate Text
chaPart = lens _chaPart (\ s a -> s{_chaPart = a})

-- | Multipart request metadata.
chaPayload :: Lens' ChannelsUpdate Channel
chaPayload
  = lens _chaPayload (\ s a -> s{_chaPayload = a})

-- | The onBehalfOfContentOwner parameter indicates that the authenticated
-- user is acting on behalf of the content owner specified in the parameter
-- value. This parameter is intended for YouTube content partners that own
-- and manage many different YouTube channels. It allows content owners to
-- authenticate once and get access to all their video and channel data,
-- without having to provide authentication credentials for each individual
-- channel. The actual CMS account that the user authenticates with needs
-- to be linked to the specified YouTube content owner.
chaOnBehalfOfContentOwner :: Lens' ChannelsUpdate (Maybe Text)
chaOnBehalfOfContentOwner
  = lens _chaOnBehalfOfContentOwner
      (\ s a -> s{_chaOnBehalfOfContentOwner = a})

-- | Selector specifying which fields to include in a partial response.
chaFields :: Lens' ChannelsUpdate (Maybe Text)
chaFields
  = lens _chaFields (\ s a -> s{_chaFields = a})

instance GoogleRequest ChannelsUpdate where
        type Rs ChannelsUpdate = Channel
        type Scopes ChannelsUpdate =
             '["https://www.googleapis.com/auth/youtube",
               "https://www.googleapis.com/auth/youtube.force-ssl",
               "https://www.googleapis.com/auth/youtubepartner"]
        requestClient ChannelsUpdate'{..}
          = go (Just _chaPart) _chaOnBehalfOfContentOwner
              _chaFields
              (Just AltJSON)
              _chaPayload
              youTubeService
          where go
                  = buildClient (Proxy :: Proxy ChannelsUpdateResource)
                      mempty
