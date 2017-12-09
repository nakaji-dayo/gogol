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
-- Module      : Network.Google.Resource.YouTube.CommentThreads.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new top-level comment. To add a reply to an existing comment,
-- use the comments.insert method instead.
--
-- /See:/ <https://developers.google.com/youtube/v3 YouTube Data API Reference> for @youtube.commentThreads.insert@.
module Network.Google.Resource.YouTube.CommentThreads.Insert
    (
    -- * REST Resource
      CommentThreadsInsertResource

    -- * Creating a Request
    , commentThreadsInsert
    , CommentThreadsInsert

    -- * Request Lenses
    , ctiPart
    , ctiPayload
    , ctiFields
    ) where

import Network.Google.Prelude
import Network.Google.YouTube.Types

-- | A resource alias for @youtube.commentThreads.insert@ method which the
-- 'CommentThreadsInsert' request conforms to.
type CommentThreadsInsertResource =
     "youtube" :>
       "v3" :>
         "commentThreads" :>
           QueryParam "part" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] CommentThread :>
                   Post '[JSON] CommentThread

-- | Creates a new top-level comment. To add a reply to an existing comment,
-- use the comments.insert method instead.
--
-- /See:/ 'commentThreadsInsert' smart constructor.
data CommentThreadsInsert = CommentThreadsInsert'
    { _ctiPart :: !Text
    , _ctiPayload :: !CommentThread
    , _ctiFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommentThreadsInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctiPart'
--
-- * 'ctiPayload'
--
-- * 'ctiFields'
commentThreadsInsert
    :: Text -- ^ 'ctiPart'
    -> CommentThread -- ^ 'ctiPayload'
    -> CommentThreadsInsert
commentThreadsInsert pCtiPart_ pCtiPayload_ = 
    CommentThreadsInsert'
    { _ctiPart = pCtiPart_
    , _ctiPayload = pCtiPayload_
    , _ctiFields = Nothing
    }

-- | The part parameter identifies the properties that the API response will
-- include. Set the parameter value to snippet. The snippet part has a
-- quota cost of 2 units.
ctiPart :: Lens' CommentThreadsInsert Text
ctiPart = lens _ctiPart (\ s a -> s{_ctiPart = a})

-- | Multipart request metadata.
ctiPayload :: Lens' CommentThreadsInsert CommentThread
ctiPayload
  = lens _ctiPayload (\ s a -> s{_ctiPayload = a})

-- | Selector specifying which fields to include in a partial response.
ctiFields :: Lens' CommentThreadsInsert (Maybe Text)
ctiFields
  = lens _ctiFields (\ s a -> s{_ctiFields = a})

instance GoogleRequest CommentThreadsInsert where
        type Rs CommentThreadsInsert = CommentThread
        type Scopes CommentThreadsInsert =
             '["https://www.googleapis.com/auth/youtube.force-ssl"]
        requestClient CommentThreadsInsert'{..}
          = go (Just _ctiPart) _ctiFields (Just AltJSON)
              _ctiPayload
              youTubeService
          where go
                  = buildClient
                      (Proxy :: Proxy CommentThreadsInsertResource)
                      mempty
