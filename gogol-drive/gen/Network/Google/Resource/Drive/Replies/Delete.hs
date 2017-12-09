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
-- Module      : Network.Google.Resource.Drive.Replies.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reply.
--
-- /See:/ <https://developers.google.com/drive/ Drive API Reference> for @drive.replies.delete@.
module Network.Google.Resource.Drive.Replies.Delete
    (
    -- * REST Resource
      RepliesDeleteResource

    -- * Creating a Request
    , repliesDelete
    , RepliesDelete

    -- * Request Lenses
    , rdReplyId
    , rdFileId
    , rdCommentId
    , rdFields
    ) where

import Network.Google.Drive.Types
import Network.Google.Prelude

-- | A resource alias for @drive.replies.delete@ method which the
-- 'RepliesDelete' request conforms to.
type RepliesDeleteResource =
     "drive" :>
       "v3" :>
         "files" :>
           Capture "fileId" Text :>
             "comments" :>
               Capture "commentId" Text :>
                 "replies" :>
                   Capture "replyId" Text :>
                     QueryParam "fields" Text :>
                       QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Deletes a reply.
--
-- /See:/ 'repliesDelete' smart constructor.
data RepliesDelete = RepliesDelete'
    { _rdReplyId :: !Text
    , _rdFileId :: !Text
    , _rdCommentId :: !Text
    , _rdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RepliesDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdReplyId'
--
-- * 'rdFileId'
--
-- * 'rdCommentId'
--
-- * 'rdFields'
repliesDelete
    :: Text -- ^ 'rdReplyId'
    -> Text -- ^ 'rdFileId'
    -> Text -- ^ 'rdCommentId'
    -> RepliesDelete
repliesDelete pRdReplyId_ pRdFileId_ pRdCommentId_ = 
    RepliesDelete'
    { _rdReplyId = pRdReplyId_
    , _rdFileId = pRdFileId_
    , _rdCommentId = pRdCommentId_
    , _rdFields = Nothing
    }

-- | The ID of the reply.
rdReplyId :: Lens' RepliesDelete Text
rdReplyId
  = lens _rdReplyId (\ s a -> s{_rdReplyId = a})

-- | The ID of the file.
rdFileId :: Lens' RepliesDelete Text
rdFileId = lens _rdFileId (\ s a -> s{_rdFileId = a})

-- | The ID of the comment.
rdCommentId :: Lens' RepliesDelete Text
rdCommentId
  = lens _rdCommentId (\ s a -> s{_rdCommentId = a})

-- | Selector specifying which fields to include in a partial response.
rdFields :: Lens' RepliesDelete (Maybe Text)
rdFields = lens _rdFields (\ s a -> s{_rdFields = a})

instance GoogleRequest RepliesDelete where
        type Rs RepliesDelete = ()
        type Scopes RepliesDelete =
             '["https://www.googleapis.com/auth/drive",
               "https://www.googleapis.com/auth/drive.file"]
        requestClient RepliesDelete'{..}
          = go _rdFileId _rdCommentId _rdReplyId _rdFields
              (Just AltJSON)
              driveService
          where go
                  = buildClient (Proxy :: Proxy RepliesDeleteResource)
                      mempty
