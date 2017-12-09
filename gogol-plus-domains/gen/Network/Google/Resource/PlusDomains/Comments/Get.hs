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
-- Module      : Network.Google.Resource.PlusDomains.Comments.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a comment.
--
-- /See:/ <https://developers.google.com/+/domains/ Google+ Domains API Reference> for @plusDomains.comments.get@.
module Network.Google.Resource.PlusDomains.Comments.Get
    (
    -- * REST Resource
      CommentsGetResource

    -- * Creating a Request
    , commentsGet
    , CommentsGet

    -- * Request Lenses
    , comCommentId
    , comFields
    ) where

import Network.Google.PlusDomains.Types
import Network.Google.Prelude

-- | A resource alias for @plusDomains.comments.get@ method which the
-- 'CommentsGet' request conforms to.
type CommentsGetResource =
     "plusDomains" :>
       "v1" :>
         "comments" :>
           Capture "commentId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] Comment

-- | Get a comment.
--
-- /See:/ 'commentsGet' smart constructor.
data CommentsGet = CommentsGet'
    { _comCommentId :: !Text
    , _comFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommentsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'comCommentId'
--
-- * 'comFields'
commentsGet
    :: Text -- ^ 'comCommentId'
    -> CommentsGet
commentsGet pComCommentId_ = 
    CommentsGet'
    { _comCommentId = pComCommentId_
    , _comFields = Nothing
    }

-- | The ID of the comment to get.
comCommentId :: Lens' CommentsGet Text
comCommentId
  = lens _comCommentId (\ s a -> s{_comCommentId = a})

-- | Selector specifying which fields to include in a partial response.
comFields :: Lens' CommentsGet (Maybe Text)
comFields
  = lens _comFields (\ s a -> s{_comFields = a})

instance GoogleRequest CommentsGet where
        type Rs CommentsGet = Comment
        type Scopes CommentsGet =
             '["https://www.googleapis.com/auth/plus.login",
               "https://www.googleapis.com/auth/plus.stream.read"]
        requestClient CommentsGet'{..}
          = go _comCommentId _comFields (Just AltJSON)
              plusDomainsService
          where go
                  = buildClient (Proxy :: Proxy CommentsGetResource)
                      mempty
