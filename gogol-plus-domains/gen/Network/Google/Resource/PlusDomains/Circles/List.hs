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
-- Module      : Network.Google.Resource.PlusDomains.Circles.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all of the circles for a user.
--
-- /See:/ <https://developers.google.com/+/domains/ Google+ Domains API Reference> for @plusDomains.circles.list@.
module Network.Google.Resource.PlusDomains.Circles.List
    (
    -- * REST Resource
      CirclesListResource

    -- * Creating a Request
    , circlesList
    , CirclesList

    -- * Request Lenses
    , cirUserId
    , cirPageToken
    , cirMaxResults
    , cirFields
    ) where

import Network.Google.PlusDomains.Types
import Network.Google.Prelude

-- | A resource alias for @plusDomains.circles.list@ method which the
-- 'CirclesList' request conforms to.
type CirclesListResource =
     "plusDomains" :>
       "v1" :>
         "people" :>
           Capture "userId" Text :>
             "circles" :>
               QueryParam "pageToken" Text :>
                 QueryParam "maxResults" (Textual Word32) :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :> Get '[JSON] CircleFeed

-- | List all of the circles for a user.
--
-- /See:/ 'circlesList' smart constructor.
data CirclesList = CirclesList'
    { _cirUserId :: !Text
    , _cirPageToken :: !(Maybe Text)
    , _cirMaxResults :: !(Textual Word32)
    , _cirFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CirclesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cirUserId'
--
-- * 'cirPageToken'
--
-- * 'cirMaxResults'
--
-- * 'cirFields'
circlesList
    :: Text -- ^ 'cirUserId'
    -> CirclesList
circlesList pCirUserId_ = 
    CirclesList'
    { _cirUserId = pCirUserId_
    , _cirPageToken = Nothing
    , _cirMaxResults = 20
    , _cirFields = Nothing
    }

-- | The ID of the user to get circles for. The special value \"me\" can be
-- used to indicate the authenticated user.
cirUserId :: Lens' CirclesList Text
cirUserId
  = lens _cirUserId (\ s a -> s{_cirUserId = a})

-- | The continuation token, which is used to page through large result sets.
-- To get the next page of results, set this parameter to the value of
-- \"nextPageToken\" from the previous response.
cirPageToken :: Lens' CirclesList (Maybe Text)
cirPageToken
  = lens _cirPageToken (\ s a -> s{_cirPageToken = a})

-- | The maximum number of circles to include in the response, which is used
-- for paging. For any response, the actual number returned might be less
-- than the specified maxResults.
cirMaxResults :: Lens' CirclesList Word32
cirMaxResults
  = lens _cirMaxResults
      (\ s a -> s{_cirMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
cirFields :: Lens' CirclesList (Maybe Text)
cirFields
  = lens _cirFields (\ s a -> s{_cirFields = a})

instance GoogleRequest CirclesList where
        type Rs CirclesList = CircleFeed
        type Scopes CirclesList =
             '["https://www.googleapis.com/auth/plus.circles.read",
               "https://www.googleapis.com/auth/plus.login",
               "https://www.googleapis.com/auth/plus.me"]
        requestClient CirclesList'{..}
          = go _cirUserId _cirPageToken (Just _cirMaxResults)
              _cirFields
              (Just AltJSON)
              plusDomainsService
          where go
                  = buildClient (Proxy :: Proxy CirclesListResource)
                      mempty
