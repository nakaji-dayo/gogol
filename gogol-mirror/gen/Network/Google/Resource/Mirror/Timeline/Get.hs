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
-- Module      : Network.Google.Resource.Mirror.Timeline.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a single timeline item by ID.
--
-- /See:/ <https://developers.google.com/glass Google Mirror API Reference> for @mirror.timeline.get@.
module Network.Google.Resource.Mirror.Timeline.Get
    (
    -- * REST Resource
      TimelineGetResource

    -- * Creating a Request
    , timelineGet
    , TimelineGet

    -- * Request Lenses
    , tgId
    , tgFields
    ) where

import Network.Google.Mirror.Types
import Network.Google.Prelude

-- | A resource alias for @mirror.timeline.get@ method which the
-- 'TimelineGet' request conforms to.
type TimelineGetResource =
     "mirror" :>
       "v1" :>
         "timeline" :>
           Capture "id" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] TimelineItem

-- | Gets a single timeline item by ID.
--
-- /See:/ 'timelineGet' smart constructor.
data TimelineGet = TimelineGet'
    { _tgId :: !Text
    , _tgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TimelineGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgId'
--
-- * 'tgFields'
timelineGet
    :: Text -- ^ 'tgId'
    -> TimelineGet
timelineGet pTgId_ = 
    TimelineGet'
    { _tgId = pTgId_
    , _tgFields = Nothing
    }

-- | The ID of the timeline item.
tgId :: Lens' TimelineGet Text
tgId = lens _tgId (\ s a -> s{_tgId = a})

-- | Selector specifying which fields to include in a partial response.
tgFields :: Lens' TimelineGet (Maybe Text)
tgFields = lens _tgFields (\ s a -> s{_tgFields = a})

instance GoogleRequest TimelineGet where
        type Rs TimelineGet = TimelineItem
        type Scopes TimelineGet =
             '["https://www.googleapis.com/auth/glass.location",
               "https://www.googleapis.com/auth/glass.timeline"]
        requestClient TimelineGet'{..}
          = go _tgId _tgFields (Just AltJSON) mirrorService
          where go
                  = buildClient (Proxy :: Proxy TimelineGetResource)
                      mempty
