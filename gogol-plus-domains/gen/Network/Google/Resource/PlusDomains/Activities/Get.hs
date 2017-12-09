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
-- Module      : Network.Google.Resource.PlusDomains.Activities.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get an activity.
--
-- /See:/ <https://developers.google.com/+/domains/ Google+ Domains API Reference> for @plusDomains.activities.get@.
module Network.Google.Resource.PlusDomains.Activities.Get
    (
    -- * REST Resource
      ActivitiesGetResource

    -- * Creating a Request
    , activitiesGet
    , ActivitiesGet

    -- * Request Lenses
    , agActivityId
    , agFields
    ) where

import Network.Google.PlusDomains.Types
import Network.Google.Prelude

-- | A resource alias for @plusDomains.activities.get@ method which the
-- 'ActivitiesGet' request conforms to.
type ActivitiesGetResource =
     "plusDomains" :>
       "v1" :>
         "activities" :>
           Capture "activityId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] Activity

-- | Get an activity.
--
-- /See:/ 'activitiesGet' smart constructor.
data ActivitiesGet = ActivitiesGet'
    { _agActivityId :: !Text
    , _agFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ActivitiesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agActivityId'
--
-- * 'agFields'
activitiesGet
    :: Text -- ^ 'agActivityId'
    -> ActivitiesGet
activitiesGet pAgActivityId_ = 
    ActivitiesGet'
    { _agActivityId = pAgActivityId_
    , _agFields = Nothing
    }

-- | The ID of the activity to get.
agActivityId :: Lens' ActivitiesGet Text
agActivityId
  = lens _agActivityId (\ s a -> s{_agActivityId = a})

-- | Selector specifying which fields to include in a partial response.
agFields :: Lens' ActivitiesGet (Maybe Text)
agFields = lens _agFields (\ s a -> s{_agFields = a})

instance GoogleRequest ActivitiesGet where
        type Rs ActivitiesGet = Activity
        type Scopes ActivitiesGet =
             '["https://www.googleapis.com/auth/plus.login",
               "https://www.googleapis.com/auth/plus.me",
               "https://www.googleapis.com/auth/plus.stream.read"]
        requestClient ActivitiesGet'{..}
          = go _agActivityId _agFields (Just AltJSON)
              plusDomainsService
          where go
                  = buildClient (Proxy :: Proxy ActivitiesGetResource)
                      mempty
