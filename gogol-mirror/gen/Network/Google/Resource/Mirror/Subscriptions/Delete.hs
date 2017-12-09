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
-- Module      : Network.Google.Resource.Mirror.Subscriptions.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscription.
--
-- /See:/ <https://developers.google.com/glass Google Mirror API Reference> for @mirror.subscriptions.delete@.
module Network.Google.Resource.Mirror.Subscriptions.Delete
    (
    -- * REST Resource
      SubscriptionsDeleteResource

    -- * Creating a Request
    , subscriptionsDelete
    , SubscriptionsDelete

    -- * Request Lenses
    , sdId
    , sdFields
    ) where

import Network.Google.Mirror.Types
import Network.Google.Prelude

-- | A resource alias for @mirror.subscriptions.delete@ method which the
-- 'SubscriptionsDelete' request conforms to.
type SubscriptionsDeleteResource =
     "mirror" :>
       "v1" :>
         "subscriptions" :>
           Capture "id" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Deletes a subscription.
--
-- /See:/ 'subscriptionsDelete' smart constructor.
data SubscriptionsDelete = SubscriptionsDelete'
    { _sdId :: !Text
    , _sdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubscriptionsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdId'
--
-- * 'sdFields'
subscriptionsDelete
    :: Text -- ^ 'sdId'
    -> SubscriptionsDelete
subscriptionsDelete pSdId_ = 
    SubscriptionsDelete'
    { _sdId = pSdId_
    , _sdFields = Nothing
    }

-- | The ID of the subscription.
sdId :: Lens' SubscriptionsDelete Text
sdId = lens _sdId (\ s a -> s{_sdId = a})

-- | Selector specifying which fields to include in a partial response.
sdFields :: Lens' SubscriptionsDelete (Maybe Text)
sdFields = lens _sdFields (\ s a -> s{_sdFields = a})

instance GoogleRequest SubscriptionsDelete where
        type Rs SubscriptionsDelete = ()
        type Scopes SubscriptionsDelete =
             '["https://www.googleapis.com/auth/glass.timeline"]
        requestClient SubscriptionsDelete'{..}
          = go _sdId _sdFields (Just AltJSON) mirrorService
          where go
                  = buildClient
                      (Proxy :: Proxy SubscriptionsDeleteResource)
                      mempty
