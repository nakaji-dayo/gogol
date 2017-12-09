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
-- Module      : Network.Google.Resource.Mirror.Subscriptions.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new subscription.
--
-- /See:/ <https://developers.google.com/glass Google Mirror API Reference> for @mirror.subscriptions.insert@.
module Network.Google.Resource.Mirror.Subscriptions.Insert
    (
    -- * REST Resource
      SubscriptionsInsertResource

    -- * Creating a Request
    , subscriptionsInsert
    , SubscriptionsInsert

    -- * Request Lenses
    , siPayload
    , siFields
    ) where

import Network.Google.Mirror.Types
import Network.Google.Prelude

-- | A resource alias for @mirror.subscriptions.insert@ method which the
-- 'SubscriptionsInsert' request conforms to.
type SubscriptionsInsertResource =
     "mirror" :>
       "v1" :>
         "subscriptions" :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :>
               ReqBody '[JSON] Subscription :>
                 Post '[JSON] Subscription

-- | Creates a new subscription.
--
-- /See:/ 'subscriptionsInsert' smart constructor.
data SubscriptionsInsert = SubscriptionsInsert'
    { _siPayload :: !Subscription
    , _siFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubscriptionsInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siPayload'
--
-- * 'siFields'
subscriptionsInsert
    :: Subscription -- ^ 'siPayload'
    -> SubscriptionsInsert
subscriptionsInsert pSiPayload_ = 
    SubscriptionsInsert'
    { _siPayload = pSiPayload_
    , _siFields = Nothing
    }

-- | Multipart request metadata.
siPayload :: Lens' SubscriptionsInsert Subscription
siPayload
  = lens _siPayload (\ s a -> s{_siPayload = a})

-- | Selector specifying which fields to include in a partial response.
siFields :: Lens' SubscriptionsInsert (Maybe Text)
siFields = lens _siFields (\ s a -> s{_siFields = a})

instance GoogleRequest SubscriptionsInsert where
        type Rs SubscriptionsInsert = Subscription
        type Scopes SubscriptionsInsert =
             '["https://www.googleapis.com/auth/glass.timeline"]
        requestClient SubscriptionsInsert'{..}
          = go _siFields (Just AltJSON) _siPayload
              mirrorService
          where go
                  = buildClient
                      (Proxy :: Proxy SubscriptionsInsertResource)
                      mempty
