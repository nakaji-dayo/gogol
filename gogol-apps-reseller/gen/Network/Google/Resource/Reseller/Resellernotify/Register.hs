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
-- Module      : Network.Google.Resource.Reseller.Resellernotify.Register
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a Reseller for receiving notifications.
--
-- /See:/ <https://developers.google.com/google-apps/reseller/ Enterprise Apps Reseller API Reference> for @reseller.resellernotify.register@.
module Network.Google.Resource.Reseller.Resellernotify.Register
    (
    -- * REST Resource
      ResellernotifyRegisterResource

    -- * Creating a Request
    , resellernotifyRegister
    , ResellernotifyRegister

    -- * Request Lenses
    , rrServiceAccountEmailAddress
    , rrFields
    ) where

import Network.Google.AppsReseller.Types
import Network.Google.Prelude

-- | A resource alias for @reseller.resellernotify.register@ method which the
-- 'ResellernotifyRegister' request conforms to.
type ResellernotifyRegisterResource =
     "apps" :>
       "reseller" :>
         "v1" :>
           "resellernotify" :>
             "register" :>
               QueryParam "serviceAccountEmailAddress" Text :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Post '[JSON] ResellernotifyResource

-- | Registers a Reseller for receiving notifications.
--
-- /See:/ 'resellernotifyRegister' smart constructor.
data ResellernotifyRegister = ResellernotifyRegister'
    { _rrServiceAccountEmailAddress :: !(Maybe Text)
    , _rrFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResellernotifyRegister' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrServiceAccountEmailAddress'
--
-- * 'rrFields'
resellernotifyRegister
    :: ResellernotifyRegister
resellernotifyRegister = 
    ResellernotifyRegister'
    { _rrServiceAccountEmailAddress = Nothing
    , _rrFields = Nothing
    }

-- | The service account which will own the created Cloud-PubSub topic.
rrServiceAccountEmailAddress :: Lens' ResellernotifyRegister (Maybe Text)
rrServiceAccountEmailAddress
  = lens _rrServiceAccountEmailAddress
      (\ s a -> s{_rrServiceAccountEmailAddress = a})

-- | Selector specifying which fields to include in a partial response.
rrFields :: Lens' ResellernotifyRegister (Maybe Text)
rrFields = lens _rrFields (\ s a -> s{_rrFields = a})

instance GoogleRequest ResellernotifyRegister where
        type Rs ResellernotifyRegister =
             ResellernotifyResource
        type Scopes ResellernotifyRegister =
             '["https://www.googleapis.com/auth/apps.order"]
        requestClient ResellernotifyRegister'{..}
          = go _rrServiceAccountEmailAddress _rrFields
              (Just AltJSON)
              appsResellerService
          where go
                  = buildClient
                      (Proxy :: Proxy ResellernotifyRegisterResource)
                      mempty
