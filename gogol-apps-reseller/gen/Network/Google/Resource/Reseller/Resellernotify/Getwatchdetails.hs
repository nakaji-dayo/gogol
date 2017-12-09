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
-- Module      : Network.Google.Resource.Reseller.Resellernotify.Getwatchdetails
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the details of the watch corresponding to the reseller.
--
-- /See:/ <https://developers.google.com/google-apps/reseller/ Enterprise Apps Reseller API Reference> for @reseller.resellernotify.getwatchdetails@.
module Network.Google.Resource.Reseller.Resellernotify.Getwatchdetails
    (
    -- * REST Resource
      ResellernotifyGetwatchdetailsResource

    -- * Creating a Request
    , resellernotifyGetwatchdetails
    , ResellernotifyGetwatchdetails

    -- * Request Lenses
    , rgFields
    ) where

import Network.Google.AppsReseller.Types
import Network.Google.Prelude

-- | A resource alias for @reseller.resellernotify.getwatchdetails@ method which the
-- 'ResellernotifyGetwatchdetails' request conforms to.
type ResellernotifyGetwatchdetailsResource =
     "apps" :>
       "reseller" :>
         "v1" :>
           "resellernotify" :>
             "getwatchdetails" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   Get '[JSON] ResellernotifyGetwatchdetailsResponse

-- | Returns all the details of the watch corresponding to the reseller.
--
-- /See:/ 'resellernotifyGetwatchdetails' smart constructor.
newtype ResellernotifyGetwatchdetails = ResellernotifyGetwatchdetails'
    { _rgFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResellernotifyGetwatchdetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgFields'
resellernotifyGetwatchdetails
    :: ResellernotifyGetwatchdetails
resellernotifyGetwatchdetails = 
    ResellernotifyGetwatchdetails'
    { _rgFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
rgFields :: Lens' ResellernotifyGetwatchdetails (Maybe Text)
rgFields = lens _rgFields (\ s a -> s{_rgFields = a})

instance GoogleRequest ResellernotifyGetwatchdetails
         where
        type Rs ResellernotifyGetwatchdetails =
             ResellernotifyGetwatchdetailsResponse
        type Scopes ResellernotifyGetwatchdetails =
             '["https://www.googleapis.com/auth/apps.order",
               "https://www.googleapis.com/auth/apps.order.readonly"]
        requestClient ResellernotifyGetwatchdetails'{..}
          = go _rgFields (Just AltJSON) appsResellerService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ResellernotifyGetwatchdetailsResource)
                      mempty
