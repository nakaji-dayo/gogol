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
-- Module      : Network.Google.Resource.Mirror.Locations.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of locations for the user.
--
-- /See:/ <https://developers.google.com/glass Google Mirror API Reference> for @mirror.locations.list@.
module Network.Google.Resource.Mirror.Locations.List
    (
    -- * REST Resource
      LocationsListResource

    -- * Creating a Request
    , locationsList
    , LocationsList

    -- * Request Lenses
    , llFields
    ) where

import Network.Google.Mirror.Types
import Network.Google.Prelude

-- | A resource alias for @mirror.locations.list@ method which the
-- 'LocationsList' request conforms to.
type LocationsListResource =
     "mirror" :>
       "v1" :>
         "locations" :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :>
               Get '[JSON] LocationsListResponse

-- | Retrieves a list of locations for the user.
--
-- /See:/ 'locationsList' smart constructor.
newtype LocationsList = LocationsList'
    { _llFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LocationsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llFields'
locationsList
    :: LocationsList
locationsList = 
    LocationsList'
    { _llFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
llFields :: Lens' LocationsList (Maybe Text)
llFields = lens _llFields (\ s a -> s{_llFields = a})

instance GoogleRequest LocationsList where
        type Rs LocationsList = LocationsListResponse
        type Scopes LocationsList =
             '["https://www.googleapis.com/auth/glass.location",
               "https://www.googleapis.com/auth/glass.timeline"]
        requestClient LocationsList'{..}
          = go _llFields (Just AltJSON) mirrorService
          where go
                  = buildClient (Proxy :: Proxy LocationsListResource)
                      mempty
