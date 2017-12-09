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
-- Module      : Network.Google.Resource.AppState.States.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the states keys, and optionally the state data.
--
-- /See:/ <https://developers.google.com/games/services/web/api/states Google App State API Reference> for @appstate.states.list@.
module Network.Google.Resource.AppState.States.List
    (
    -- * REST Resource
      StatesListResource

    -- * Creating a Request
    , statesList
    , StatesList

    -- * Request Lenses
    , slIncludeData
    , slFields
    ) where

import Network.Google.AppState.Types
import Network.Google.Prelude

-- | A resource alias for @appstate.states.list@ method which the
-- 'StatesList' request conforms to.
type StatesListResource =
     "appstate" :>
       "v1" :>
         "states" :>
           QueryParam "includeData" Bool :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] ListResponse

-- | Lists all the states keys, and optionally the state data.
--
-- /See:/ 'statesList' smart constructor.
data StatesList = StatesList'
    { _slIncludeData :: !Bool
    , _slFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StatesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slIncludeData'
--
-- * 'slFields'
statesList
    :: StatesList
statesList = 
    StatesList'
    { _slIncludeData = False
    , _slFields = Nothing
    }

-- | Whether to include the full data in addition to the version number
slIncludeData :: Lens' StatesList Bool
slIncludeData
  = lens _slIncludeData
      (\ s a -> s{_slIncludeData = a})

-- | Selector specifying which fields to include in a partial response.
slFields :: Lens' StatesList (Maybe Text)
slFields = lens _slFields (\ s a -> s{_slFields = a})

instance GoogleRequest StatesList where
        type Rs StatesList = ListResponse
        type Scopes StatesList =
             '["https://www.googleapis.com/auth/appstate"]
        requestClient StatesList'{..}
          = go (Just _slIncludeData) _slFields (Just AltJSON)
              appStateService
          where go
                  = buildClient (Proxy :: Proxy StatesListResource)
                      mempty
