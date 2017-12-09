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
-- Module      : Network.Google.Resource.AppState.States.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the data corresponding to the passed key. If the key does not
-- exist on the server, an HTTP 404 will be returned.
--
-- /See:/ <https://developers.google.com/games/services/web/api/states Google App State API Reference> for @appstate.states.get@.
module Network.Google.Resource.AppState.States.Get
    (
    -- * REST Resource
      StatesGetResource

    -- * Creating a Request
    , statesGet
    , StatesGet

    -- * Request Lenses
    , sgStateKey
    , sgFields
    ) where

import Network.Google.AppState.Types
import Network.Google.Prelude

-- | A resource alias for @appstate.states.get@ method which the
-- 'StatesGet' request conforms to.
type StatesGetResource =
     "appstate" :>
       "v1" :>
         "states" :>
           Capture "stateKey" (Textual Int32) :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] GetResponse

-- | Retrieves the data corresponding to the passed key. If the key does not
-- exist on the server, an HTTP 404 will be returned.
--
-- /See:/ 'statesGet' smart constructor.
data StatesGet = StatesGet'
    { _sgStateKey :: !(Textual Int32)
    , _sgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StatesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgStateKey'
--
-- * 'sgFields'
statesGet
    :: Int32 -- ^ 'sgStateKey'
    -> StatesGet
statesGet pSgStateKey_ = 
    StatesGet'
    { _sgStateKey = _Coerce # pSgStateKey_
    , _sgFields = Nothing
    }

-- | The key for the data to be retrieved.
sgStateKey :: Lens' StatesGet Int32
sgStateKey
  = lens _sgStateKey (\ s a -> s{_sgStateKey = a}) .
      _Coerce

-- | Selector specifying which fields to include in a partial response.
sgFields :: Lens' StatesGet (Maybe Text)
sgFields = lens _sgFields (\ s a -> s{_sgFields = a})

instance GoogleRequest StatesGet where
        type Rs StatesGet = GetResponse
        type Scopes StatesGet =
             '["https://www.googleapis.com/auth/appstate"]
        requestClient StatesGet'{..}
          = go _sgStateKey _sgFields (Just AltJSON)
              appStateService
          where go
                  = buildClient (Proxy :: Proxy StatesGetResource)
                      mempty
