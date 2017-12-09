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
-- Module      : Network.Google.Resource.PlusDomains.Circles.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a circle.
--
-- /See:/ <https://developers.google.com/+/domains/ Google+ Domains API Reference> for @plusDomains.circles.get@.
module Network.Google.Resource.PlusDomains.Circles.Get
    (
    -- * REST Resource
      CirclesGetResource

    -- * Creating a Request
    , circlesGet
    , CirclesGet

    -- * Request Lenses
    , cgCircleId
    , cgFields
    ) where

import Network.Google.PlusDomains.Types
import Network.Google.Prelude

-- | A resource alias for @plusDomains.circles.get@ method which the
-- 'CirclesGet' request conforms to.
type CirclesGetResource =
     "plusDomains" :>
       "v1" :>
         "circles" :>
           Capture "circleId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] Circle

-- | Get a circle.
--
-- /See:/ 'circlesGet' smart constructor.
data CirclesGet = CirclesGet'
    { _cgCircleId :: !Text
    , _cgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CirclesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgCircleId'
--
-- * 'cgFields'
circlesGet
    :: Text -- ^ 'cgCircleId'
    -> CirclesGet
circlesGet pCgCircleId_ = 
    CirclesGet'
    { _cgCircleId = pCgCircleId_
    , _cgFields = Nothing
    }

-- | The ID of the circle to get.
cgCircleId :: Lens' CirclesGet Text
cgCircleId
  = lens _cgCircleId (\ s a -> s{_cgCircleId = a})

-- | Selector specifying which fields to include in a partial response.
cgFields :: Lens' CirclesGet (Maybe Text)
cgFields = lens _cgFields (\ s a -> s{_cgFields = a})

instance GoogleRequest CirclesGet where
        type Rs CirclesGet = Circle
        type Scopes CirclesGet =
             '["https://www.googleapis.com/auth/plus.circles.read",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient CirclesGet'{..}
          = go _cgCircleId _cgFields (Just AltJSON)
              plusDomainsService
          where go
                  = buildClient (Proxy :: Proxy CirclesGetResource)
                      mempty
