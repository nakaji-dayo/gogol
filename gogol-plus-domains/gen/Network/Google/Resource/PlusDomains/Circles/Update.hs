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
-- Module      : Network.Google.Resource.PlusDomains.Circles.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a circle\'s description.
--
-- /See:/ <https://developers.google.com/+/domains/ Google+ Domains API Reference> for @plusDomains.circles.update@.
module Network.Google.Resource.PlusDomains.Circles.Update
    (
    -- * REST Resource
      CirclesUpdateResource

    -- * Creating a Request
    , circlesUpdate
    , CirclesUpdate

    -- * Request Lenses
    , cuPayload
    , cuCircleId
    , cuFields
    ) where

import Network.Google.PlusDomains.Types
import Network.Google.Prelude

-- | A resource alias for @plusDomains.circles.update@ method which the
-- 'CirclesUpdate' request conforms to.
type CirclesUpdateResource =
     "plusDomains" :>
       "v1" :>
         "circles" :>
           Capture "circleId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] Circle :> Put '[JSON] Circle

-- | Update a circle\'s description.
--
-- /See:/ 'circlesUpdate' smart constructor.
data CirclesUpdate = CirclesUpdate'
    { _cuPayload :: !Circle
    , _cuCircleId :: !Text
    , _cuFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CirclesUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuPayload'
--
-- * 'cuCircleId'
--
-- * 'cuFields'
circlesUpdate
    :: Circle -- ^ 'cuPayload'
    -> Text -- ^ 'cuCircleId'
    -> CirclesUpdate
circlesUpdate pCuPayload_ pCuCircleId_ = 
    CirclesUpdate'
    { _cuPayload = pCuPayload_
    , _cuCircleId = pCuCircleId_
    , _cuFields = Nothing
    }

-- | Multipart request metadata.
cuPayload :: Lens' CirclesUpdate Circle
cuPayload
  = lens _cuPayload (\ s a -> s{_cuPayload = a})

-- | The ID of the circle to update.
cuCircleId :: Lens' CirclesUpdate Text
cuCircleId
  = lens _cuCircleId (\ s a -> s{_cuCircleId = a})

-- | Selector specifying which fields to include in a partial response.
cuFields :: Lens' CirclesUpdate (Maybe Text)
cuFields = lens _cuFields (\ s a -> s{_cuFields = a})

instance GoogleRequest CirclesUpdate where
        type Rs CirclesUpdate = Circle
        type Scopes CirclesUpdate =
             '["https://www.googleapis.com/auth/plus.circles.write",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient CirclesUpdate'{..}
          = go _cuCircleId _cuFields (Just AltJSON) _cuPayload
              plusDomainsService
          where go
                  = buildClient (Proxy :: Proxy CirclesUpdateResource)
                      mempty
