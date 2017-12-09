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
-- Module      : Network.Google.Resource.PlusDomains.Circles.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a circle\'s description. This method supports patch semantics.
--
-- /See:/ <https://developers.google.com/+/domains/ Google+ Domains API Reference> for @plusDomains.circles.patch@.
module Network.Google.Resource.PlusDomains.Circles.Patch
    (
    -- * REST Resource
      CirclesPatchResource

    -- * Creating a Request
    , circlesPatch
    , CirclesPatch

    -- * Request Lenses
    , cpPayload
    , cpCircleId
    , cpFields
    ) where

import Network.Google.PlusDomains.Types
import Network.Google.Prelude

-- | A resource alias for @plusDomains.circles.patch@ method which the
-- 'CirclesPatch' request conforms to.
type CirclesPatchResource =
     "plusDomains" :>
       "v1" :>
         "circles" :>
           Capture "circleId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] Circle :> Patch '[JSON] Circle

-- | Update a circle\'s description. This method supports patch semantics.
--
-- /See:/ 'circlesPatch' smart constructor.
data CirclesPatch = CirclesPatch'
    { _cpPayload :: !Circle
    , _cpCircleId :: !Text
    , _cpFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CirclesPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPayload'
--
-- * 'cpCircleId'
--
-- * 'cpFields'
circlesPatch
    :: Circle -- ^ 'cpPayload'
    -> Text -- ^ 'cpCircleId'
    -> CirclesPatch
circlesPatch pCpPayload_ pCpCircleId_ = 
    CirclesPatch'
    { _cpPayload = pCpPayload_
    , _cpCircleId = pCpCircleId_
    , _cpFields = Nothing
    }

-- | Multipart request metadata.
cpPayload :: Lens' CirclesPatch Circle
cpPayload
  = lens _cpPayload (\ s a -> s{_cpPayload = a})

-- | The ID of the circle to update.
cpCircleId :: Lens' CirclesPatch Text
cpCircleId
  = lens _cpCircleId (\ s a -> s{_cpCircleId = a})

-- | Selector specifying which fields to include in a partial response.
cpFields :: Lens' CirclesPatch (Maybe Text)
cpFields = lens _cpFields (\ s a -> s{_cpFields = a})

instance GoogleRequest CirclesPatch where
        type Rs CirclesPatch = Circle
        type Scopes CirclesPatch =
             '["https://www.googleapis.com/auth/plus.circles.write",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient CirclesPatch'{..}
          = go _cpCircleId _cpFields (Just AltJSON) _cpPayload
              plusDomainsService
          where go
                  = buildClient (Proxy :: Proxy CirclesPatchResource)
                      mempty
