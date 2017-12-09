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
-- Module      : Network.Google.Resource.PlusDomains.Circles.Remove
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a circle.
--
-- /See:/ <https://developers.google.com/+/domains/ Google+ Domains API Reference> for @plusDomains.circles.remove@.
module Network.Google.Resource.PlusDomains.Circles.Remove
    (
    -- * REST Resource
      CirclesRemoveResource

    -- * Creating a Request
    , circlesRemove
    , CirclesRemove

    -- * Request Lenses
    , crCircleId
    , crFields
    ) where

import Network.Google.PlusDomains.Types
import Network.Google.Prelude

-- | A resource alias for @plusDomains.circles.remove@ method which the
-- 'CirclesRemove' request conforms to.
type CirclesRemoveResource =
     "plusDomains" :>
       "v1" :>
         "circles" :>
           Capture "circleId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Delete a circle.
--
-- /See:/ 'circlesRemove' smart constructor.
data CirclesRemove = CirclesRemove'
    { _crCircleId :: !Text
    , _crFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CirclesRemove' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crCircleId'
--
-- * 'crFields'
circlesRemove
    :: Text -- ^ 'crCircleId'
    -> CirclesRemove
circlesRemove pCrCircleId_ = 
    CirclesRemove'
    { _crCircleId = pCrCircleId_
    , _crFields = Nothing
    }

-- | The ID of the circle to delete.
crCircleId :: Lens' CirclesRemove Text
crCircleId
  = lens _crCircleId (\ s a -> s{_crCircleId = a})

-- | Selector specifying which fields to include in a partial response.
crFields :: Lens' CirclesRemove (Maybe Text)
crFields = lens _crFields (\ s a -> s{_crFields = a})

instance GoogleRequest CirclesRemove where
        type Rs CirclesRemove = ()
        type Scopes CirclesRemove =
             '["https://www.googleapis.com/auth/plus.circles.write",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient CirclesRemove'{..}
          = go _crCircleId _crFields (Just AltJSON)
              plusDomainsService
          where go
                  = buildClient (Proxy :: Proxy CirclesRemoveResource)
                      mempty
