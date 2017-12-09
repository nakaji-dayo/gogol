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
-- Module      : Network.Google.Resource.Compute.Subnetworks.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specified subnetwork. Get a list of available subnetworks
-- list() request.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.subnetworks.get@.
module Network.Google.Resource.Compute.Subnetworks.Get
    (
    -- * REST Resource
      SubnetworksGetResource

    -- * Creating a Request
    , subnetworksGet
    , SubnetworksGet

    -- * Request Lenses
    , sgProject
    , sgSubnetwork
    , sgRegion
    , sgFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.subnetworks.get@ method which the
-- 'SubnetworksGet' request conforms to.
type SubnetworksGetResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "regions" :>
               Capture "region" Text :>
                 "subnetworks" :>
                   Capture "subnetwork" Text :>
                     QueryParam "fields" Text :>
                       QueryParam "alt" AltJSON :> Get '[JSON] Subnetwork

-- | Returns the specified subnetwork. Get a list of available subnetworks
-- list() request.
--
-- /See:/ 'subnetworksGet' smart constructor.
data SubnetworksGet = SubnetworksGet'
    { _sgProject :: !Text
    , _sgSubnetwork :: !Text
    , _sgRegion :: !Text
    , _sgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SubnetworksGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgProject'
--
-- * 'sgSubnetwork'
--
-- * 'sgRegion'
--
-- * 'sgFields'
subnetworksGet
    :: Text -- ^ 'sgProject'
    -> Text -- ^ 'sgSubnetwork'
    -> Text -- ^ 'sgRegion'
    -> SubnetworksGet
subnetworksGet pSgProject_ pSgSubnetwork_ pSgRegion_ = 
    SubnetworksGet'
    { _sgProject = pSgProject_
    , _sgSubnetwork = pSgSubnetwork_
    , _sgRegion = pSgRegion_
    , _sgFields = Nothing
    }

-- | Project ID for this request.
sgProject :: Lens' SubnetworksGet Text
sgProject
  = lens _sgProject (\ s a -> s{_sgProject = a})

-- | Name of the Subnetwork resource to return.
sgSubnetwork :: Lens' SubnetworksGet Text
sgSubnetwork
  = lens _sgSubnetwork (\ s a -> s{_sgSubnetwork = a})

-- | Name of the region scoping this request.
sgRegion :: Lens' SubnetworksGet Text
sgRegion = lens _sgRegion (\ s a -> s{_sgRegion = a})

-- | Selector specifying which fields to include in a partial response.
sgFields :: Lens' SubnetworksGet (Maybe Text)
sgFields = lens _sgFields (\ s a -> s{_sgFields = a})

instance GoogleRequest SubnetworksGet where
        type Rs SubnetworksGet = Subnetwork
        type Scopes SubnetworksGet =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient SubnetworksGet'{..}
          = go _sgProject _sgRegion _sgSubnetwork _sgFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient (Proxy :: Proxy SubnetworksGetResource)
                      mempty
