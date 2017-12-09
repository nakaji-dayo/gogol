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
-- Module      : Network.Google.Resource.Compute.Routers.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Router resource.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.routers.delete@.
module Network.Google.Resource.Compute.Routers.Delete
    (
    -- * REST Resource
      RoutersDeleteResource

    -- * Creating a Request
    , routersDelete
    , RoutersDelete

    -- * Request Lenses
    , rddRequestId
    , rddProject
    , rddRouter
    , rddRegion
    , rddFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.routers.delete@ method which the
-- 'RoutersDelete' request conforms to.
type RoutersDeleteResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "regions" :>
               Capture "region" Text :>
                 "routers" :>
                   Capture "router" Text :>
                     QueryParam "requestId" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :> Delete '[JSON] Operation

-- | Deletes the specified Router resource.
--
-- /See:/ 'routersDelete' smart constructor.
data RoutersDelete = RoutersDelete'
    { _rddRequestId :: !(Maybe Text)
    , _rddProject :: !Text
    , _rddRouter :: !Text
    , _rddRegion :: !Text
    , _rddFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RoutersDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rddRequestId'
--
-- * 'rddProject'
--
-- * 'rddRouter'
--
-- * 'rddRegion'
--
-- * 'rddFields'
routersDelete
    :: Text -- ^ 'rddProject'
    -> Text -- ^ 'rddRouter'
    -> Text -- ^ 'rddRegion'
    -> RoutersDelete
routersDelete pRddProject_ pRddRouter_ pRddRegion_ = 
    RoutersDelete'
    { _rddRequestId = Nothing
    , _rddProject = pRddProject_
    , _rddRouter = pRddRouter_
    , _rddRegion = pRddRegion_
    , _rddFields = Nothing
    }

-- | An optional request ID to identify requests. Specify a unique request ID
-- so that if you must retry your request, the server will know to ignore
-- the request if it has already been completed. For example, consider a
-- situation where you make an initial request and the request times out.
-- If you make the request again with the same request ID, the server can
-- check if original operation with the same request ID was received, and
-- if so, will ignore the second request. This prevents clients from
-- accidentally creating duplicate commitments. The request ID must be a
-- valid UUID with the exception that zero UUID is not supported
-- (00000000-0000-0000-0000-000000000000).
rddRequestId :: Lens' RoutersDelete (Maybe Text)
rddRequestId
  = lens _rddRequestId (\ s a -> s{_rddRequestId = a})

-- | Project ID for this request.
rddProject :: Lens' RoutersDelete Text
rddProject
  = lens _rddProject (\ s a -> s{_rddProject = a})

-- | Name of the Router resource to delete.
rddRouter :: Lens' RoutersDelete Text
rddRouter
  = lens _rddRouter (\ s a -> s{_rddRouter = a})

-- | Name of the region for this request.
rddRegion :: Lens' RoutersDelete Text
rddRegion
  = lens _rddRegion (\ s a -> s{_rddRegion = a})

-- | Selector specifying which fields to include in a partial response.
rddFields :: Lens' RoutersDelete (Maybe Text)
rddFields
  = lens _rddFields (\ s a -> s{_rddFields = a})

instance GoogleRequest RoutersDelete where
        type Rs RoutersDelete = Operation
        type Scopes RoutersDelete =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute"]
        requestClient RoutersDelete'{..}
          = go _rddProject _rddRegion _rddRouter _rddRequestId
              _rddFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient (Proxy :: Proxy RoutersDeleteResource)
                      mempty
