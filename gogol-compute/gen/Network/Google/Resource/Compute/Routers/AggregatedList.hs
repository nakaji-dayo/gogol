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
-- Module      : Network.Google.Resource.Compute.Routers.AggregatedList
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an aggregated list of routers.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.routers.aggregatedList@.
module Network.Google.Resource.Compute.Routers.AggregatedList
    (
    -- * REST Resource
      RoutersAggregatedListResource

    -- * Creating a Request
    , routersAggregatedList
    , RoutersAggregatedList

    -- * Request Lenses
    , rOrderBy
    , rProject
    , rFilter
    , rPageToken
    , rMaxResults
    , rFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.routers.aggregatedList@ method which the
-- 'RoutersAggregatedList' request conforms to.
type RoutersAggregatedListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "aggregated" :>
               "routers" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] RouterAggregatedList

-- | Retrieves an aggregated list of routers.
--
-- /See:/ 'routersAggregatedList' smart constructor.
data RoutersAggregatedList = RoutersAggregatedList'
    { _rOrderBy :: !(Maybe Text)
    , _rProject :: !Text
    , _rFilter :: !(Maybe Text)
    , _rPageToken :: !(Maybe Text)
    , _rMaxResults :: !(Textual Word32)
    , _rFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RoutersAggregatedList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rOrderBy'
--
-- * 'rProject'
--
-- * 'rFilter'
--
-- * 'rPageToken'
--
-- * 'rMaxResults'
--
-- * 'rFields'
routersAggregatedList
    :: Text -- ^ 'rProject'
    -> RoutersAggregatedList
routersAggregatedList pRProject_ = 
    RoutersAggregatedList'
    { _rOrderBy = Nothing
    , _rProject = pRProject_
    , _rFilter = Nothing
    , _rPageToken = Nothing
    , _rMaxResults = 500
    , _rFields = Nothing
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
rOrderBy :: Lens' RoutersAggregatedList (Maybe Text)
rOrderBy = lens _rOrderBy (\ s a -> s{_rOrderBy = a})

-- | Project ID for this request.
rProject :: Lens' RoutersAggregatedList Text
rProject = lens _rProject (\ s a -> s{_rProject = a})

-- | Sets a filter {expression} for filtering listed resources. Your
-- {expression} must be in the format: field_name comparison_string
-- literal_string. The field_name is the name of the field you want to
-- compare. Only atomic field types are supported (string, number,
-- boolean). The comparison_string must be either eq (equals) or ne (not
-- equals). The literal_string is the string value to filter to. The
-- literal value must be valid for the type of field you are filtering by
-- (string, number, boolean). For string fields, the literal value is
-- interpreted as a regular expression using RE2 syntax. The literal value
-- must match the entire field. For example, to filter for instances that
-- do not have a name of example-instance, you would use name ne
-- example-instance. You can filter on nested fields. For example, you
-- could filter on instances that have set the scheduling.automaticRestart
-- field to true. Use filtering on nested fields to take advantage of
-- labels to organize and search for results based on label values. To
-- filter on multiple expressions, provide each separate expression within
-- parentheses. For example, (scheduling.automaticRestart eq true) (zone eq
-- us-central1-f). Multiple expressions are treated as AND expressions,
-- meaning that resources must match all expressions to pass the filters.
rFilter :: Lens' RoutersAggregatedList (Maybe Text)
rFilter = lens _rFilter (\ s a -> s{_rFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
rPageToken :: Lens' RoutersAggregatedList (Maybe Text)
rPageToken
  = lens _rPageToken (\ s a -> s{_rPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
rMaxResults :: Lens' RoutersAggregatedList Word32
rMaxResults
  = lens _rMaxResults (\ s a -> s{_rMaxResults = a}) .
      _Coerce

-- | Selector specifying which fields to include in a partial response.
rFields :: Lens' RoutersAggregatedList (Maybe Text)
rFields = lens _rFields (\ s a -> s{_rFields = a})

instance GoogleRequest RoutersAggregatedList where
        type Rs RoutersAggregatedList = RouterAggregatedList
        type Scopes RoutersAggregatedList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient RoutersAggregatedList'{..}
          = go _rProject _rOrderBy _rFilter _rPageToken
              (Just _rMaxResults)
              _rFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy RoutersAggregatedListResource)
                      mempty
