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
-- Module      : Network.Google.Resource.Compute.RegionCommitments.AggregatedList
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an aggregated list of commitments.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.regionCommitments.aggregatedList@.
module Network.Google.Resource.Compute.RegionCommitments.AggregatedList
    (
    -- * REST Resource
      RegionCommitmentsAggregatedListResource

    -- * Creating a Request
    , regionCommitmentsAggregatedList
    , RegionCommitmentsAggregatedList

    -- * Request Lenses
    , rcalOrderBy
    , rcalProject
    , rcalFilter
    , rcalPageToken
    , rcalMaxResults
    , rcalFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.regionCommitments.aggregatedList@ method which the
-- 'RegionCommitmentsAggregatedList' request conforms to.
type RegionCommitmentsAggregatedListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "aggregated" :>
               "commitments" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] CommitmentAggregatedList

-- | Retrieves an aggregated list of commitments.
--
-- /See:/ 'regionCommitmentsAggregatedList' smart constructor.
data RegionCommitmentsAggregatedList = RegionCommitmentsAggregatedList'
    { _rcalOrderBy :: !(Maybe Text)
    , _rcalProject :: !Text
    , _rcalFilter :: !(Maybe Text)
    , _rcalPageToken :: !(Maybe Text)
    , _rcalMaxResults :: !(Textual Word32)
    , _rcalFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegionCommitmentsAggregatedList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcalOrderBy'
--
-- * 'rcalProject'
--
-- * 'rcalFilter'
--
-- * 'rcalPageToken'
--
-- * 'rcalMaxResults'
--
-- * 'rcalFields'
regionCommitmentsAggregatedList
    :: Text -- ^ 'rcalProject'
    -> RegionCommitmentsAggregatedList
regionCommitmentsAggregatedList pRcalProject_ = 
    RegionCommitmentsAggregatedList'
    { _rcalOrderBy = Nothing
    , _rcalProject = pRcalProject_
    , _rcalFilter = Nothing
    , _rcalPageToken = Nothing
    , _rcalMaxResults = 500
    , _rcalFields = Nothing
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
rcalOrderBy :: Lens' RegionCommitmentsAggregatedList (Maybe Text)
rcalOrderBy
  = lens _rcalOrderBy (\ s a -> s{_rcalOrderBy = a})

-- | Project ID for this request.
rcalProject :: Lens' RegionCommitmentsAggregatedList Text
rcalProject
  = lens _rcalProject (\ s a -> s{_rcalProject = a})

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
rcalFilter :: Lens' RegionCommitmentsAggregatedList (Maybe Text)
rcalFilter
  = lens _rcalFilter (\ s a -> s{_rcalFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
rcalPageToken :: Lens' RegionCommitmentsAggregatedList (Maybe Text)
rcalPageToken
  = lens _rcalPageToken
      (\ s a -> s{_rcalPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
rcalMaxResults :: Lens' RegionCommitmentsAggregatedList Word32
rcalMaxResults
  = lens _rcalMaxResults
      (\ s a -> s{_rcalMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
rcalFields :: Lens' RegionCommitmentsAggregatedList (Maybe Text)
rcalFields
  = lens _rcalFields (\ s a -> s{_rcalFields = a})

instance GoogleRequest
         RegionCommitmentsAggregatedList where
        type Rs RegionCommitmentsAggregatedList =
             CommitmentAggregatedList
        type Scopes RegionCommitmentsAggregatedList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient RegionCommitmentsAggregatedList'{..}
          = go _rcalProject _rcalOrderBy _rcalFilter
              _rcalPageToken
              (Just _rcalMaxResults)
              _rcalFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy RegionCommitmentsAggregatedListResource)
                      mempty
