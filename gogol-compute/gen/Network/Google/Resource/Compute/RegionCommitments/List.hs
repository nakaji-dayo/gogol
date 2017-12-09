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
-- Module      : Network.Google.Resource.Compute.RegionCommitments.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of commitments contained within the specified region.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.regionCommitments.list@.
module Network.Google.Resource.Compute.RegionCommitments.List
    (
    -- * REST Resource
      RegionCommitmentsListResource

    -- * Creating a Request
    , regionCommitmentsList
    , RegionCommitmentsList

    -- * Request Lenses
    , rclOrderBy
    , rclProject
    , rclFilter
    , rclRegion
    , rclPageToken
    , rclMaxResults
    , rclFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.regionCommitments.list@ method which the
-- 'RegionCommitmentsList' request conforms to.
type RegionCommitmentsListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "regions" :>
               Capture "region" Text :>
                 "commitments" :>
                   QueryParam "orderBy" Text :>
                     QueryParam "filter" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "maxResults" (Textual Word32) :>
                           QueryParam "fields" Text :>
                             QueryParam "alt" AltJSON :>
                               Get '[JSON] CommitmentList

-- | Retrieves a list of commitments contained within the specified region.
--
-- /See:/ 'regionCommitmentsList' smart constructor.
data RegionCommitmentsList = RegionCommitmentsList'
    { _rclOrderBy :: !(Maybe Text)
    , _rclProject :: !Text
    , _rclFilter :: !(Maybe Text)
    , _rclRegion :: !Text
    , _rclPageToken :: !(Maybe Text)
    , _rclMaxResults :: !(Textual Word32)
    , _rclFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegionCommitmentsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rclOrderBy'
--
-- * 'rclProject'
--
-- * 'rclFilter'
--
-- * 'rclRegion'
--
-- * 'rclPageToken'
--
-- * 'rclMaxResults'
--
-- * 'rclFields'
regionCommitmentsList
    :: Text -- ^ 'rclProject'
    -> Text -- ^ 'rclRegion'
    -> RegionCommitmentsList
regionCommitmentsList pRclProject_ pRclRegion_ = 
    RegionCommitmentsList'
    { _rclOrderBy = Nothing
    , _rclProject = pRclProject_
    , _rclFilter = Nothing
    , _rclRegion = pRclRegion_
    , _rclPageToken = Nothing
    , _rclMaxResults = 500
    , _rclFields = Nothing
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
rclOrderBy :: Lens' RegionCommitmentsList (Maybe Text)
rclOrderBy
  = lens _rclOrderBy (\ s a -> s{_rclOrderBy = a})

-- | Project ID for this request.
rclProject :: Lens' RegionCommitmentsList Text
rclProject
  = lens _rclProject (\ s a -> s{_rclProject = a})

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
rclFilter :: Lens' RegionCommitmentsList (Maybe Text)
rclFilter
  = lens _rclFilter (\ s a -> s{_rclFilter = a})

-- | Name of the region for this request.
rclRegion :: Lens' RegionCommitmentsList Text
rclRegion
  = lens _rclRegion (\ s a -> s{_rclRegion = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
rclPageToken :: Lens' RegionCommitmentsList (Maybe Text)
rclPageToken
  = lens _rclPageToken (\ s a -> s{_rclPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
rclMaxResults :: Lens' RegionCommitmentsList Word32
rclMaxResults
  = lens _rclMaxResults
      (\ s a -> s{_rclMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
rclFields :: Lens' RegionCommitmentsList (Maybe Text)
rclFields
  = lens _rclFields (\ s a -> s{_rclFields = a})

instance GoogleRequest RegionCommitmentsList where
        type Rs RegionCommitmentsList = CommitmentList
        type Scopes RegionCommitmentsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient RegionCommitmentsList'{..}
          = go _rclProject _rclRegion _rclOrderBy _rclFilter
              _rclPageToken
              (Just _rclMaxResults)
              _rclFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy RegionCommitmentsListResource)
                      mempty
