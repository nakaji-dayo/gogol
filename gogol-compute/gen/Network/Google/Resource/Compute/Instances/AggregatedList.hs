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
-- Module      : Network.Google.Resource.Compute.Instances.AggregatedList
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves aggregated list of instances.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.instances.aggregatedList@.
module Network.Google.Resource.Compute.Instances.AggregatedList
    (
    -- * REST Resource
      InstancesAggregatedListResource

    -- * Creating a Request
    , instancesAggregatedList
    , InstancesAggregatedList

    -- * Request Lenses
    , ialaOrderBy
    , ialaProject
    , ialaFilter
    , ialaPageToken
    , ialaMaxResults
    , ialaFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.instances.aggregatedList@ method which the
-- 'InstancesAggregatedList' request conforms to.
type InstancesAggregatedListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "aggregated" :>
               "instances" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] InstanceAggregatedList

-- | Retrieves aggregated list of instances.
--
-- /See:/ 'instancesAggregatedList' smart constructor.
data InstancesAggregatedList = InstancesAggregatedList'
    { _ialaOrderBy :: !(Maybe Text)
    , _ialaProject :: !Text
    , _ialaFilter :: !(Maybe Text)
    , _ialaPageToken :: !(Maybe Text)
    , _ialaMaxResults :: !(Textual Word32)
    , _ialaFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstancesAggregatedList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ialaOrderBy'
--
-- * 'ialaProject'
--
-- * 'ialaFilter'
--
-- * 'ialaPageToken'
--
-- * 'ialaMaxResults'
--
-- * 'ialaFields'
instancesAggregatedList
    :: Text -- ^ 'ialaProject'
    -> InstancesAggregatedList
instancesAggregatedList pIalaProject_ = 
    InstancesAggregatedList'
    { _ialaOrderBy = Nothing
    , _ialaProject = pIalaProject_
    , _ialaFilter = Nothing
    , _ialaPageToken = Nothing
    , _ialaMaxResults = 500
    , _ialaFields = Nothing
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
ialaOrderBy :: Lens' InstancesAggregatedList (Maybe Text)
ialaOrderBy
  = lens _ialaOrderBy (\ s a -> s{_ialaOrderBy = a})

-- | Project ID for this request.
ialaProject :: Lens' InstancesAggregatedList Text
ialaProject
  = lens _ialaProject (\ s a -> s{_ialaProject = a})

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
ialaFilter :: Lens' InstancesAggregatedList (Maybe Text)
ialaFilter
  = lens _ialaFilter (\ s a -> s{_ialaFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
ialaPageToken :: Lens' InstancesAggregatedList (Maybe Text)
ialaPageToken
  = lens _ialaPageToken
      (\ s a -> s{_ialaPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
ialaMaxResults :: Lens' InstancesAggregatedList Word32
ialaMaxResults
  = lens _ialaMaxResults
      (\ s a -> s{_ialaMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
ialaFields :: Lens' InstancesAggregatedList (Maybe Text)
ialaFields
  = lens _ialaFields (\ s a -> s{_ialaFields = a})

instance GoogleRequest InstancesAggregatedList where
        type Rs InstancesAggregatedList =
             InstanceAggregatedList
        type Scopes InstancesAggregatedList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient InstancesAggregatedList'{..}
          = go _ialaProject _ialaOrderBy _ialaFilter
              _ialaPageToken
              (Just _ialaMaxResults)
              _ialaFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy InstancesAggregatedListResource)
                      mempty
