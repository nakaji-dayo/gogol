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
-- Module      : Network.Google.Resource.Compute.BackendBuckets.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of BackendBucket resources available to the specified
-- project.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.backendBuckets.list@.
module Network.Google.Resource.Compute.BackendBuckets.List
    (
    -- * REST Resource
      BackendBucketsListResource

    -- * Creating a Request
    , backendBucketsList
    , BackendBucketsList

    -- * Request Lenses
    , bblOrderBy
    , bblProject
    , bblFilter
    , bblPageToken
    , bblMaxResults
    , bblFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.backendBuckets.list@ method which the
-- 'BackendBucketsList' request conforms to.
type BackendBucketsListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "global" :>
               "backendBuckets" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] BackendBucketList

-- | Retrieves the list of BackendBucket resources available to the specified
-- project.
--
-- /See:/ 'backendBucketsList' smart constructor.
data BackendBucketsList = BackendBucketsList'
    { _bblOrderBy :: !(Maybe Text)
    , _bblProject :: !Text
    , _bblFilter :: !(Maybe Text)
    , _bblPageToken :: !(Maybe Text)
    , _bblMaxResults :: !(Textual Word32)
    , _bblFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BackendBucketsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bblOrderBy'
--
-- * 'bblProject'
--
-- * 'bblFilter'
--
-- * 'bblPageToken'
--
-- * 'bblMaxResults'
--
-- * 'bblFields'
backendBucketsList
    :: Text -- ^ 'bblProject'
    -> BackendBucketsList
backendBucketsList pBblProject_ = 
    BackendBucketsList'
    { _bblOrderBy = Nothing
    , _bblProject = pBblProject_
    , _bblFilter = Nothing
    , _bblPageToken = Nothing
    , _bblMaxResults = 500
    , _bblFields = Nothing
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
bblOrderBy :: Lens' BackendBucketsList (Maybe Text)
bblOrderBy
  = lens _bblOrderBy (\ s a -> s{_bblOrderBy = a})

-- | Project ID for this request.
bblProject :: Lens' BackendBucketsList Text
bblProject
  = lens _bblProject (\ s a -> s{_bblProject = a})

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
bblFilter :: Lens' BackendBucketsList (Maybe Text)
bblFilter
  = lens _bblFilter (\ s a -> s{_bblFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
bblPageToken :: Lens' BackendBucketsList (Maybe Text)
bblPageToken
  = lens _bblPageToken (\ s a -> s{_bblPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
bblMaxResults :: Lens' BackendBucketsList Word32
bblMaxResults
  = lens _bblMaxResults
      (\ s a -> s{_bblMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
bblFields :: Lens' BackendBucketsList (Maybe Text)
bblFields
  = lens _bblFields (\ s a -> s{_bblFields = a})

instance GoogleRequest BackendBucketsList where
        type Rs BackendBucketsList = BackendBucketList
        type Scopes BackendBucketsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient BackendBucketsList'{..}
          = go _bblProject _bblOrderBy _bblFilter _bblPageToken
              (Just _bblMaxResults)
              _bblFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy BackendBucketsListResource)
                      mempty
