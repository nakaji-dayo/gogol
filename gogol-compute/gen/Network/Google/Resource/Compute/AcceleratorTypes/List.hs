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
-- Module      : Network.Google.Resource.Compute.AcceleratorTypes.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of accelerator types available to the specified
-- project.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.acceleratorTypes.list@.
module Network.Google.Resource.Compute.AcceleratorTypes.List
    (
    -- * REST Resource
      AcceleratorTypesListResource

    -- * Creating a Request
    , acceleratorTypesList
    , AcceleratorTypesList

    -- * Request Lenses
    , atlOrderBy
    , atlProject
    , atlZone
    , atlFilter
    , atlPageToken
    , atlMaxResults
    , atlFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.acceleratorTypes.list@ method which the
-- 'AcceleratorTypesList' request conforms to.
type AcceleratorTypesListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "zones" :>
               Capture "zone" Text :>
                 "acceleratorTypes" :>
                   QueryParam "orderBy" Text :>
                     QueryParam "filter" Text :>
                       QueryParam "pageToken" Text :>
                         QueryParam "maxResults" (Textual Word32) :>
                           QueryParam "fields" Text :>
                             QueryParam "alt" AltJSON :>
                               Get '[JSON] AcceleratorTypeList

-- | Retrieves a list of accelerator types available to the specified
-- project.
--
-- /See:/ 'acceleratorTypesList' smart constructor.
data AcceleratorTypesList = AcceleratorTypesList'
    { _atlOrderBy :: !(Maybe Text)
    , _atlProject :: !Text
    , _atlZone :: !Text
    , _atlFilter :: !(Maybe Text)
    , _atlPageToken :: !(Maybe Text)
    , _atlMaxResults :: !(Textual Word32)
    , _atlFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AcceleratorTypesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atlOrderBy'
--
-- * 'atlProject'
--
-- * 'atlZone'
--
-- * 'atlFilter'
--
-- * 'atlPageToken'
--
-- * 'atlMaxResults'
--
-- * 'atlFields'
acceleratorTypesList
    :: Text -- ^ 'atlProject'
    -> Text -- ^ 'atlZone'
    -> AcceleratorTypesList
acceleratorTypesList pAtlProject_ pAtlZone_ = 
    AcceleratorTypesList'
    { _atlOrderBy = Nothing
    , _atlProject = pAtlProject_
    , _atlZone = pAtlZone_
    , _atlFilter = Nothing
    , _atlPageToken = Nothing
    , _atlMaxResults = 500
    , _atlFields = Nothing
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
atlOrderBy :: Lens' AcceleratorTypesList (Maybe Text)
atlOrderBy
  = lens _atlOrderBy (\ s a -> s{_atlOrderBy = a})

-- | Project ID for this request.
atlProject :: Lens' AcceleratorTypesList Text
atlProject
  = lens _atlProject (\ s a -> s{_atlProject = a})

-- | The name of the zone for this request.
atlZone :: Lens' AcceleratorTypesList Text
atlZone = lens _atlZone (\ s a -> s{_atlZone = a})

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
atlFilter :: Lens' AcceleratorTypesList (Maybe Text)
atlFilter
  = lens _atlFilter (\ s a -> s{_atlFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
atlPageToken :: Lens' AcceleratorTypesList (Maybe Text)
atlPageToken
  = lens _atlPageToken (\ s a -> s{_atlPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
atlMaxResults :: Lens' AcceleratorTypesList Word32
atlMaxResults
  = lens _atlMaxResults
      (\ s a -> s{_atlMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
atlFields :: Lens' AcceleratorTypesList (Maybe Text)
atlFields
  = lens _atlFields (\ s a -> s{_atlFields = a})

instance GoogleRequest AcceleratorTypesList where
        type Rs AcceleratorTypesList = AcceleratorTypeList
        type Scopes AcceleratorTypesList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient AcceleratorTypesList'{..}
          = go _atlProject _atlZone _atlOrderBy _atlFilter
              _atlPageToken
              (Just _atlMaxResults)
              _atlFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy AcceleratorTypesListResource)
                      mempty
