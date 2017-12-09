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
-- Module      : Network.Google.Resource.Compute.Interconnects.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of interconnect available to the specified project.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.interconnects.list@.
module Network.Google.Resource.Compute.Interconnects.List
    (
    -- * REST Resource
      InterconnectsListResource

    -- * Creating a Request
    , interconnectsList
    , InterconnectsList

    -- * Request Lenses
    , ilOrderBy
    , ilProject
    , ilFilter
    , ilPageToken
    , ilMaxResults
    , ilFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.interconnects.list@ method which the
-- 'InterconnectsList' request conforms to.
type InterconnectsListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "global" :>
               "interconnects" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] InterconnectList

-- | Retrieves the list of interconnect available to the specified project.
--
-- /See:/ 'interconnectsList' smart constructor.
data InterconnectsList = InterconnectsList'
    { _ilOrderBy :: !(Maybe Text)
    , _ilProject :: !Text
    , _ilFilter :: !(Maybe Text)
    , _ilPageToken :: !(Maybe Text)
    , _ilMaxResults :: !(Textual Word32)
    , _ilFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InterconnectsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilOrderBy'
--
-- * 'ilProject'
--
-- * 'ilFilter'
--
-- * 'ilPageToken'
--
-- * 'ilMaxResults'
--
-- * 'ilFields'
interconnectsList
    :: Text -- ^ 'ilProject'
    -> InterconnectsList
interconnectsList pIlProject_ = 
    InterconnectsList'
    { _ilOrderBy = Nothing
    , _ilProject = pIlProject_
    , _ilFilter = Nothing
    , _ilPageToken = Nothing
    , _ilMaxResults = 500
    , _ilFields = Nothing
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
ilOrderBy :: Lens' InterconnectsList (Maybe Text)
ilOrderBy
  = lens _ilOrderBy (\ s a -> s{_ilOrderBy = a})

-- | Project ID for this request.
ilProject :: Lens' InterconnectsList Text
ilProject
  = lens _ilProject (\ s a -> s{_ilProject = a})

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
ilFilter :: Lens' InterconnectsList (Maybe Text)
ilFilter = lens _ilFilter (\ s a -> s{_ilFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
ilPageToken :: Lens' InterconnectsList (Maybe Text)
ilPageToken
  = lens _ilPageToken (\ s a -> s{_ilPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
ilMaxResults :: Lens' InterconnectsList Word32
ilMaxResults
  = lens _ilMaxResults (\ s a -> s{_ilMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
ilFields :: Lens' InterconnectsList (Maybe Text)
ilFields = lens _ilFields (\ s a -> s{_ilFields = a})

instance GoogleRequest InterconnectsList where
        type Rs InterconnectsList = InterconnectList
        type Scopes InterconnectsList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient InterconnectsList'{..}
          = go _ilProject _ilOrderBy _ilFilter _ilPageToken
              (Just _ilMaxResults)
              _ilFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy InterconnectsListResource)
                      mempty
