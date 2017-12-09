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
-- Module      : Network.Google.Resource.Compute.InterconnectAttachments.AggregatedList
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an aggregated list of interconnect attachments.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.interconnectAttachments.aggregatedList@.
module Network.Google.Resource.Compute.InterconnectAttachments.AggregatedList
    (
    -- * REST Resource
      InterconnectAttachmentsAggregatedListResource

    -- * Creating a Request
    , interconnectAttachmentsAggregatedList
    , InterconnectAttachmentsAggregatedList

    -- * Request Lenses
    , iaalOrderBy
    , iaalProject
    , iaalFilter
    , iaalPageToken
    , iaalMaxResults
    , iaalFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.interconnectAttachments.aggregatedList@ method which the
-- 'InterconnectAttachmentsAggregatedList' request conforms to.
type InterconnectAttachmentsAggregatedListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "aggregated" :>
               "interconnectAttachments" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] InterconnectAttachmentAggregatedList

-- | Retrieves an aggregated list of interconnect attachments.
--
-- /See:/ 'interconnectAttachmentsAggregatedList' smart constructor.
data InterconnectAttachmentsAggregatedList = InterconnectAttachmentsAggregatedList'
    { _iaalOrderBy :: !(Maybe Text)
    , _iaalProject :: !Text
    , _iaalFilter :: !(Maybe Text)
    , _iaalPageToken :: !(Maybe Text)
    , _iaalMaxResults :: !(Textual Word32)
    , _iaalFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InterconnectAttachmentsAggregatedList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaalOrderBy'
--
-- * 'iaalProject'
--
-- * 'iaalFilter'
--
-- * 'iaalPageToken'
--
-- * 'iaalMaxResults'
--
-- * 'iaalFields'
interconnectAttachmentsAggregatedList
    :: Text -- ^ 'iaalProject'
    -> InterconnectAttachmentsAggregatedList
interconnectAttachmentsAggregatedList pIaalProject_ = 
    InterconnectAttachmentsAggregatedList'
    { _iaalOrderBy = Nothing
    , _iaalProject = pIaalProject_
    , _iaalFilter = Nothing
    , _iaalPageToken = Nothing
    , _iaalMaxResults = 500
    , _iaalFields = Nothing
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
iaalOrderBy :: Lens' InterconnectAttachmentsAggregatedList (Maybe Text)
iaalOrderBy
  = lens _iaalOrderBy (\ s a -> s{_iaalOrderBy = a})

-- | Project ID for this request.
iaalProject :: Lens' InterconnectAttachmentsAggregatedList Text
iaalProject
  = lens _iaalProject (\ s a -> s{_iaalProject = a})

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
iaalFilter :: Lens' InterconnectAttachmentsAggregatedList (Maybe Text)
iaalFilter
  = lens _iaalFilter (\ s a -> s{_iaalFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
iaalPageToken :: Lens' InterconnectAttachmentsAggregatedList (Maybe Text)
iaalPageToken
  = lens _iaalPageToken
      (\ s a -> s{_iaalPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
iaalMaxResults :: Lens' InterconnectAttachmentsAggregatedList Word32
iaalMaxResults
  = lens _iaalMaxResults
      (\ s a -> s{_iaalMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
iaalFields :: Lens' InterconnectAttachmentsAggregatedList (Maybe Text)
iaalFields
  = lens _iaalFields (\ s a -> s{_iaalFields = a})

instance GoogleRequest
         InterconnectAttachmentsAggregatedList where
        type Rs InterconnectAttachmentsAggregatedList =
             InterconnectAttachmentAggregatedList
        type Scopes InterconnectAttachmentsAggregatedList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient
          InterconnectAttachmentsAggregatedList'{..}
          = go _iaalProject _iaalOrderBy _iaalFilter
              _iaalPageToken
              (Just _iaalMaxResults)
              _iaalFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy InterconnectAttachmentsAggregatedListResource)
                      mempty
