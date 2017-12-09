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
-- Module      : Network.Google.Resource.Compute.Images.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of custom images available to the specified project.
-- Custom images are images you create that belong to your project. This
-- method does not get any images that belong to other projects, including
-- publicly-available images, like Debian 8. If you want to get a list of
-- publicly-available images, use this method to make a request to the
-- respective image project, such as debian-cloud or windows-cloud.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.images.list@.
module Network.Google.Resource.Compute.Images.List
    (
    -- * REST Resource
      ImagesListResource

    -- * Creating a Request
    , imagesList
    , ImagesList

    -- * Request Lenses
    , imamOrderBy
    , imamProject
    , imamFilter
    , imamPageToken
    , imamMaxResults
    , imamFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.images.list@ method which the
-- 'ImagesList' request conforms to.
type ImagesListResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "global" :>
               "images" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :> Get '[JSON] ImageList

-- | Retrieves the list of custom images available to the specified project.
-- Custom images are images you create that belong to your project. This
-- method does not get any images that belong to other projects, including
-- publicly-available images, like Debian 8. If you want to get a list of
-- publicly-available images, use this method to make a request to the
-- respective image project, such as debian-cloud or windows-cloud.
--
-- /See:/ 'imagesList' smart constructor.
data ImagesList = ImagesList'
    { _imamOrderBy :: !(Maybe Text)
    , _imamProject :: !Text
    , _imamFilter :: !(Maybe Text)
    , _imamPageToken :: !(Maybe Text)
    , _imamMaxResults :: !(Textual Word32)
    , _imamFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImagesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imamOrderBy'
--
-- * 'imamProject'
--
-- * 'imamFilter'
--
-- * 'imamPageToken'
--
-- * 'imamMaxResults'
--
-- * 'imamFields'
imagesList
    :: Text -- ^ 'imamProject'
    -> ImagesList
imagesList pImamProject_ = 
    ImagesList'
    { _imamOrderBy = Nothing
    , _imamProject = pImamProject_
    , _imamFilter = Nothing
    , _imamPageToken = Nothing
    , _imamMaxResults = 500
    , _imamFields = Nothing
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
imamOrderBy :: Lens' ImagesList (Maybe Text)
imamOrderBy
  = lens _imamOrderBy (\ s a -> s{_imamOrderBy = a})

-- | Project ID for this request.
imamProject :: Lens' ImagesList Text
imamProject
  = lens _imamProject (\ s a -> s{_imamProject = a})

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
imamFilter :: Lens' ImagesList (Maybe Text)
imamFilter
  = lens _imamFilter (\ s a -> s{_imamFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
imamPageToken :: Lens' ImagesList (Maybe Text)
imamPageToken
  = lens _imamPageToken
      (\ s a -> s{_imamPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
imamMaxResults :: Lens' ImagesList Word32
imamMaxResults
  = lens _imamMaxResults
      (\ s a -> s{_imamMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
imamFields :: Lens' ImagesList (Maybe Text)
imamFields
  = lens _imamFields (\ s a -> s{_imamFields = a})

instance GoogleRequest ImagesList where
        type Rs ImagesList = ImageList
        type Scopes ImagesList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient ImagesList'{..}
          = go _imamProject _imamOrderBy _imamFilter
              _imamPageToken
              (Just _imamMaxResults)
              _imamFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient (Proxy :: Proxy ImagesListResource)
                      mempty
