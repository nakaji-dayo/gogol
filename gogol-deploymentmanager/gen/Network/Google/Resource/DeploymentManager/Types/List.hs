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
-- Module      : Network.Google.Resource.DeploymentManager.Types.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all resource types for Deployment Manager.
--
-- /See:/ <https://cloud.google.com/deployment-manager/ Google Cloud Deployment Manager API Reference> for @deploymentmanager.types.list@.
module Network.Google.Resource.DeploymentManager.Types.List
    (
    -- * REST Resource
      TypesListResource

    -- * Creating a Request
    , typesList
    , TypesList

    -- * Request Lenses
    , tlOrderBy
    , tlProject
    , tlFilter
    , tlPageToken
    , tlMaxResults
    , tlFields
    ) where

import Network.Google.DeploymentManager.Types
import Network.Google.Prelude

-- | A resource alias for @deploymentmanager.types.list@ method which the
-- 'TypesList' request conforms to.
type TypesListResource =
     "deploymentmanager" :>
       "v2" :>
         "projects" :>
           Capture "project" Text :>
             "global" :>
               "types" :>
                 QueryParam "orderBy" Text :>
                   QueryParam "filter" Text :>
                     QueryParam "pageToken" Text :>
                       QueryParam "maxResults" (Textual Word32) :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             Get '[JSON] TypesListResponse

-- | Lists all resource types for Deployment Manager.
--
-- /See:/ 'typesList' smart constructor.
data TypesList = TypesList'
    { _tlOrderBy :: !(Maybe Text)
    , _tlProject :: !Text
    , _tlFilter :: !(Maybe Text)
    , _tlPageToken :: !(Maybe Text)
    , _tlMaxResults :: !(Textual Word32)
    , _tlFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TypesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlOrderBy'
--
-- * 'tlProject'
--
-- * 'tlFilter'
--
-- * 'tlPageToken'
--
-- * 'tlMaxResults'
--
-- * 'tlFields'
typesList
    :: Text -- ^ 'tlProject'
    -> TypesList
typesList pTlProject_ = 
    TypesList'
    { _tlOrderBy = Nothing
    , _tlProject = pTlProject_
    , _tlFilter = Nothing
    , _tlPageToken = Nothing
    , _tlMaxResults = 500
    , _tlFields = Nothing
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
tlOrderBy :: Lens' TypesList (Maybe Text)
tlOrderBy
  = lens _tlOrderBy (\ s a -> s{_tlOrderBy = a})

-- | The project ID for this request.
tlProject :: Lens' TypesList Text
tlProject
  = lens _tlProject (\ s a -> s{_tlProject = a})

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
tlFilter :: Lens' TypesList (Maybe Text)
tlFilter = lens _tlFilter (\ s a -> s{_tlFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
tlPageToken :: Lens' TypesList (Maybe Text)
tlPageToken
  = lens _tlPageToken (\ s a -> s{_tlPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
tlMaxResults :: Lens' TypesList Word32
tlMaxResults
  = lens _tlMaxResults (\ s a -> s{_tlMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
tlFields :: Lens' TypesList (Maybe Text)
tlFields = lens _tlFields (\ s a -> s{_tlFields = a})

instance GoogleRequest TypesList where
        type Rs TypesList = TypesListResponse
        type Scopes TypesList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/cloud-platform.read-only",
               "https://www.googleapis.com/auth/ndev.cloudman",
               "https://www.googleapis.com/auth/ndev.cloudman.readonly"]
        requestClient TypesList'{..}
          = go _tlProject _tlOrderBy _tlFilter _tlPageToken
              (Just _tlMaxResults)
              _tlFields
              (Just AltJSON)
              deploymentManagerService
          where go
                  = buildClient (Proxy :: Proxy TypesListResource)
                      mempty
