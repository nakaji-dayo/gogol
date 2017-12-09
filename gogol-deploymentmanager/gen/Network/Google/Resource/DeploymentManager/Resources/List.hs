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
-- Module      : Network.Google.Resource.DeploymentManager.Resources.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all resources in a given deployment.
--
-- /See:/ <https://cloud.google.com/deployment-manager/ Google Cloud Deployment Manager API Reference> for @deploymentmanager.resources.list@.
module Network.Google.Resource.DeploymentManager.Resources.List
    (
    -- * REST Resource
      ResourcesListResource

    -- * Creating a Request
    , resourcesList
    , ResourcesList

    -- * Request Lenses
    , rlOrderBy
    , rlProject
    , rlFilter
    , rlPageToken
    , rlMaxResults
    , rlFields
    , rlDeployment
    ) where

import Network.Google.DeploymentManager.Types
import Network.Google.Prelude

-- | A resource alias for @deploymentmanager.resources.list@ method which the
-- 'ResourcesList' request conforms to.
type ResourcesListResource =
     "deploymentmanager" :>
       "v2" :>
         "projects" :>
           Capture "project" Text :>
             "global" :>
               "deployments" :>
                 Capture "deployment" Text :>
                   "resources" :>
                     QueryParam "orderBy" Text :>
                       QueryParam "filter" Text :>
                         QueryParam "pageToken" Text :>
                           QueryParam "maxResults" (Textual Word32) :>
                             QueryParam "fields" Text :>
                               QueryParam "alt" AltJSON :>
                                 Get '[JSON] ResourcesListResponse

-- | Lists all resources in a given deployment.
--
-- /See:/ 'resourcesList' smart constructor.
data ResourcesList = ResourcesList'
    { _rlOrderBy :: !(Maybe Text)
    , _rlProject :: !Text
    , _rlFilter :: !(Maybe Text)
    , _rlPageToken :: !(Maybe Text)
    , _rlMaxResults :: !(Textual Word32)
    , _rlFields :: !(Maybe Text)
    , _rlDeployment :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourcesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlOrderBy'
--
-- * 'rlProject'
--
-- * 'rlFilter'
--
-- * 'rlPageToken'
--
-- * 'rlMaxResults'
--
-- * 'rlFields'
--
-- * 'rlDeployment'
resourcesList
    :: Text -- ^ 'rlProject'
    -> Text -- ^ 'rlDeployment'
    -> ResourcesList
resourcesList pRlProject_ pRlDeployment_ = 
    ResourcesList'
    { _rlOrderBy = Nothing
    , _rlProject = pRlProject_
    , _rlFilter = Nothing
    , _rlPageToken = Nothing
    , _rlMaxResults = 500
    , _rlFields = Nothing
    , _rlDeployment = pRlDeployment_
    }

-- | Sorts list results by a certain order. By default, results are returned
-- in alphanumerical order based on the resource name. You can also sort
-- results in descending order based on the creation timestamp using
-- orderBy=\"creationTimestamp desc\". This sorts results based on the
-- creationTimestamp field in reverse chronological order (newest result
-- first). Use this to sort resources like operations so that the newest
-- operation is returned first. Currently, only sorting by name or
-- creationTimestamp desc is supported.
rlOrderBy :: Lens' ResourcesList (Maybe Text)
rlOrderBy
  = lens _rlOrderBy (\ s a -> s{_rlOrderBy = a})

-- | The project ID for this request.
rlProject :: Lens' ResourcesList Text
rlProject
  = lens _rlProject (\ s a -> s{_rlProject = a})

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
rlFilter :: Lens' ResourcesList (Maybe Text)
rlFilter = lens _rlFilter (\ s a -> s{_rlFilter = a})

-- | Specifies a page token to use. Set pageToken to the nextPageToken
-- returned by a previous list request to get the next page of results.
rlPageToken :: Lens' ResourcesList (Maybe Text)
rlPageToken
  = lens _rlPageToken (\ s a -> s{_rlPageToken = a})

-- | The maximum number of results per page that should be returned. If the
-- number of available results is larger than maxResults, Compute Engine
-- returns a nextPageToken that can be used to get the next page of results
-- in subsequent list requests. Acceptable values are 0 to 500, inclusive.
-- (Default: 500)
rlMaxResults :: Lens' ResourcesList Word32
rlMaxResults
  = lens _rlMaxResults (\ s a -> s{_rlMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
rlFields :: Lens' ResourcesList (Maybe Text)
rlFields = lens _rlFields (\ s a -> s{_rlFields = a})

-- | The name of the deployment for this request.
rlDeployment :: Lens' ResourcesList Text
rlDeployment
  = lens _rlDeployment (\ s a -> s{_rlDeployment = a})

instance GoogleRequest ResourcesList where
        type Rs ResourcesList = ResourcesListResponse
        type Scopes ResourcesList =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/cloud-platform.read-only",
               "https://www.googleapis.com/auth/ndev.cloudman",
               "https://www.googleapis.com/auth/ndev.cloudman.readonly"]
        requestClient ResourcesList'{..}
          = go _rlProject _rlDeployment _rlOrderBy _rlFilter
              _rlPageToken
              (Just _rlMaxResults)
              _rlFields
              (Just AltJSON)
              deploymentManagerService
          where go
                  = buildClient (Proxy :: Proxy ResourcesListResource)
                      mempty
