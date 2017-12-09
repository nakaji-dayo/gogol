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
-- Module      : Network.Google.Resource.Compute.Projects.GetXpnResources
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get service resources (a.k.a service project) associated with this host
-- project.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.projects.getXpnResources@.
module Network.Google.Resource.Compute.Projects.GetXpnResources
    (
    -- * REST Resource
      ProjectsGetXpnResourcesResource

    -- * Creating a Request
    , projectsGetXpnResources'
    , ProjectsGetXpnResources'

    -- * Request Lenses
    , pgxrProject
    , pgxrOrderBy
    , pgxrFilter
    , pgxrPageToken
    , pgxrMaxResults
    , pgxrFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.projects.getXpnResources@ method which the
-- 'ProjectsGetXpnResources'' request conforms to.
type ProjectsGetXpnResourcesResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "getXpnResources" :>
               QueryParam "order_by" Text :>
                 QueryParam "filter" Text :>
                   QueryParam "pageToken" Text :>
                     QueryParam "maxResults" (Textual Word32) :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON] ProjectsGetXpnResources

-- | Get service resources (a.k.a service project) associated with this host
-- project.
--
-- /See:/ 'projectsGetXpnResources'' smart constructor.
data ProjectsGetXpnResources' = ProjectsGetXpnResources''
    { _pgxrProject :: !Text
    , _pgxrOrderBy :: !(Maybe Text)
    , _pgxrFilter :: !(Maybe Text)
    , _pgxrPageToken :: !(Maybe Text)
    , _pgxrMaxResults :: !(Textual Word32)
    , _pgxrFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsGetXpnResources'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgxrProject'
--
-- * 'pgxrOrderBy'
--
-- * 'pgxrFilter'
--
-- * 'pgxrPageToken'
--
-- * 'pgxrMaxResults'
--
-- * 'pgxrFields'
projectsGetXpnResources'
    :: Text -- ^ 'pgxrProject'
    -> ProjectsGetXpnResources'
projectsGetXpnResources' pPgxrProject_ = 
    ProjectsGetXpnResources''
    { _pgxrProject = pPgxrProject_
    , _pgxrOrderBy = Nothing
    , _pgxrFilter = Nothing
    , _pgxrPageToken = Nothing
    , _pgxrMaxResults = 500
    , _pgxrFields = Nothing
    }

-- | Project ID for this request.
pgxrProject :: Lens' ProjectsGetXpnResources' Text
pgxrProject
  = lens _pgxrProject (\ s a -> s{_pgxrProject = a})

pgxrOrderBy :: Lens' ProjectsGetXpnResources' (Maybe Text)
pgxrOrderBy
  = lens _pgxrOrderBy (\ s a -> s{_pgxrOrderBy = a})

pgxrFilter :: Lens' ProjectsGetXpnResources' (Maybe Text)
pgxrFilter
  = lens _pgxrFilter (\ s a -> s{_pgxrFilter = a})

pgxrPageToken :: Lens' ProjectsGetXpnResources' (Maybe Text)
pgxrPageToken
  = lens _pgxrPageToken
      (\ s a -> s{_pgxrPageToken = a})

pgxrMaxResults :: Lens' ProjectsGetXpnResources' Word32
pgxrMaxResults
  = lens _pgxrMaxResults
      (\ s a -> s{_pgxrMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
pgxrFields :: Lens' ProjectsGetXpnResources' (Maybe Text)
pgxrFields
  = lens _pgxrFields (\ s a -> s{_pgxrFields = a})

instance GoogleRequest ProjectsGetXpnResources' where
        type Rs ProjectsGetXpnResources' =
             ProjectsGetXpnResources
        type Scopes ProjectsGetXpnResources' =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute"]
        requestClient ProjectsGetXpnResources''{..}
          = go _pgxrProject _pgxrOrderBy _pgxrFilter
              _pgxrPageToken
              (Just _pgxrMaxResults)
              _pgxrFields
              (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsGetXpnResourcesResource)
                      mempty
