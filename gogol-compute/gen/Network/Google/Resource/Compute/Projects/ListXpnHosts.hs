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
-- Module      : Network.Google.Resource.Compute.Projects.ListXpnHosts
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all shared VPC host projects visible to the user in an
-- organization.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.projects.listXpnHosts@.
module Network.Google.Resource.Compute.Projects.ListXpnHosts
    (
    -- * REST Resource
      ProjectsListXpnHostsResource

    -- * Creating a Request
    , projectsListXpnHosts
    , ProjectsListXpnHosts

    -- * Request Lenses
    , plxhProject
    , plxhPayload
    , plxhOrderBy
    , plxhFilter
    , plxhPageToken
    , plxhMaxResults
    , plxhFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.projects.listXpnHosts@ method which the
-- 'ProjectsListXpnHosts' request conforms to.
type ProjectsListXpnHostsResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "listXpnHosts" :>
               QueryParam "order_by" Text :>
                 QueryParam "filter" Text :>
                   QueryParam "pageToken" Text :>
                     QueryParam "maxResults" (Textual Word32) :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] ProjectsListXpnHostsRequest :>
                             Post '[JSON] XpnHostList

-- | List all shared VPC host projects visible to the user in an
-- organization.
--
-- /See:/ 'projectsListXpnHosts' smart constructor.
data ProjectsListXpnHosts = ProjectsListXpnHosts'
    { _plxhProject :: !Text
    , _plxhPayload :: !ProjectsListXpnHostsRequest
    , _plxhOrderBy :: !(Maybe Text)
    , _plxhFilter :: !(Maybe Text)
    , _plxhPageToken :: !(Maybe Text)
    , _plxhMaxResults :: !(Textual Word32)
    , _plxhFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsListXpnHosts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plxhProject'
--
-- * 'plxhPayload'
--
-- * 'plxhOrderBy'
--
-- * 'plxhFilter'
--
-- * 'plxhPageToken'
--
-- * 'plxhMaxResults'
--
-- * 'plxhFields'
projectsListXpnHosts
    :: Text -- ^ 'plxhProject'
    -> ProjectsListXpnHostsRequest -- ^ 'plxhPayload'
    -> ProjectsListXpnHosts
projectsListXpnHosts pPlxhProject_ pPlxhPayload_ = 
    ProjectsListXpnHosts'
    { _plxhProject = pPlxhProject_
    , _plxhPayload = pPlxhPayload_
    , _plxhOrderBy = Nothing
    , _plxhFilter = Nothing
    , _plxhPageToken = Nothing
    , _plxhMaxResults = 500
    , _plxhFields = Nothing
    }

-- | Project ID for this request.
plxhProject :: Lens' ProjectsListXpnHosts Text
plxhProject
  = lens _plxhProject (\ s a -> s{_plxhProject = a})

-- | Multipart request metadata.
plxhPayload :: Lens' ProjectsListXpnHosts ProjectsListXpnHostsRequest
plxhPayload
  = lens _plxhPayload (\ s a -> s{_plxhPayload = a})

plxhOrderBy :: Lens' ProjectsListXpnHosts (Maybe Text)
plxhOrderBy
  = lens _plxhOrderBy (\ s a -> s{_plxhOrderBy = a})

plxhFilter :: Lens' ProjectsListXpnHosts (Maybe Text)
plxhFilter
  = lens _plxhFilter (\ s a -> s{_plxhFilter = a})

plxhPageToken :: Lens' ProjectsListXpnHosts (Maybe Text)
plxhPageToken
  = lens _plxhPageToken
      (\ s a -> s{_plxhPageToken = a})

plxhMaxResults :: Lens' ProjectsListXpnHosts Word32
plxhMaxResults
  = lens _plxhMaxResults
      (\ s a -> s{_plxhMaxResults = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
plxhFields :: Lens' ProjectsListXpnHosts (Maybe Text)
plxhFields
  = lens _plxhFields (\ s a -> s{_plxhFields = a})

instance GoogleRequest ProjectsListXpnHosts where
        type Rs ProjectsListXpnHosts = XpnHostList
        type Scopes ProjectsListXpnHosts =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute"]
        requestClient ProjectsListXpnHosts'{..}
          = go _plxhProject _plxhOrderBy _plxhFilter
              _plxhPageToken
              (Just _plxhMaxResults)
              _plxhFields
              (Just AltJSON)
              _plxhPayload
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsListXpnHostsResource)
                      mempty
