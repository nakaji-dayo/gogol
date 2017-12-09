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
-- Module      : Network.Google.Resource.Compute.Projects.GetXpnHost
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the shared VPC host project that this project links to. May be empty
-- if no link exists.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.projects.getXpnHost@.
module Network.Google.Resource.Compute.Projects.GetXpnHost
    (
    -- * REST Resource
      ProjectsGetXpnHostResource

    -- * Creating a Request
    , projectsGetXpnHost
    , ProjectsGetXpnHost

    -- * Request Lenses
    , pgxhProject
    , pgxhFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.projects.getXpnHost@ method which the
-- 'ProjectsGetXpnHost' request conforms to.
type ProjectsGetXpnHostResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             "getXpnHost" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] Project

-- | Get the shared VPC host project that this project links to. May be empty
-- if no link exists.
--
-- /See:/ 'projectsGetXpnHost' smart constructor.
data ProjectsGetXpnHost = ProjectsGetXpnHost'
    { _pgxhProject :: !Text
    , _pgxhFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsGetXpnHost' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgxhProject'
--
-- * 'pgxhFields'
projectsGetXpnHost
    :: Text -- ^ 'pgxhProject'
    -> ProjectsGetXpnHost
projectsGetXpnHost pPgxhProject_ = 
    ProjectsGetXpnHost'
    { _pgxhProject = pPgxhProject_
    , _pgxhFields = Nothing
    }

-- | Project ID for this request.
pgxhProject :: Lens' ProjectsGetXpnHost Text
pgxhProject
  = lens _pgxhProject (\ s a -> s{_pgxhProject = a})

-- | Selector specifying which fields to include in a partial response.
pgxhFields :: Lens' ProjectsGetXpnHost (Maybe Text)
pgxhFields
  = lens _pgxhFields (\ s a -> s{_pgxhFields = a})

instance GoogleRequest ProjectsGetXpnHost where
        type Rs ProjectsGetXpnHost = Project
        type Scopes ProjectsGetXpnHost =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute"]
        requestClient ProjectsGetXpnHost'{..}
          = go _pgxhProject _pgxhFields (Just AltJSON)
              computeService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsGetXpnHostResource)
                      mempty
