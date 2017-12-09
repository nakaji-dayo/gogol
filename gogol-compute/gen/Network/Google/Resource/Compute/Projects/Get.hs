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
-- Module      : Network.Google.Resource.Compute.Projects.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specified Project resource.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference> for @compute.projects.get@.
module Network.Google.Resource.Compute.Projects.Get
    (
    -- * REST Resource
      ProjectsGetResource

    -- * Creating a Request
    , projectsGet
    , ProjectsGet

    -- * Request Lenses
    , pgProject
    , pgFields
    ) where

import Network.Google.Compute.Types
import Network.Google.Prelude

-- | A resource alias for @compute.projects.get@ method which the
-- 'ProjectsGet' request conforms to.
type ProjectsGetResource =
     "compute" :>
       "v1" :>
         "projects" :>
           Capture "project" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] Project

-- | Returns the specified Project resource.
--
-- /See:/ 'projectsGet' smart constructor.
data ProjectsGet = ProjectsGet'
    { _pgProject :: !Text
    , _pgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgProject'
--
-- * 'pgFields'
projectsGet
    :: Text -- ^ 'pgProject'
    -> ProjectsGet
projectsGet pPgProject_ = 
    ProjectsGet'
    { _pgProject = pPgProject_
    , _pgFields = Nothing
    }

-- | Project ID for this request.
pgProject :: Lens' ProjectsGet Text
pgProject
  = lens _pgProject (\ s a -> s{_pgProject = a})

-- | Selector specifying which fields to include in a partial response.
pgFields :: Lens' ProjectsGet (Maybe Text)
pgFields = lens _pgFields (\ s a -> s{_pgFields = a})

instance GoogleRequest ProjectsGet where
        type Rs ProjectsGet = Project
        type Scopes ProjectsGet =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/compute",
               "https://www.googleapis.com/auth/compute.readonly"]
        requestClient ProjectsGet'{..}
          = go _pgProject _pgFields (Just AltJSON)
              computeService
          where go
                  = buildClient (Proxy :: Proxy ProjectsGetResource)
                      mempty
