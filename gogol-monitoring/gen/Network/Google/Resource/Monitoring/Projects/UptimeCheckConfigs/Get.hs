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
-- Module      : Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a single uptime check configuration.
--
-- /See:/ <https://cloud.google.com/monitoring/api/ Stackdriver Monitoring API Reference> for @monitoring.projects.uptimeCheckConfigs.get@.
module Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Get
    (
    -- * REST Resource
      ProjectsUptimeCheckConfigsGetResource

    -- * Creating a Request
    , projectsUptimeCheckConfigsGet
    , ProjectsUptimeCheckConfigsGet

    -- * Request Lenses
    , puccgXgafv
    , puccgUploadProtocol
    , puccgPp
    , puccgAccessToken
    , puccgUploadType
    , puccgBearerToken
    , puccgName
    , puccgFields
    , puccgCallback
    ) where

import Network.Google.Monitoring.Types
import Network.Google.Prelude

-- | A resource alias for @monitoring.projects.uptimeCheckConfigs.get@ method which the
-- 'ProjectsUptimeCheckConfigsGet' request conforms to.
type ProjectsUptimeCheckConfigsGetResource =
     "v3" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           Get '[JSON] UptimeCheckConfig

-- | Gets a single uptime check configuration.
--
-- /See:/ 'projectsUptimeCheckConfigsGet' smart constructor.
data ProjectsUptimeCheckConfigsGet = ProjectsUptimeCheckConfigsGet'
    { _puccgXgafv :: !(Maybe Xgafv)
    , _puccgUploadProtocol :: !(Maybe Text)
    , _puccgPp :: !Bool
    , _puccgAccessToken :: !(Maybe Text)
    , _puccgUploadType :: !(Maybe Text)
    , _puccgBearerToken :: !(Maybe Text)
    , _puccgName :: !Text
    , _puccgFields :: !(Maybe Text)
    , _puccgCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsUptimeCheckConfigsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'puccgXgafv'
--
-- * 'puccgUploadProtocol'
--
-- * 'puccgPp'
--
-- * 'puccgAccessToken'
--
-- * 'puccgUploadType'
--
-- * 'puccgBearerToken'
--
-- * 'puccgName'
--
-- * 'puccgFields'
--
-- * 'puccgCallback'
projectsUptimeCheckConfigsGet
    :: Text -- ^ 'puccgName'
    -> ProjectsUptimeCheckConfigsGet
projectsUptimeCheckConfigsGet pPuccgName_ = 
    ProjectsUptimeCheckConfigsGet'
    { _puccgXgafv = Nothing
    , _puccgUploadProtocol = Nothing
    , _puccgPp = True
    , _puccgAccessToken = Nothing
    , _puccgUploadType = Nothing
    , _puccgBearerToken = Nothing
    , _puccgName = pPuccgName_
    , _puccgFields = Nothing
    , _puccgCallback = Nothing
    }

-- | V1 error format.
puccgXgafv :: Lens' ProjectsUptimeCheckConfigsGet (Maybe Xgafv)
puccgXgafv
  = lens _puccgXgafv (\ s a -> s{_puccgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
puccgUploadProtocol :: Lens' ProjectsUptimeCheckConfigsGet (Maybe Text)
puccgUploadProtocol
  = lens _puccgUploadProtocol
      (\ s a -> s{_puccgUploadProtocol = a})

-- | Pretty-print response.
puccgPp :: Lens' ProjectsUptimeCheckConfigsGet Bool
puccgPp = lens _puccgPp (\ s a -> s{_puccgPp = a})

-- | OAuth access token.
puccgAccessToken :: Lens' ProjectsUptimeCheckConfigsGet (Maybe Text)
puccgAccessToken
  = lens _puccgAccessToken
      (\ s a -> s{_puccgAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
puccgUploadType :: Lens' ProjectsUptimeCheckConfigsGet (Maybe Text)
puccgUploadType
  = lens _puccgUploadType
      (\ s a -> s{_puccgUploadType = a})

-- | OAuth bearer token.
puccgBearerToken :: Lens' ProjectsUptimeCheckConfigsGet (Maybe Text)
puccgBearerToken
  = lens _puccgBearerToken
      (\ s a -> s{_puccgBearerToken = a})

-- | The uptime check configuration to retrieve. The format
-- isprojects\/[PROJECT_ID]\/uptimeCheckConfigs\/[UPTIME_CHECK_ID].
puccgName :: Lens' ProjectsUptimeCheckConfigsGet Text
puccgName
  = lens _puccgName (\ s a -> s{_puccgName = a})

-- | Selector specifying which fields to include in a partial response.
puccgFields :: Lens' ProjectsUptimeCheckConfigsGet (Maybe Text)
puccgFields
  = lens _puccgFields (\ s a -> s{_puccgFields = a})

-- | JSONP
puccgCallback :: Lens' ProjectsUptimeCheckConfigsGet (Maybe Text)
puccgCallback
  = lens _puccgCallback
      (\ s a -> s{_puccgCallback = a})

instance GoogleRequest ProjectsUptimeCheckConfigsGet
         where
        type Rs ProjectsUptimeCheckConfigsGet =
             UptimeCheckConfig
        type Scopes ProjectsUptimeCheckConfigsGet =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/monitoring",
               "https://www.googleapis.com/auth/monitoring.read"]
        requestClient ProjectsUptimeCheckConfigsGet'{..}
          = go _puccgName _puccgXgafv _puccgUploadProtocol
              (Just _puccgPp)
              _puccgAccessToken
              _puccgUploadType
              _puccgBearerToken
              _puccgCallback
              _puccgFields
              (Just AltJSON)
              monitoringService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsUptimeCheckConfigsGetResource)
                      mempty
