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
-- Module      : Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an uptime check configuration. Note that this method will fail
-- if the uptime check configuration is referenced by an alert policy or
-- other dependent configs that would be rendered invalid by the deletion.
--
-- /See:/ <https://cloud.google.com/monitoring/api/ Stackdriver Monitoring API Reference> for @monitoring.projects.uptimeCheckConfigs.delete@.
module Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Delete
    (
    -- * REST Resource
      ProjectsUptimeCheckConfigsDeleteResource

    -- * Creating a Request
    , projectsUptimeCheckConfigsDelete
    , ProjectsUptimeCheckConfigsDelete

    -- * Request Lenses
    , puccdXgafv
    , puccdUploadProtocol
    , puccdPp
    , puccdAccessToken
    , puccdUploadType
    , puccdBearerToken
    , puccdName
    , puccdFields
    , puccdCallback
    ) where

import Network.Google.Monitoring.Types
import Network.Google.Prelude

-- | A resource alias for @monitoring.projects.uptimeCheckConfigs.delete@ method which the
-- 'ProjectsUptimeCheckConfigsDelete' request conforms to.
type ProjectsUptimeCheckConfigsDeleteResource =
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
                         QueryParam "alt" AltJSON :> Delete '[JSON] Empty

-- | Deletes an uptime check configuration. Note that this method will fail
-- if the uptime check configuration is referenced by an alert policy or
-- other dependent configs that would be rendered invalid by the deletion.
--
-- /See:/ 'projectsUptimeCheckConfigsDelete' smart constructor.
data ProjectsUptimeCheckConfigsDelete = ProjectsUptimeCheckConfigsDelete'
    { _puccdXgafv :: !(Maybe Xgafv)
    , _puccdUploadProtocol :: !(Maybe Text)
    , _puccdPp :: !Bool
    , _puccdAccessToken :: !(Maybe Text)
    , _puccdUploadType :: !(Maybe Text)
    , _puccdBearerToken :: !(Maybe Text)
    , _puccdName :: !Text
    , _puccdFields :: !(Maybe Text)
    , _puccdCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsUptimeCheckConfigsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'puccdXgafv'
--
-- * 'puccdUploadProtocol'
--
-- * 'puccdPp'
--
-- * 'puccdAccessToken'
--
-- * 'puccdUploadType'
--
-- * 'puccdBearerToken'
--
-- * 'puccdName'
--
-- * 'puccdFields'
--
-- * 'puccdCallback'
projectsUptimeCheckConfigsDelete
    :: Text -- ^ 'puccdName'
    -> ProjectsUptimeCheckConfigsDelete
projectsUptimeCheckConfigsDelete pPuccdName_ = 
    ProjectsUptimeCheckConfigsDelete'
    { _puccdXgafv = Nothing
    , _puccdUploadProtocol = Nothing
    , _puccdPp = True
    , _puccdAccessToken = Nothing
    , _puccdUploadType = Nothing
    , _puccdBearerToken = Nothing
    , _puccdName = pPuccdName_
    , _puccdFields = Nothing
    , _puccdCallback = Nothing
    }

-- | V1 error format.
puccdXgafv :: Lens' ProjectsUptimeCheckConfigsDelete (Maybe Xgafv)
puccdXgafv
  = lens _puccdXgafv (\ s a -> s{_puccdXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
puccdUploadProtocol :: Lens' ProjectsUptimeCheckConfigsDelete (Maybe Text)
puccdUploadProtocol
  = lens _puccdUploadProtocol
      (\ s a -> s{_puccdUploadProtocol = a})

-- | Pretty-print response.
puccdPp :: Lens' ProjectsUptimeCheckConfigsDelete Bool
puccdPp = lens _puccdPp (\ s a -> s{_puccdPp = a})

-- | OAuth access token.
puccdAccessToken :: Lens' ProjectsUptimeCheckConfigsDelete (Maybe Text)
puccdAccessToken
  = lens _puccdAccessToken
      (\ s a -> s{_puccdAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
puccdUploadType :: Lens' ProjectsUptimeCheckConfigsDelete (Maybe Text)
puccdUploadType
  = lens _puccdUploadType
      (\ s a -> s{_puccdUploadType = a})

-- | OAuth bearer token.
puccdBearerToken :: Lens' ProjectsUptimeCheckConfigsDelete (Maybe Text)
puccdBearerToken
  = lens _puccdBearerToken
      (\ s a -> s{_puccdBearerToken = a})

-- | The uptime check configuration to delete. The format
-- isprojects\/[PROJECT_ID]\/uptimeCheckConfigs\/[UPTIME_CHECK_ID].
puccdName :: Lens' ProjectsUptimeCheckConfigsDelete Text
puccdName
  = lens _puccdName (\ s a -> s{_puccdName = a})

-- | Selector specifying which fields to include in a partial response.
puccdFields :: Lens' ProjectsUptimeCheckConfigsDelete (Maybe Text)
puccdFields
  = lens _puccdFields (\ s a -> s{_puccdFields = a})

-- | JSONP
puccdCallback :: Lens' ProjectsUptimeCheckConfigsDelete (Maybe Text)
puccdCallback
  = lens _puccdCallback
      (\ s a -> s{_puccdCallback = a})

instance GoogleRequest
         ProjectsUptimeCheckConfigsDelete where
        type Rs ProjectsUptimeCheckConfigsDelete = Empty
        type Scopes ProjectsUptimeCheckConfigsDelete =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/monitoring"]
        requestClient ProjectsUptimeCheckConfigsDelete'{..}
          = go _puccdName _puccdXgafv _puccdUploadProtocol
              (Just _puccdPp)
              _puccdAccessToken
              _puccdUploadType
              _puccdBearerToken
              _puccdCallback
              _puccdFields
              (Just AltJSON)
              monitoringService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsUptimeCheckConfigsDeleteResource)
                      mempty
