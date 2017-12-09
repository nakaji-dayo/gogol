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
-- Module      : Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an uptime check configuration. You can either replace the entire
-- configuration with a new one or replace only certain fields in the
-- current configuration by specifying the fields to be updated via
-- \"updateMask\". Returns the updated configuration.
--
-- /See:/ <https://cloud.google.com/monitoring/api/ Stackdriver Monitoring API Reference> for @monitoring.projects.uptimeCheckConfigs.patch@.
module Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Patch
    (
    -- * REST Resource
      ProjectsUptimeCheckConfigsPatchResource

    -- * Creating a Request
    , projectsUptimeCheckConfigsPatch
    , ProjectsUptimeCheckConfigsPatch

    -- * Request Lenses
    , puccpXgafv
    , puccpUploadProtocol
    , puccpUpdateMask
    , puccpPp
    , puccpAccessToken
    , puccpUploadType
    , puccpPayload
    , puccpBearerToken
    , puccpName
    , puccpFields
    , puccpCallback
    ) where

import Network.Google.Monitoring.Types
import Network.Google.Prelude

-- | A resource alias for @monitoring.projects.uptimeCheckConfigs.patch@ method which the
-- 'ProjectsUptimeCheckConfigsPatch' request conforms to.
type ProjectsUptimeCheckConfigsPatchResource =
     "v3" :>
       Capture "name" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "updateMask" FieldMask :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "bearer_token" Text :>
                       QueryParam "callback" Text :>
                         QueryParam "fields" Text :>
                           QueryParam "alt" AltJSON :>
                             ReqBody '[JSON] UptimeCheckConfig :>
                               Patch '[JSON] UptimeCheckConfig

-- | Updates an uptime check configuration. You can either replace the entire
-- configuration with a new one or replace only certain fields in the
-- current configuration by specifying the fields to be updated via
-- \"updateMask\". Returns the updated configuration.
--
-- /See:/ 'projectsUptimeCheckConfigsPatch' smart constructor.
data ProjectsUptimeCheckConfigsPatch = ProjectsUptimeCheckConfigsPatch'
    { _puccpXgafv :: !(Maybe Xgafv)
    , _puccpUploadProtocol :: !(Maybe Text)
    , _puccpUpdateMask :: !(Maybe FieldMask)
    , _puccpPp :: !Bool
    , _puccpAccessToken :: !(Maybe Text)
    , _puccpUploadType :: !(Maybe Text)
    , _puccpPayload :: !UptimeCheckConfig
    , _puccpBearerToken :: !(Maybe Text)
    , _puccpName :: !Text
    , _puccpFields :: !(Maybe Text)
    , _puccpCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsUptimeCheckConfigsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'puccpXgafv'
--
-- * 'puccpUploadProtocol'
--
-- * 'puccpUpdateMask'
--
-- * 'puccpPp'
--
-- * 'puccpAccessToken'
--
-- * 'puccpUploadType'
--
-- * 'puccpPayload'
--
-- * 'puccpBearerToken'
--
-- * 'puccpName'
--
-- * 'puccpFields'
--
-- * 'puccpCallback'
projectsUptimeCheckConfigsPatch
    :: UptimeCheckConfig -- ^ 'puccpPayload'
    -> Text -- ^ 'puccpName'
    -> ProjectsUptimeCheckConfigsPatch
projectsUptimeCheckConfigsPatch pPuccpPayload_ pPuccpName_ = 
    ProjectsUptimeCheckConfigsPatch'
    { _puccpXgafv = Nothing
    , _puccpUploadProtocol = Nothing
    , _puccpUpdateMask = Nothing
    , _puccpPp = True
    , _puccpAccessToken = Nothing
    , _puccpUploadType = Nothing
    , _puccpPayload = pPuccpPayload_
    , _puccpBearerToken = Nothing
    , _puccpName = pPuccpName_
    , _puccpFields = Nothing
    , _puccpCallback = Nothing
    }

-- | V1 error format.
puccpXgafv :: Lens' ProjectsUptimeCheckConfigsPatch (Maybe Xgafv)
puccpXgafv
  = lens _puccpXgafv (\ s a -> s{_puccpXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
puccpUploadProtocol :: Lens' ProjectsUptimeCheckConfigsPatch (Maybe Text)
puccpUploadProtocol
  = lens _puccpUploadProtocol
      (\ s a -> s{_puccpUploadProtocol = a})

-- | Optional. If present, only the listed fields in the current uptime check
-- configuration are updated with values from the new configuration. If
-- this field is empty, then the current configuration is completely
-- replaced with the new configuration.
puccpUpdateMask :: Lens' ProjectsUptimeCheckConfigsPatch (Maybe FieldMask)
puccpUpdateMask
  = lens _puccpUpdateMask
      (\ s a -> s{_puccpUpdateMask = a})

-- | Pretty-print response.
puccpPp :: Lens' ProjectsUptimeCheckConfigsPatch Bool
puccpPp = lens _puccpPp (\ s a -> s{_puccpPp = a})

-- | OAuth access token.
puccpAccessToken :: Lens' ProjectsUptimeCheckConfigsPatch (Maybe Text)
puccpAccessToken
  = lens _puccpAccessToken
      (\ s a -> s{_puccpAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
puccpUploadType :: Lens' ProjectsUptimeCheckConfigsPatch (Maybe Text)
puccpUploadType
  = lens _puccpUploadType
      (\ s a -> s{_puccpUploadType = a})

-- | Multipart request metadata.
puccpPayload :: Lens' ProjectsUptimeCheckConfigsPatch UptimeCheckConfig
puccpPayload
  = lens _puccpPayload (\ s a -> s{_puccpPayload = a})

-- | OAuth bearer token.
puccpBearerToken :: Lens' ProjectsUptimeCheckConfigsPatch (Maybe Text)
puccpBearerToken
  = lens _puccpBearerToken
      (\ s a -> s{_puccpBearerToken = a})

-- | A unique resource name for this UptimeCheckConfig. The format
-- is:projects\/[PROJECT_ID]\/uptimeCheckConfigs\/[UPTIME_CHECK_ID].This
-- field should be omitted when creating the uptime check configuration; on
-- create, the resource name is assigned by the server and included in the
-- response.
puccpName :: Lens' ProjectsUptimeCheckConfigsPatch Text
puccpName
  = lens _puccpName (\ s a -> s{_puccpName = a})

-- | Selector specifying which fields to include in a partial response.
puccpFields :: Lens' ProjectsUptimeCheckConfigsPatch (Maybe Text)
puccpFields
  = lens _puccpFields (\ s a -> s{_puccpFields = a})

-- | JSONP
puccpCallback :: Lens' ProjectsUptimeCheckConfigsPatch (Maybe Text)
puccpCallback
  = lens _puccpCallback
      (\ s a -> s{_puccpCallback = a})

instance GoogleRequest
         ProjectsUptimeCheckConfigsPatch where
        type Rs ProjectsUptimeCheckConfigsPatch =
             UptimeCheckConfig
        type Scopes ProjectsUptimeCheckConfigsPatch =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/monitoring"]
        requestClient ProjectsUptimeCheckConfigsPatch'{..}
          = go _puccpName _puccpXgafv _puccpUploadProtocol
              _puccpUpdateMask
              (Just _puccpPp)
              _puccpAccessToken
              _puccpUploadType
              _puccpBearerToken
              _puccpCallback
              _puccpFields
              (Just AltJSON)
              _puccpPayload
              monitoringService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ProjectsUptimeCheckConfigsPatchResource)
                      mempty
