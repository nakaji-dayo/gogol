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
-- Module      : Network.Google.Resource.AppEngine.Apps.Firewall.IngressRules.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a firewall rule for the application.
--
-- /See:/ <https://cloud.google.com/appengine/docs/admin-api/ Google App Engine Admin API Reference> for @appengine.apps.firewall.ingressRules.create@.
module Network.Google.Resource.AppEngine.Apps.Firewall.IngressRules.Create
    (
    -- * REST Resource
      AppsFirewallIngressRulesCreateResource

    -- * Creating a Request
    , appsFirewallIngressRulesCreate
    , AppsFirewallIngressRulesCreate

    -- * Request Lenses
    , afircXgafv
    , afircUploadProtocol
    , afircPp
    , afircAccessToken
    , afircUploadType
    , afircPayload
    , afircBearerToken
    , afircAppsId
    , afircFields
    , afircCallback
    ) where

import Network.Google.AppEngine.Types
import Network.Google.Prelude

-- | A resource alias for @appengine.apps.firewall.ingressRules.create@ method which the
-- 'AppsFirewallIngressRulesCreate' request conforms to.
type AppsFirewallIngressRulesCreateResource =
     "v1" :>
       "apps" :>
         Capture "appsId" Text :>
           "firewall" :>
             "ingressRules" :>
               QueryParam "$.xgafv" Xgafv :>
                 QueryParam "upload_protocol" Text :>
                   QueryParam "pp" Bool :>
                     QueryParam "access_token" Text :>
                       QueryParam "uploadType" Text :>
                         QueryParam "bearer_token" Text :>
                           QueryParam "callback" Text :>
                             QueryParam "fields" Text :>
                               QueryParam "alt" AltJSON :>
                                 ReqBody '[JSON] FirewallRule :>
                                   Post '[JSON] FirewallRule

-- | Creates a firewall rule for the application.
--
-- /See:/ 'appsFirewallIngressRulesCreate' smart constructor.
data AppsFirewallIngressRulesCreate = AppsFirewallIngressRulesCreate'
    { _afircXgafv :: !(Maybe Xgafv)
    , _afircUploadProtocol :: !(Maybe Text)
    , _afircPp :: !Bool
    , _afircAccessToken :: !(Maybe Text)
    , _afircUploadType :: !(Maybe Text)
    , _afircPayload :: !FirewallRule
    , _afircBearerToken :: !(Maybe Text)
    , _afircAppsId :: !Text
    , _afircFields :: !(Maybe Text)
    , _afircCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AppsFirewallIngressRulesCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afircXgafv'
--
-- * 'afircUploadProtocol'
--
-- * 'afircPp'
--
-- * 'afircAccessToken'
--
-- * 'afircUploadType'
--
-- * 'afircPayload'
--
-- * 'afircBearerToken'
--
-- * 'afircAppsId'
--
-- * 'afircFields'
--
-- * 'afircCallback'
appsFirewallIngressRulesCreate
    :: FirewallRule -- ^ 'afircPayload'
    -> Text -- ^ 'afircAppsId'
    -> AppsFirewallIngressRulesCreate
appsFirewallIngressRulesCreate pAfircPayload_ pAfircAppsId_ = 
    AppsFirewallIngressRulesCreate'
    { _afircXgafv = Nothing
    , _afircUploadProtocol = Nothing
    , _afircPp = True
    , _afircAccessToken = Nothing
    , _afircUploadType = Nothing
    , _afircPayload = pAfircPayload_
    , _afircBearerToken = Nothing
    , _afircAppsId = pAfircAppsId_
    , _afircFields = Nothing
    , _afircCallback = Nothing
    }

-- | V1 error format.
afircXgafv :: Lens' AppsFirewallIngressRulesCreate (Maybe Xgafv)
afircXgafv
  = lens _afircXgafv (\ s a -> s{_afircXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
afircUploadProtocol :: Lens' AppsFirewallIngressRulesCreate (Maybe Text)
afircUploadProtocol
  = lens _afircUploadProtocol
      (\ s a -> s{_afircUploadProtocol = a})

-- | Pretty-print response.
afircPp :: Lens' AppsFirewallIngressRulesCreate Bool
afircPp = lens _afircPp (\ s a -> s{_afircPp = a})

-- | OAuth access token.
afircAccessToken :: Lens' AppsFirewallIngressRulesCreate (Maybe Text)
afircAccessToken
  = lens _afircAccessToken
      (\ s a -> s{_afircAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
afircUploadType :: Lens' AppsFirewallIngressRulesCreate (Maybe Text)
afircUploadType
  = lens _afircUploadType
      (\ s a -> s{_afircUploadType = a})

-- | Multipart request metadata.
afircPayload :: Lens' AppsFirewallIngressRulesCreate FirewallRule
afircPayload
  = lens _afircPayload (\ s a -> s{_afircPayload = a})

-- | OAuth bearer token.
afircBearerToken :: Lens' AppsFirewallIngressRulesCreate (Maybe Text)
afircBearerToken
  = lens _afircBearerToken
      (\ s a -> s{_afircBearerToken = a})

-- | Part of \`parent\`. Name of the parent Firewall collection in which to
-- create a new rule. Example: apps\/myapp\/firewall\/ingressRules.
afircAppsId :: Lens' AppsFirewallIngressRulesCreate Text
afircAppsId
  = lens _afircAppsId (\ s a -> s{_afircAppsId = a})

-- | Selector specifying which fields to include in a partial response.
afircFields :: Lens' AppsFirewallIngressRulesCreate (Maybe Text)
afircFields
  = lens _afircFields (\ s a -> s{_afircFields = a})

-- | JSONP
afircCallback :: Lens' AppsFirewallIngressRulesCreate (Maybe Text)
afircCallback
  = lens _afircCallback
      (\ s a -> s{_afircCallback = a})

instance GoogleRequest AppsFirewallIngressRulesCreate
         where
        type Rs AppsFirewallIngressRulesCreate = FirewallRule
        type Scopes AppsFirewallIngressRulesCreate =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient AppsFirewallIngressRulesCreate'{..}
          = go _afircAppsId _afircXgafv _afircUploadProtocol
              (Just _afircPp)
              _afircAccessToken
              _afircUploadType
              _afircBearerToken
              _afircCallback
              _afircFields
              (Just AltJSON)
              _afircPayload
              appEngineService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AppsFirewallIngressRulesCreateResource)
                      mempty
