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
-- Module      : Network.Google.Resource.AppEngine.Apps.Firewall.IngressRules.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified firewall rule.
--
-- /See:/ <https://cloud.google.com/appengine/docs/admin-api/ Google App Engine Admin API Reference> for @appengine.apps.firewall.ingressRules.delete@.
module Network.Google.Resource.AppEngine.Apps.Firewall.IngressRules.Delete
    (
    -- * REST Resource
      AppsFirewallIngressRulesDeleteResource

    -- * Creating a Request
    , appsFirewallIngressRulesDelete
    , AppsFirewallIngressRulesDelete

    -- * Request Lenses
    , afirdXgafv
    , afirdUploadProtocol
    , afirdPp
    , afirdAccessToken
    , afirdUploadType
    , afirdBearerToken
    , afirdIngressRulesId
    , afirdAppsId
    , afirdFields
    , afirdCallback
    ) where

import Network.Google.AppEngine.Types
import Network.Google.Prelude

-- | A resource alias for @appengine.apps.firewall.ingressRules.delete@ method which the
-- 'AppsFirewallIngressRulesDelete' request conforms to.
type AppsFirewallIngressRulesDeleteResource =
     "v1" :>
       "apps" :>
         Capture "appsId" Text :>
           "firewall" :>
             "ingressRules" :>
               Capture "ingressRulesId" Text :>
                 QueryParam "$.xgafv" Xgafv :>
                   QueryParam "upload_protocol" Text :>
                     QueryParam "pp" Bool :>
                       QueryParam "access_token" Text :>
                         QueryParam "uploadType" Text :>
                           QueryParam "bearer_token" Text :>
                             QueryParam "callback" Text :>
                               QueryParam "fields" Text :>
                                 QueryParam "alt" AltJSON :>
                                   Delete '[JSON] Empty

-- | Deletes the specified firewall rule.
--
-- /See:/ 'appsFirewallIngressRulesDelete' smart constructor.
data AppsFirewallIngressRulesDelete = AppsFirewallIngressRulesDelete'
    { _afirdXgafv :: !(Maybe Xgafv)
    , _afirdUploadProtocol :: !(Maybe Text)
    , _afirdPp :: !Bool
    , _afirdAccessToken :: !(Maybe Text)
    , _afirdUploadType :: !(Maybe Text)
    , _afirdBearerToken :: !(Maybe Text)
    , _afirdIngressRulesId :: !Text
    , _afirdAppsId :: !Text
    , _afirdFields :: !(Maybe Text)
    , _afirdCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AppsFirewallIngressRulesDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afirdXgafv'
--
-- * 'afirdUploadProtocol'
--
-- * 'afirdPp'
--
-- * 'afirdAccessToken'
--
-- * 'afirdUploadType'
--
-- * 'afirdBearerToken'
--
-- * 'afirdIngressRulesId'
--
-- * 'afirdAppsId'
--
-- * 'afirdFields'
--
-- * 'afirdCallback'
appsFirewallIngressRulesDelete
    :: Text -- ^ 'afirdIngressRulesId'
    -> Text -- ^ 'afirdAppsId'
    -> AppsFirewallIngressRulesDelete
appsFirewallIngressRulesDelete pAfirdIngressRulesId_ pAfirdAppsId_ = 
    AppsFirewallIngressRulesDelete'
    { _afirdXgafv = Nothing
    , _afirdUploadProtocol = Nothing
    , _afirdPp = True
    , _afirdAccessToken = Nothing
    , _afirdUploadType = Nothing
    , _afirdBearerToken = Nothing
    , _afirdIngressRulesId = pAfirdIngressRulesId_
    , _afirdAppsId = pAfirdAppsId_
    , _afirdFields = Nothing
    , _afirdCallback = Nothing
    }

-- | V1 error format.
afirdXgafv :: Lens' AppsFirewallIngressRulesDelete (Maybe Xgafv)
afirdXgafv
  = lens _afirdXgafv (\ s a -> s{_afirdXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
afirdUploadProtocol :: Lens' AppsFirewallIngressRulesDelete (Maybe Text)
afirdUploadProtocol
  = lens _afirdUploadProtocol
      (\ s a -> s{_afirdUploadProtocol = a})

-- | Pretty-print response.
afirdPp :: Lens' AppsFirewallIngressRulesDelete Bool
afirdPp = lens _afirdPp (\ s a -> s{_afirdPp = a})

-- | OAuth access token.
afirdAccessToken :: Lens' AppsFirewallIngressRulesDelete (Maybe Text)
afirdAccessToken
  = lens _afirdAccessToken
      (\ s a -> s{_afirdAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
afirdUploadType :: Lens' AppsFirewallIngressRulesDelete (Maybe Text)
afirdUploadType
  = lens _afirdUploadType
      (\ s a -> s{_afirdUploadType = a})

-- | OAuth bearer token.
afirdBearerToken :: Lens' AppsFirewallIngressRulesDelete (Maybe Text)
afirdBearerToken
  = lens _afirdBearerToken
      (\ s a -> s{_afirdBearerToken = a})

-- | Part of \`name\`. See documentation of \`appsId\`.
afirdIngressRulesId :: Lens' AppsFirewallIngressRulesDelete Text
afirdIngressRulesId
  = lens _afirdIngressRulesId
      (\ s a -> s{_afirdIngressRulesId = a})

-- | Part of \`name\`. Name of the Firewall resource to delete. Example:
-- apps\/myapp\/firewall\/ingressRules\/100.
afirdAppsId :: Lens' AppsFirewallIngressRulesDelete Text
afirdAppsId
  = lens _afirdAppsId (\ s a -> s{_afirdAppsId = a})

-- | Selector specifying which fields to include in a partial response.
afirdFields :: Lens' AppsFirewallIngressRulesDelete (Maybe Text)
afirdFields
  = lens _afirdFields (\ s a -> s{_afirdFields = a})

-- | JSONP
afirdCallback :: Lens' AppsFirewallIngressRulesDelete (Maybe Text)
afirdCallback
  = lens _afirdCallback
      (\ s a -> s{_afirdCallback = a})

instance GoogleRequest AppsFirewallIngressRulesDelete
         where
        type Rs AppsFirewallIngressRulesDelete = Empty
        type Scopes AppsFirewallIngressRulesDelete =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient AppsFirewallIngressRulesDelete'{..}
          = go _afirdAppsId _afirdIngressRulesId _afirdXgafv
              _afirdUploadProtocol
              (Just _afirdPp)
              _afirdAccessToken
              _afirdUploadType
              _afirdBearerToken
              _afirdCallback
              _afirdFields
              (Just AltJSON)
              appEngineService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AppsFirewallIngressRulesDeleteResource)
                      mempty
