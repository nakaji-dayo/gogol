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
-- Module      : Network.Google.Resource.AppEngine.Apps.AuthorizedCertificates.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads the specified SSL certificate.
--
-- /See:/ <https://cloud.google.com/appengine/docs/admin-api/ Google App Engine Admin API Reference> for @appengine.apps.authorizedCertificates.create@.
module Network.Google.Resource.AppEngine.Apps.AuthorizedCertificates.Create
    (
    -- * REST Resource
      AppsAuthorizedCertificatesCreateResource

    -- * Creating a Request
    , appsAuthorizedCertificatesCreate
    , AppsAuthorizedCertificatesCreate

    -- * Request Lenses
    , aaccXgafv
    , aaccUploadProtocol
    , aaccPp
    , aaccAccessToken
    , aaccUploadType
    , aaccPayload
    , aaccBearerToken
    , aaccAppsId
    , aaccFields
    , aaccCallback
    ) where

import Network.Google.AppEngine.Types
import Network.Google.Prelude

-- | A resource alias for @appengine.apps.authorizedCertificates.create@ method which the
-- 'AppsAuthorizedCertificatesCreate' request conforms to.
type AppsAuthorizedCertificatesCreateResource =
     "v1" :>
       "apps" :>
         Capture "appsId" Text :>
           "authorizedCertificates" :>
             QueryParam "$.xgafv" Xgafv :>
               QueryParam "upload_protocol" Text :>
                 QueryParam "pp" Bool :>
                   QueryParam "access_token" Text :>
                     QueryParam "uploadType" Text :>
                       QueryParam "bearer_token" Text :>
                         QueryParam "callback" Text :>
                           QueryParam "fields" Text :>
                             QueryParam "alt" AltJSON :>
                               ReqBody '[JSON] AuthorizedCertificate :>
                                 Post '[JSON] AuthorizedCertificate

-- | Uploads the specified SSL certificate.
--
-- /See:/ 'appsAuthorizedCertificatesCreate' smart constructor.
data AppsAuthorizedCertificatesCreate = AppsAuthorizedCertificatesCreate'
    { _aaccXgafv :: !(Maybe Xgafv)
    , _aaccUploadProtocol :: !(Maybe Text)
    , _aaccPp :: !Bool
    , _aaccAccessToken :: !(Maybe Text)
    , _aaccUploadType :: !(Maybe Text)
    , _aaccPayload :: !AuthorizedCertificate
    , _aaccBearerToken :: !(Maybe Text)
    , _aaccAppsId :: !Text
    , _aaccFields :: !(Maybe Text)
    , _aaccCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AppsAuthorizedCertificatesCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaccXgafv'
--
-- * 'aaccUploadProtocol'
--
-- * 'aaccPp'
--
-- * 'aaccAccessToken'
--
-- * 'aaccUploadType'
--
-- * 'aaccPayload'
--
-- * 'aaccBearerToken'
--
-- * 'aaccAppsId'
--
-- * 'aaccFields'
--
-- * 'aaccCallback'
appsAuthorizedCertificatesCreate
    :: AuthorizedCertificate -- ^ 'aaccPayload'
    -> Text -- ^ 'aaccAppsId'
    -> AppsAuthorizedCertificatesCreate
appsAuthorizedCertificatesCreate pAaccPayload_ pAaccAppsId_ = 
    AppsAuthorizedCertificatesCreate'
    { _aaccXgafv = Nothing
    , _aaccUploadProtocol = Nothing
    , _aaccPp = True
    , _aaccAccessToken = Nothing
    , _aaccUploadType = Nothing
    , _aaccPayload = pAaccPayload_
    , _aaccBearerToken = Nothing
    , _aaccAppsId = pAaccAppsId_
    , _aaccFields = Nothing
    , _aaccCallback = Nothing
    }

-- | V1 error format.
aaccXgafv :: Lens' AppsAuthorizedCertificatesCreate (Maybe Xgafv)
aaccXgafv
  = lens _aaccXgafv (\ s a -> s{_aaccXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aaccUploadProtocol :: Lens' AppsAuthorizedCertificatesCreate (Maybe Text)
aaccUploadProtocol
  = lens _aaccUploadProtocol
      (\ s a -> s{_aaccUploadProtocol = a})

-- | Pretty-print response.
aaccPp :: Lens' AppsAuthorizedCertificatesCreate Bool
aaccPp = lens _aaccPp (\ s a -> s{_aaccPp = a})

-- | OAuth access token.
aaccAccessToken :: Lens' AppsAuthorizedCertificatesCreate (Maybe Text)
aaccAccessToken
  = lens _aaccAccessToken
      (\ s a -> s{_aaccAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aaccUploadType :: Lens' AppsAuthorizedCertificatesCreate (Maybe Text)
aaccUploadType
  = lens _aaccUploadType
      (\ s a -> s{_aaccUploadType = a})

-- | Multipart request metadata.
aaccPayload :: Lens' AppsAuthorizedCertificatesCreate AuthorizedCertificate
aaccPayload
  = lens _aaccPayload (\ s a -> s{_aaccPayload = a})

-- | OAuth bearer token.
aaccBearerToken :: Lens' AppsAuthorizedCertificatesCreate (Maybe Text)
aaccBearerToken
  = lens _aaccBearerToken
      (\ s a -> s{_aaccBearerToken = a})

-- | Part of \`parent\`. Name of the parent Application resource. Example:
-- apps\/myapp.
aaccAppsId :: Lens' AppsAuthorizedCertificatesCreate Text
aaccAppsId
  = lens _aaccAppsId (\ s a -> s{_aaccAppsId = a})

-- | Selector specifying which fields to include in a partial response.
aaccFields :: Lens' AppsAuthorizedCertificatesCreate (Maybe Text)
aaccFields
  = lens _aaccFields (\ s a -> s{_aaccFields = a})

-- | JSONP
aaccCallback :: Lens' AppsAuthorizedCertificatesCreate (Maybe Text)
aaccCallback
  = lens _aaccCallback (\ s a -> s{_aaccCallback = a})

instance GoogleRequest
         AppsAuthorizedCertificatesCreate where
        type Rs AppsAuthorizedCertificatesCreate =
             AuthorizedCertificate
        type Scopes AppsAuthorizedCertificatesCreate =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient AppsAuthorizedCertificatesCreate'{..}
          = go _aaccAppsId _aaccXgafv _aaccUploadProtocol
              (Just _aaccPp)
              _aaccAccessToken
              _aaccUploadType
              _aaccBearerToken
              _aaccCallback
              _aaccFields
              (Just AltJSON)
              _aaccPayload
              appEngineService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AppsAuthorizedCertificatesCreateResource)
                      mempty
