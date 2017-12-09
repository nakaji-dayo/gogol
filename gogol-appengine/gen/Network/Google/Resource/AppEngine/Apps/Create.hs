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
-- Module      : Network.Google.Resource.AppEngine.Apps.Create
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an App Engine application for a Google Cloud Platform project.
-- Required fields: id - The ID of the target Cloud Platform project.
-- location - The region
-- (https:\/\/cloud.google.com\/appengine\/docs\/locations) where you want
-- the App Engine application located.For more information about App Engine
-- applications, see Managing Projects, Applications, and Billing
-- (https:\/\/cloud.google.com\/appengine\/docs\/python\/console\/).
--
-- /See:/ <https://cloud.google.com/appengine/docs/admin-api/ Google App Engine Admin API Reference> for @appengine.apps.create@.
module Network.Google.Resource.AppEngine.Apps.Create
    (
    -- * REST Resource
      AppsCreateResource

    -- * Creating a Request
    , appsCreate
    , AppsCreate

    -- * Request Lenses
    , acXgafv
    , acUploadProtocol
    , acPp
    , acAccessToken
    , acUploadType
    , acPayload
    , acBearerToken
    , acFields
    , acCallback
    ) where

import Network.Google.AppEngine.Types
import Network.Google.Prelude

-- | A resource alias for @appengine.apps.create@ method which the
-- 'AppsCreate' request conforms to.
type AppsCreateResource =
     "v1" :>
       "apps" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] Application :> Post '[JSON] Operation

-- | Creates an App Engine application for a Google Cloud Platform project.
-- Required fields: id - The ID of the target Cloud Platform project.
-- location - The region
-- (https:\/\/cloud.google.com\/appengine\/docs\/locations) where you want
-- the App Engine application located.For more information about App Engine
-- applications, see Managing Projects, Applications, and Billing
-- (https:\/\/cloud.google.com\/appengine\/docs\/python\/console\/).
--
-- /See:/ 'appsCreate' smart constructor.
data AppsCreate = AppsCreate'
    { _acXgafv :: !(Maybe Xgafv)
    , _acUploadProtocol :: !(Maybe Text)
    , _acPp :: !Bool
    , _acAccessToken :: !(Maybe Text)
    , _acUploadType :: !(Maybe Text)
    , _acPayload :: !Application
    , _acBearerToken :: !(Maybe Text)
    , _acFields :: !(Maybe Text)
    , _acCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AppsCreate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acXgafv'
--
-- * 'acUploadProtocol'
--
-- * 'acPp'
--
-- * 'acAccessToken'
--
-- * 'acUploadType'
--
-- * 'acPayload'
--
-- * 'acBearerToken'
--
-- * 'acFields'
--
-- * 'acCallback'
appsCreate
    :: Application -- ^ 'acPayload'
    -> AppsCreate
appsCreate pAcPayload_ = 
    AppsCreate'
    { _acXgafv = Nothing
    , _acUploadProtocol = Nothing
    , _acPp = True
    , _acAccessToken = Nothing
    , _acUploadType = Nothing
    , _acPayload = pAcPayload_
    , _acBearerToken = Nothing
    , _acFields = Nothing
    , _acCallback = Nothing
    }

-- | V1 error format.
acXgafv :: Lens' AppsCreate (Maybe Xgafv)
acXgafv = lens _acXgafv (\ s a -> s{_acXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
acUploadProtocol :: Lens' AppsCreate (Maybe Text)
acUploadProtocol
  = lens _acUploadProtocol
      (\ s a -> s{_acUploadProtocol = a})

-- | Pretty-print response.
acPp :: Lens' AppsCreate Bool
acPp = lens _acPp (\ s a -> s{_acPp = a})

-- | OAuth access token.
acAccessToken :: Lens' AppsCreate (Maybe Text)
acAccessToken
  = lens _acAccessToken
      (\ s a -> s{_acAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
acUploadType :: Lens' AppsCreate (Maybe Text)
acUploadType
  = lens _acUploadType (\ s a -> s{_acUploadType = a})

-- | Multipart request metadata.
acPayload :: Lens' AppsCreate Application
acPayload
  = lens _acPayload (\ s a -> s{_acPayload = a})

-- | OAuth bearer token.
acBearerToken :: Lens' AppsCreate (Maybe Text)
acBearerToken
  = lens _acBearerToken
      (\ s a -> s{_acBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
acFields :: Lens' AppsCreate (Maybe Text)
acFields = lens _acFields (\ s a -> s{_acFields = a})

-- | JSONP
acCallback :: Lens' AppsCreate (Maybe Text)
acCallback
  = lens _acCallback (\ s a -> s{_acCallback = a})

instance GoogleRequest AppsCreate where
        type Rs AppsCreate = Operation
        type Scopes AppsCreate =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient AppsCreate'{..}
          = go _acXgafv _acUploadProtocol (Just _acPp)
              _acAccessToken
              _acUploadType
              _acBearerToken
              _acCallback
              _acFields
              (Just AltJSON)
              _acPayload
              appEngineService
          where go
                  = buildClient (Proxy :: Proxy AppsCreateResource)
                      mempty
