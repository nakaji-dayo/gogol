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
-- Module      : Network.Google.Resource.AppEngine.Apps.AuthorizedCertificates.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all SSL certificates the user is authorized to administer.
--
-- /See:/ <https://cloud.google.com/appengine/docs/admin-api/ Google App Engine Admin API Reference> for @appengine.apps.authorizedCertificates.list@.
module Network.Google.Resource.AppEngine.Apps.AuthorizedCertificates.List
    (
    -- * REST Resource
      AppsAuthorizedCertificatesListResource

    -- * Creating a Request
    , appsAuthorizedCertificatesList
    , AppsAuthorizedCertificatesList

    -- * Request Lenses
    , aaclXgafv
    , aaclUploadProtocol
    , aaclPp
    , aaclAccessToken
    , aaclUploadType
    , aaclBearerToken
    , aaclAppsId
    , aaclView
    , aaclPageToken
    , aaclPageSize
    , aaclFields
    , aaclCallback
    ) where

import Network.Google.AppEngine.Types
import Network.Google.Prelude

-- | A resource alias for @appengine.apps.authorizedCertificates.list@ method which the
-- 'AppsAuthorizedCertificatesList' request conforms to.
type AppsAuthorizedCertificatesListResource =
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
                         QueryParam "view" Text :>
                           QueryParam "pageToken" Text :>
                             QueryParam "pageSize" (Textual Int32) :>
                               QueryParam "callback" Text :>
                                 QueryParam "fields" Text :>
                                   QueryParam "alt" AltJSON :>
                                     Get '[JSON]
                                       ListAuthorizedCertificatesResponse

-- | Lists all SSL certificates the user is authorized to administer.
--
-- /See:/ 'appsAuthorizedCertificatesList' smart constructor.
data AppsAuthorizedCertificatesList = AppsAuthorizedCertificatesList'
    { _aaclXgafv :: !(Maybe Xgafv)
    , _aaclUploadProtocol :: !(Maybe Text)
    , _aaclPp :: !Bool
    , _aaclAccessToken :: !(Maybe Text)
    , _aaclUploadType :: !(Maybe Text)
    , _aaclBearerToken :: !(Maybe Text)
    , _aaclAppsId :: !Text
    , _aaclView :: !(Maybe Text)
    , _aaclPageToken :: !(Maybe Text)
    , _aaclPageSize :: !(Maybe (Textual Int32))
    , _aaclFields :: !(Maybe Text)
    , _aaclCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AppsAuthorizedCertificatesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaclXgafv'
--
-- * 'aaclUploadProtocol'
--
-- * 'aaclPp'
--
-- * 'aaclAccessToken'
--
-- * 'aaclUploadType'
--
-- * 'aaclBearerToken'
--
-- * 'aaclAppsId'
--
-- * 'aaclView'
--
-- * 'aaclPageToken'
--
-- * 'aaclPageSize'
--
-- * 'aaclFields'
--
-- * 'aaclCallback'
appsAuthorizedCertificatesList
    :: Text -- ^ 'aaclAppsId'
    -> AppsAuthorizedCertificatesList
appsAuthorizedCertificatesList pAaclAppsId_ = 
    AppsAuthorizedCertificatesList'
    { _aaclXgafv = Nothing
    , _aaclUploadProtocol = Nothing
    , _aaclPp = True
    , _aaclAccessToken = Nothing
    , _aaclUploadType = Nothing
    , _aaclBearerToken = Nothing
    , _aaclAppsId = pAaclAppsId_
    , _aaclView = Nothing
    , _aaclPageToken = Nothing
    , _aaclPageSize = Nothing
    , _aaclFields = Nothing
    , _aaclCallback = Nothing
    }

-- | V1 error format.
aaclXgafv :: Lens' AppsAuthorizedCertificatesList (Maybe Xgafv)
aaclXgafv
  = lens _aaclXgafv (\ s a -> s{_aaclXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
aaclUploadProtocol :: Lens' AppsAuthorizedCertificatesList (Maybe Text)
aaclUploadProtocol
  = lens _aaclUploadProtocol
      (\ s a -> s{_aaclUploadProtocol = a})

-- | Pretty-print response.
aaclPp :: Lens' AppsAuthorizedCertificatesList Bool
aaclPp = lens _aaclPp (\ s a -> s{_aaclPp = a})

-- | OAuth access token.
aaclAccessToken :: Lens' AppsAuthorizedCertificatesList (Maybe Text)
aaclAccessToken
  = lens _aaclAccessToken
      (\ s a -> s{_aaclAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
aaclUploadType :: Lens' AppsAuthorizedCertificatesList (Maybe Text)
aaclUploadType
  = lens _aaclUploadType
      (\ s a -> s{_aaclUploadType = a})

-- | OAuth bearer token.
aaclBearerToken :: Lens' AppsAuthorizedCertificatesList (Maybe Text)
aaclBearerToken
  = lens _aaclBearerToken
      (\ s a -> s{_aaclBearerToken = a})

-- | Part of \`parent\`. Name of the parent Application resource. Example:
-- apps\/myapp.
aaclAppsId :: Lens' AppsAuthorizedCertificatesList Text
aaclAppsId
  = lens _aaclAppsId (\ s a -> s{_aaclAppsId = a})

-- | Controls the set of fields returned in the LIST response.
aaclView :: Lens' AppsAuthorizedCertificatesList (Maybe Text)
aaclView = lens _aaclView (\ s a -> s{_aaclView = a})

-- | Continuation token for fetching the next page of results.
aaclPageToken :: Lens' AppsAuthorizedCertificatesList (Maybe Text)
aaclPageToken
  = lens _aaclPageToken
      (\ s a -> s{_aaclPageToken = a})

-- | Maximum results to return per page.
aaclPageSize :: Lens' AppsAuthorizedCertificatesList (Maybe Int32)
aaclPageSize
  = lens _aaclPageSize (\ s a -> s{_aaclPageSize = a})
      . mapping _Coerce

-- | Selector specifying which fields to include in a partial response.
aaclFields :: Lens' AppsAuthorizedCertificatesList (Maybe Text)
aaclFields
  = lens _aaclFields (\ s a -> s{_aaclFields = a})

-- | JSONP
aaclCallback :: Lens' AppsAuthorizedCertificatesList (Maybe Text)
aaclCallback
  = lens _aaclCallback (\ s a -> s{_aaclCallback = a})

instance GoogleRequest AppsAuthorizedCertificatesList
         where
        type Rs AppsAuthorizedCertificatesList =
             ListAuthorizedCertificatesResponse
        type Scopes AppsAuthorizedCertificatesList =
             '["https://www.googleapis.com/auth/appengine.admin",
               "https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/cloud-platform.read-only"]
        requestClient AppsAuthorizedCertificatesList'{..}
          = go _aaclAppsId _aaclXgafv _aaclUploadProtocol
              (Just _aaclPp)
              _aaclAccessToken
              _aaclUploadType
              _aaclBearerToken
              _aaclView
              _aaclPageToken
              _aaclPageSize
              _aaclCallback
              _aaclFields
              (Just AltJSON)
              appEngineService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy AppsAuthorizedCertificatesListResource)
                      mempty
