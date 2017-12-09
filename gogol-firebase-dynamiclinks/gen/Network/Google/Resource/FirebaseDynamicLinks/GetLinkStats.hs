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
-- Module      : Network.Google.Resource.FirebaseDynamicLinks.GetLinkStats
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches analytics stats of a short Dynamic Link for a given duration.
-- Metrics include number of clicks, redirects, installs, app first opens,
-- and app reopens.
--
-- /See:/ <https://firebase.google.com/docs/dynamic-links/ Firebase Dynamic Links API Reference> for @firebasedynamiclinks.getLinkStats@.
module Network.Google.Resource.FirebaseDynamicLinks.GetLinkStats
    (
    -- * REST Resource
      GetLinkStatsResource

    -- * Creating a Request
    , getLinkStats
    , GetLinkStats

    -- * Request Lenses
    , glsXgafv
    , glsUploadProtocol
    , glsDynamicLink
    , glsPp
    , glsAccessToken
    , glsUploadType
    , glsDurationDays
    , glsBearerToken
    , glsFields
    , glsCallback
    ) where

import Network.Google.FirebaseDynamicLinks.Types
import Network.Google.Prelude

-- | A resource alias for @firebasedynamiclinks.getLinkStats@ method which the
-- 'GetLinkStats' request conforms to.
type GetLinkStatsResource =
     "v1" :>
       Capture "dynamicLink" Text :>
         "linkStats" :>
           QueryParam "$.xgafv" Xgafv :>
             QueryParam "upload_protocol" Text :>
               QueryParam "pp" Bool :>
                 QueryParam "access_token" Text :>
                   QueryParam "uploadType" Text :>
                     QueryParam "durationDays" (Textual Int64) :>
                       QueryParam "bearer_token" Text :>
                         QueryParam "callback" Text :>
                           QueryParam "fields" Text :>
                             QueryParam "alt" AltJSON :>
                               Get '[JSON] DynamicLinkStats

-- | Fetches analytics stats of a short Dynamic Link for a given duration.
-- Metrics include number of clicks, redirects, installs, app first opens,
-- and app reopens.
--
-- /See:/ 'getLinkStats' smart constructor.
data GetLinkStats = GetLinkStats'
    { _glsXgafv :: !(Maybe Xgafv)
    , _glsUploadProtocol :: !(Maybe Text)
    , _glsDynamicLink :: !Text
    , _glsPp :: !Bool
    , _glsAccessToken :: !(Maybe Text)
    , _glsUploadType :: !(Maybe Text)
    , _glsDurationDays :: !(Maybe (Textual Int64))
    , _glsBearerToken :: !(Maybe Text)
    , _glsFields :: !(Maybe Text)
    , _glsCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetLinkStats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glsXgafv'
--
-- * 'glsUploadProtocol'
--
-- * 'glsDynamicLink'
--
-- * 'glsPp'
--
-- * 'glsAccessToken'
--
-- * 'glsUploadType'
--
-- * 'glsDurationDays'
--
-- * 'glsBearerToken'
--
-- * 'glsFields'
--
-- * 'glsCallback'
getLinkStats
    :: Text -- ^ 'glsDynamicLink'
    -> GetLinkStats
getLinkStats pGlsDynamicLink_ = 
    GetLinkStats'
    { _glsXgafv = Nothing
    , _glsUploadProtocol = Nothing
    , _glsDynamicLink = pGlsDynamicLink_
    , _glsPp = True
    , _glsAccessToken = Nothing
    , _glsUploadType = Nothing
    , _glsDurationDays = Nothing
    , _glsBearerToken = Nothing
    , _glsFields = Nothing
    , _glsCallback = Nothing
    }

-- | V1 error format.
glsXgafv :: Lens' GetLinkStats (Maybe Xgafv)
glsXgafv = lens _glsXgafv (\ s a -> s{_glsXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
glsUploadProtocol :: Lens' GetLinkStats (Maybe Text)
glsUploadProtocol
  = lens _glsUploadProtocol
      (\ s a -> s{_glsUploadProtocol = a})

-- | Dynamic Link URL. e.g. https:\/\/abcd.app.goo.gl\/wxyz
glsDynamicLink :: Lens' GetLinkStats Text
glsDynamicLink
  = lens _glsDynamicLink
      (\ s a -> s{_glsDynamicLink = a})

-- | Pretty-print response.
glsPp :: Lens' GetLinkStats Bool
glsPp = lens _glsPp (\ s a -> s{_glsPp = a})

-- | OAuth access token.
glsAccessToken :: Lens' GetLinkStats (Maybe Text)
glsAccessToken
  = lens _glsAccessToken
      (\ s a -> s{_glsAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
glsUploadType :: Lens' GetLinkStats (Maybe Text)
glsUploadType
  = lens _glsUploadType
      (\ s a -> s{_glsUploadType = a})

-- | The span of time requested in days.
glsDurationDays :: Lens' GetLinkStats (Maybe Int64)
glsDurationDays
  = lens _glsDurationDays
      (\ s a -> s{_glsDurationDays = a})
      . mapping _Coerce

-- | OAuth bearer token.
glsBearerToken :: Lens' GetLinkStats (Maybe Text)
glsBearerToken
  = lens _glsBearerToken
      (\ s a -> s{_glsBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
glsFields :: Lens' GetLinkStats (Maybe Text)
glsFields
  = lens _glsFields (\ s a -> s{_glsFields = a})

-- | JSONP
glsCallback :: Lens' GetLinkStats (Maybe Text)
glsCallback
  = lens _glsCallback (\ s a -> s{_glsCallback = a})

instance GoogleRequest GetLinkStats where
        type Rs GetLinkStats = DynamicLinkStats
        type Scopes GetLinkStats =
             '["https://www.googleapis.com/auth/firebase"]
        requestClient GetLinkStats'{..}
          = go _glsDynamicLink _glsXgafv _glsUploadProtocol
              (Just _glsPp)
              _glsAccessToken
              _glsUploadType
              _glsDurationDays
              _glsBearerToken
              _glsCallback
              _glsFields
              (Just AltJSON)
              firebaseDynamicLinksService
          where go
                  = buildClient (Proxy :: Proxy GetLinkStatsResource)
                      mempty
