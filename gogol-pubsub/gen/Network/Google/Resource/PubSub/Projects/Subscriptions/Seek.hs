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
-- Module      : Network.Google.Resource.PubSub.Projects.Subscriptions.Seek
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Seeks an existing subscription to a point in time or to a given
-- snapshot, whichever is provided in the request.
--
-- /See:/ <https://cloud.google.com/pubsub/docs Google Cloud Pub/Sub API Reference> for @pubsub.projects.subscriptions.seek@.
module Network.Google.Resource.PubSub.Projects.Subscriptions.Seek
    (
    -- * REST Resource
      ProjectsSubscriptionsSeekResource

    -- * Creating a Request
    , projectsSubscriptionsSeek
    , ProjectsSubscriptionsSeek

    -- * Request Lenses
    , pssXgafv
    , pssUploadProtocol
    , pssPp
    , pssAccessToken
    , pssUploadType
    , pssPayload
    , pssBearerToken
    , pssSubscription
    , pssFields
    , pssCallback
    ) where

import Network.Google.Prelude
import Network.Google.PubSub.Types

-- | A resource alias for @pubsub.projects.subscriptions.seek@ method which the
-- 'ProjectsSubscriptionsSeek' request conforms to.
type ProjectsSubscriptionsSeekResource =
     "v1" :>
       CaptureMode "subscription" "seek" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] SeekRequest :>
                             Post '[JSON] SeekResponse

-- | Seeks an existing subscription to a point in time or to a given
-- snapshot, whichever is provided in the request.
--
-- /See:/ 'projectsSubscriptionsSeek' smart constructor.
data ProjectsSubscriptionsSeek = ProjectsSubscriptionsSeek'
    { _pssXgafv :: !(Maybe Xgafv)
    , _pssUploadProtocol :: !(Maybe Text)
    , _pssPp :: !Bool
    , _pssAccessToken :: !(Maybe Text)
    , _pssUploadType :: !(Maybe Text)
    , _pssPayload :: !SeekRequest
    , _pssBearerToken :: !(Maybe Text)
    , _pssSubscription :: !Text
    , _pssFields :: !(Maybe Text)
    , _pssCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsSubscriptionsSeek' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pssXgafv'
--
-- * 'pssUploadProtocol'
--
-- * 'pssPp'
--
-- * 'pssAccessToken'
--
-- * 'pssUploadType'
--
-- * 'pssPayload'
--
-- * 'pssBearerToken'
--
-- * 'pssSubscription'
--
-- * 'pssFields'
--
-- * 'pssCallback'
projectsSubscriptionsSeek
    :: SeekRequest -- ^ 'pssPayload'
    -> Text -- ^ 'pssSubscription'
    -> ProjectsSubscriptionsSeek
projectsSubscriptionsSeek pPssPayload_ pPssSubscription_ = 
    ProjectsSubscriptionsSeek'
    { _pssXgafv = Nothing
    , _pssUploadProtocol = Nothing
    , _pssPp = True
    , _pssAccessToken = Nothing
    , _pssUploadType = Nothing
    , _pssPayload = pPssPayload_
    , _pssBearerToken = Nothing
    , _pssSubscription = pPssSubscription_
    , _pssFields = Nothing
    , _pssCallback = Nothing
    }

-- | V1 error format.
pssXgafv :: Lens' ProjectsSubscriptionsSeek (Maybe Xgafv)
pssXgafv = lens _pssXgafv (\ s a -> s{_pssXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pssUploadProtocol :: Lens' ProjectsSubscriptionsSeek (Maybe Text)
pssUploadProtocol
  = lens _pssUploadProtocol
      (\ s a -> s{_pssUploadProtocol = a})

-- | Pretty-print response.
pssPp :: Lens' ProjectsSubscriptionsSeek Bool
pssPp = lens _pssPp (\ s a -> s{_pssPp = a})

-- | OAuth access token.
pssAccessToken :: Lens' ProjectsSubscriptionsSeek (Maybe Text)
pssAccessToken
  = lens _pssAccessToken
      (\ s a -> s{_pssAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pssUploadType :: Lens' ProjectsSubscriptionsSeek (Maybe Text)
pssUploadType
  = lens _pssUploadType
      (\ s a -> s{_pssUploadType = a})

-- | Multipart request metadata.
pssPayload :: Lens' ProjectsSubscriptionsSeek SeekRequest
pssPayload
  = lens _pssPayload (\ s a -> s{_pssPayload = a})

-- | OAuth bearer token.
pssBearerToken :: Lens' ProjectsSubscriptionsSeek (Maybe Text)
pssBearerToken
  = lens _pssBearerToken
      (\ s a -> s{_pssBearerToken = a})

-- | The subscription to affect.
pssSubscription :: Lens' ProjectsSubscriptionsSeek Text
pssSubscription
  = lens _pssSubscription
      (\ s a -> s{_pssSubscription = a})

-- | Selector specifying which fields to include in a partial response.
pssFields :: Lens' ProjectsSubscriptionsSeek (Maybe Text)
pssFields
  = lens _pssFields (\ s a -> s{_pssFields = a})

-- | JSONP
pssCallback :: Lens' ProjectsSubscriptionsSeek (Maybe Text)
pssCallback
  = lens _pssCallback (\ s a -> s{_pssCallback = a})

instance GoogleRequest ProjectsSubscriptionsSeek
         where
        type Rs ProjectsSubscriptionsSeek = SeekResponse
        type Scopes ProjectsSubscriptionsSeek =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/pubsub"]
        requestClient ProjectsSubscriptionsSeek'{..}
          = go _pssSubscription _pssXgafv _pssUploadProtocol
              (Just _pssPp)
              _pssAccessToken
              _pssUploadType
              _pssBearerToken
              _pssCallback
              _pssFields
              (Just AltJSON)
              _pssPayload
              pubSubService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsSubscriptionsSeekResource)
                      mempty
