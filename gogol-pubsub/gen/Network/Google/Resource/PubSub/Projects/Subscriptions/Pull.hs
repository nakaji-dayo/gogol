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
-- Module      : Network.Google.Resource.PubSub.Projects.Subscriptions.Pull
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pulls messages from the server. Returns an empty list if there are no
-- messages available in the backlog. The server may return \`UNAVAILABLE\`
-- if there are too many concurrent pull requests pending for the given
-- subscription.
--
-- /See:/ <https://cloud.google.com/pubsub/docs Google Cloud Pub/Sub API Reference> for @pubsub.projects.subscriptions.pull@.
module Network.Google.Resource.PubSub.Projects.Subscriptions.Pull
    (
    -- * REST Resource
      ProjectsSubscriptionsPullResource

    -- * Creating a Request
    , projectsSubscriptionsPull
    , ProjectsSubscriptionsPull

    -- * Request Lenses
    , pspsXgafv
    , pspsUploadProtocol
    , pspsPp
    , pspsAccessToken
    , pspsUploadType
    , pspsPayload
    , pspsBearerToken
    , pspsSubscription
    , pspsFields
    , pspsCallback
    ) where

import Network.Google.Prelude
import Network.Google.PubSub.Types

-- | A resource alias for @pubsub.projects.subscriptions.pull@ method which the
-- 'ProjectsSubscriptionsPull' request conforms to.
type ProjectsSubscriptionsPullResource =
     "v1" :>
       CaptureMode "subscription" "pull" Text :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] PullRequest :>
                             Post '[JSON] PullResponse

-- | Pulls messages from the server. Returns an empty list if there are no
-- messages available in the backlog. The server may return \`UNAVAILABLE\`
-- if there are too many concurrent pull requests pending for the given
-- subscription.
--
-- /See:/ 'projectsSubscriptionsPull' smart constructor.
data ProjectsSubscriptionsPull = ProjectsSubscriptionsPull'
    { _pspsXgafv :: !(Maybe Xgafv)
    , _pspsUploadProtocol :: !(Maybe Text)
    , _pspsPp :: !Bool
    , _pspsAccessToken :: !(Maybe Text)
    , _pspsUploadType :: !(Maybe Text)
    , _pspsPayload :: !PullRequest
    , _pspsBearerToken :: !(Maybe Text)
    , _pspsSubscription :: !Text
    , _pspsFields :: !(Maybe Text)
    , _pspsCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectsSubscriptionsPull' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pspsXgafv'
--
-- * 'pspsUploadProtocol'
--
-- * 'pspsPp'
--
-- * 'pspsAccessToken'
--
-- * 'pspsUploadType'
--
-- * 'pspsPayload'
--
-- * 'pspsBearerToken'
--
-- * 'pspsSubscription'
--
-- * 'pspsFields'
--
-- * 'pspsCallback'
projectsSubscriptionsPull
    :: PullRequest -- ^ 'pspsPayload'
    -> Text -- ^ 'pspsSubscription'
    -> ProjectsSubscriptionsPull
projectsSubscriptionsPull pPspsPayload_ pPspsSubscription_ = 
    ProjectsSubscriptionsPull'
    { _pspsXgafv = Nothing
    , _pspsUploadProtocol = Nothing
    , _pspsPp = True
    , _pspsAccessToken = Nothing
    , _pspsUploadType = Nothing
    , _pspsPayload = pPspsPayload_
    , _pspsBearerToken = Nothing
    , _pspsSubscription = pPspsSubscription_
    , _pspsFields = Nothing
    , _pspsCallback = Nothing
    }

-- | V1 error format.
pspsXgafv :: Lens' ProjectsSubscriptionsPull (Maybe Xgafv)
pspsXgafv
  = lens _pspsXgafv (\ s a -> s{_pspsXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
pspsUploadProtocol :: Lens' ProjectsSubscriptionsPull (Maybe Text)
pspsUploadProtocol
  = lens _pspsUploadProtocol
      (\ s a -> s{_pspsUploadProtocol = a})

-- | Pretty-print response.
pspsPp :: Lens' ProjectsSubscriptionsPull Bool
pspsPp = lens _pspsPp (\ s a -> s{_pspsPp = a})

-- | OAuth access token.
pspsAccessToken :: Lens' ProjectsSubscriptionsPull (Maybe Text)
pspsAccessToken
  = lens _pspsAccessToken
      (\ s a -> s{_pspsAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
pspsUploadType :: Lens' ProjectsSubscriptionsPull (Maybe Text)
pspsUploadType
  = lens _pspsUploadType
      (\ s a -> s{_pspsUploadType = a})

-- | Multipart request metadata.
pspsPayload :: Lens' ProjectsSubscriptionsPull PullRequest
pspsPayload
  = lens _pspsPayload (\ s a -> s{_pspsPayload = a})

-- | OAuth bearer token.
pspsBearerToken :: Lens' ProjectsSubscriptionsPull (Maybe Text)
pspsBearerToken
  = lens _pspsBearerToken
      (\ s a -> s{_pspsBearerToken = a})

-- | The subscription from which messages should be pulled. Format is
-- \`projects\/{project}\/subscriptions\/{sub}\`.
pspsSubscription :: Lens' ProjectsSubscriptionsPull Text
pspsSubscription
  = lens _pspsSubscription
      (\ s a -> s{_pspsSubscription = a})

-- | Selector specifying which fields to include in a partial response.
pspsFields :: Lens' ProjectsSubscriptionsPull (Maybe Text)
pspsFields
  = lens _pspsFields (\ s a -> s{_pspsFields = a})

-- | JSONP
pspsCallback :: Lens' ProjectsSubscriptionsPull (Maybe Text)
pspsCallback
  = lens _pspsCallback (\ s a -> s{_pspsCallback = a})

instance GoogleRequest ProjectsSubscriptionsPull
         where
        type Rs ProjectsSubscriptionsPull = PullResponse
        type Scopes ProjectsSubscriptionsPull =
             '["https://www.googleapis.com/auth/cloud-platform",
               "https://www.googleapis.com/auth/pubsub"]
        requestClient ProjectsSubscriptionsPull'{..}
          = go _pspsSubscription _pspsXgafv _pspsUploadProtocol
              (Just _pspsPp)
              _pspsAccessToken
              _pspsUploadType
              _pspsBearerToken
              _pspsCallback
              _pspsFields
              (Just AltJSON)
              _pspsPayload
              pubSubService
          where go
                  = buildClient
                      (Proxy :: Proxy ProjectsSubscriptionsPullResource)
                      mempty
