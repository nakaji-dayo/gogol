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
-- Module      : Network.Google.Resource.SiteVerification.WebResource.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Relinquish ownership of a website or domain.
--
-- /See:/ <https://developers.google.com/site-verification/ Google Site Verification API Reference> for @siteVerification.webResource.delete@.
module Network.Google.Resource.SiteVerification.WebResource.Delete
    (
    -- * REST Resource
      WebResourceDeleteResource

    -- * Creating a Request
    , webResourceDelete
    , WebResourceDelete

    -- * Request Lenses
    , wrdId
    , wrdFields
    ) where

import Network.Google.Prelude
import Network.Google.SiteVerification.Types

-- | A resource alias for @siteVerification.webResource.delete@ method which the
-- 'WebResourceDelete' request conforms to.
type WebResourceDeleteResource =
     "siteVerification" :>
       "v1" :>
         "webResource" :>
           Capture "id" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Relinquish ownership of a website or domain.
--
-- /See:/ 'webResourceDelete' smart constructor.
data WebResourceDelete = WebResourceDelete'
    { _wrdId :: !Text
    , _wrdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'WebResourceDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wrdId'
--
-- * 'wrdFields'
webResourceDelete
    :: Text -- ^ 'wrdId'
    -> WebResourceDelete
webResourceDelete pWrdId_ = 
    WebResourceDelete'
    { _wrdId = pWrdId_
    , _wrdFields = Nothing
    }

-- | The id of a verified site or domain.
wrdId :: Lens' WebResourceDelete Text
wrdId = lens _wrdId (\ s a -> s{_wrdId = a})

-- | Selector specifying which fields to include in a partial response.
wrdFields :: Lens' WebResourceDelete (Maybe Text)
wrdFields
  = lens _wrdFields (\ s a -> s{_wrdFields = a})

instance GoogleRequest WebResourceDelete where
        type Rs WebResourceDelete = ()
        type Scopes WebResourceDelete =
             '["https://www.googleapis.com/auth/siteverification"]
        requestClient WebResourceDelete'{..}
          = go _wrdId _wrdFields (Just AltJSON)
              siteVerificationService
          where go
                  = buildClient
                      (Proxy :: Proxy WebResourceDeleteResource)
                      mempty
