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
-- Module      : Network.Google.Resource.Webmasters.Sites.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a site from the set of the user\'s Search Console sites.
--
-- /See:/ <https://developers.google.com/webmaster-tools/ Search Console API Reference> for @webmasters.sites.delete@.
module Network.Google.Resource.Webmasters.Sites.Delete
    (
    -- * REST Resource
      SitesDeleteResource

    -- * Creating a Request
    , sitesDelete
    , SitesDelete

    -- * Request Lenses
    , sSiteURL
    , sFields
    ) where

import Network.Google.Prelude
import Network.Google.WebmasterTools.Types

-- | A resource alias for @webmasters.sites.delete@ method which the
-- 'SitesDelete' request conforms to.
type SitesDeleteResource =
     "webmasters" :>
       "v3" :>
         "sites" :>
           Capture "siteUrl" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Removes a site from the set of the user\'s Search Console sites.
--
-- /See:/ 'sitesDelete' smart constructor.
data SitesDelete = SitesDelete'
    { _sSiteURL :: !Text
    , _sFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SitesDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSiteURL'
--
-- * 'sFields'
sitesDelete
    :: Text -- ^ 'sSiteURL'
    -> SitesDelete
sitesDelete pSSiteURL_ = 
    SitesDelete'
    { _sSiteURL = pSSiteURL_
    , _sFields = Nothing
    }

-- | The URI of the property as defined in Search Console. Examples:
-- http:\/\/www.example.com\/ or android-app:\/\/com.example\/
sSiteURL :: Lens' SitesDelete Text
sSiteURL = lens _sSiteURL (\ s a -> s{_sSiteURL = a})

-- | Selector specifying which fields to include in a partial response.
sFields :: Lens' SitesDelete (Maybe Text)
sFields = lens _sFields (\ s a -> s{_sFields = a})

instance GoogleRequest SitesDelete where
        type Rs SitesDelete = ()
        type Scopes SitesDelete =
             '["https://www.googleapis.com/auth/webmasters"]
        requestClient SitesDelete'{..}
          = go _sSiteURL _sFields (Just AltJSON)
              webmasterToolsService
          where go
                  = buildClient (Proxy :: Proxy SitesDeleteResource)
                      mempty
