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
-- Module      : Network.Google.Resource.Webmasters.Sites.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about specific site.
--
-- /See:/ <https://developers.google.com/webmaster-tools/ Search Console API Reference> for @webmasters.sites.get@.
module Network.Google.Resource.Webmasters.Sites.Get
    (
    -- * REST Resource
      SitesGetResource

    -- * Creating a Request
    , sitesGet
    , SitesGet

    -- * Request Lenses
    , sggSiteURL
    , sggFields
    ) where

import Network.Google.Prelude
import Network.Google.WebmasterTools.Types

-- | A resource alias for @webmasters.sites.get@ method which the
-- 'SitesGet' request conforms to.
type SitesGetResource =
     "webmasters" :>
       "v3" :>
         "sites" :>
           Capture "siteUrl" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] WmxSite

-- | Retrieves information about specific site.
--
-- /See:/ 'sitesGet' smart constructor.
data SitesGet = SitesGet'
    { _sggSiteURL :: !Text
    , _sggFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SitesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sggSiteURL'
--
-- * 'sggFields'
sitesGet
    :: Text -- ^ 'sggSiteURL'
    -> SitesGet
sitesGet pSggSiteURL_ = 
    SitesGet'
    { _sggSiteURL = pSggSiteURL_
    , _sggFields = Nothing
    }

-- | The URI of the property as defined in Search Console. Examples:
-- http:\/\/www.example.com\/ or android-app:\/\/com.example\/
sggSiteURL :: Lens' SitesGet Text
sggSiteURL
  = lens _sggSiteURL (\ s a -> s{_sggSiteURL = a})

-- | Selector specifying which fields to include in a partial response.
sggFields :: Lens' SitesGet (Maybe Text)
sggFields
  = lens _sggFields (\ s a -> s{_sggFields = a})

instance GoogleRequest SitesGet where
        type Rs SitesGet = WmxSite
        type Scopes SitesGet =
             '["https://www.googleapis.com/auth/webmasters",
               "https://www.googleapis.com/auth/webmasters.readonly"]
        requestClient SitesGet'{..}
          = go _sggSiteURL _sggFields (Just AltJSON)
              webmasterToolsService
          where go
                  = buildClient (Proxy :: Proxy SitesGetResource)
                      mempty
