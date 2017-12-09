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
-- Module      : Network.Google.Resource.Webmasters.Sitemaps.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specific sitemap.
--
-- /See:/ <https://developers.google.com/webmaster-tools/ Search Console API Reference> for @webmasters.sitemaps.get@.
module Network.Google.Resource.Webmasters.Sitemaps.Get
    (
    -- * REST Resource
      SitemapsGetResource

    -- * Creating a Request
    , sitemapsGet
    , SitemapsGet

    -- * Request Lenses
    , sgFeedpath
    , sgSiteURL
    , sgFields
    ) where

import Network.Google.Prelude
import Network.Google.WebmasterTools.Types

-- | A resource alias for @webmasters.sitemaps.get@ method which the
-- 'SitemapsGet' request conforms to.
type SitemapsGetResource =
     "webmasters" :>
       "v3" :>
         "sites" :>
           Capture "siteUrl" Text :>
             "sitemaps" :>
               Capture "feedpath" Text :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] WmxSitemap

-- | Retrieves information about a specific sitemap.
--
-- /See:/ 'sitemapsGet' smart constructor.
data SitemapsGet = SitemapsGet'
    { _sgFeedpath :: !Text
    , _sgSiteURL :: !Text
    , _sgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SitemapsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgFeedpath'
--
-- * 'sgSiteURL'
--
-- * 'sgFields'
sitemapsGet
    :: Text -- ^ 'sgFeedpath'
    -> Text -- ^ 'sgSiteURL'
    -> SitemapsGet
sitemapsGet pSgFeedpath_ pSgSiteURL_ = 
    SitemapsGet'
    { _sgFeedpath = pSgFeedpath_
    , _sgSiteURL = pSgSiteURL_
    , _sgFields = Nothing
    }

-- | The URL of the actual sitemap. For example:
-- http:\/\/www.example.com\/sitemap.xml
sgFeedpath :: Lens' SitemapsGet Text
sgFeedpath
  = lens _sgFeedpath (\ s a -> s{_sgFeedpath = a})

-- | The site\'s URL, including protocol. For example:
-- http:\/\/www.example.com\/
sgSiteURL :: Lens' SitemapsGet Text
sgSiteURL
  = lens _sgSiteURL (\ s a -> s{_sgSiteURL = a})

-- | Selector specifying which fields to include in a partial response.
sgFields :: Lens' SitemapsGet (Maybe Text)
sgFields = lens _sgFields (\ s a -> s{_sgFields = a})

instance GoogleRequest SitemapsGet where
        type Rs SitemapsGet = WmxSitemap
        type Scopes SitemapsGet =
             '["https://www.googleapis.com/auth/webmasters",
               "https://www.googleapis.com/auth/webmasters.readonly"]
        requestClient SitemapsGet'{..}
          = go _sgSiteURL _sgFeedpath _sgFields (Just AltJSON)
              webmasterToolsService
          where go
                  = buildClient (Proxy :: Proxy SitemapsGetResource)
                      mempty
