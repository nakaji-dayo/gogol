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
-- Module      : Network.Google.Resource.Webmasters.URLCrawlErrorsSamples.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists a site\'s sample URLs for the specified crawl error category and
-- platform.
--
-- /See:/ <https://developers.google.com/webmaster-tools/ Search Console API Reference> for @webmasters.urlcrawlerrorssamples.list@.
module Network.Google.Resource.Webmasters.URLCrawlErrorsSamples.List
    (
    -- * REST Resource
      URLCrawlErrorsSamplesListResource

    -- * Creating a Request
    , urlCrawlErrorsSamplesList
    , URLCrawlErrorsSamplesList

    -- * Request Lenses
    , uceslPlatform
    , uceslCategory
    , uceslSiteURL
    , uceslFields
    ) where

import Network.Google.Prelude
import Network.Google.WebmasterTools.Types

-- | A resource alias for @webmasters.urlcrawlerrorssamples.list@ method which the
-- 'URLCrawlErrorsSamplesList' request conforms to.
type URLCrawlErrorsSamplesListResource =
     "webmasters" :>
       "v3" :>
         "sites" :>
           Capture "siteUrl" Text :>
             "urlCrawlErrorsSamples" :>
               QueryParam "category"
                 URLCrawlErrorsSamplesListCategory
                 :>
                 QueryParam "platform"
                   URLCrawlErrorsSamplesListPlatform
                   :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       Get '[JSON] URLCrawlErrorsSamplesListResponse

-- | Lists a site\'s sample URLs for the specified crawl error category and
-- platform.
--
-- /See:/ 'urlCrawlErrorsSamplesList' smart constructor.
data URLCrawlErrorsSamplesList = URLCrawlErrorsSamplesList'
    { _uceslPlatform :: !URLCrawlErrorsSamplesListPlatform
    , _uceslCategory :: !URLCrawlErrorsSamplesListCategory
    , _uceslSiteURL :: !Text
    , _uceslFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'URLCrawlErrorsSamplesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uceslPlatform'
--
-- * 'uceslCategory'
--
-- * 'uceslSiteURL'
--
-- * 'uceslFields'
urlCrawlErrorsSamplesList
    :: URLCrawlErrorsSamplesListPlatform -- ^ 'uceslPlatform'
    -> URLCrawlErrorsSamplesListCategory -- ^ 'uceslCategory'
    -> Text -- ^ 'uceslSiteURL'
    -> URLCrawlErrorsSamplesList
urlCrawlErrorsSamplesList pUceslPlatform_ pUceslCategory_ pUceslSiteURL_ = 
    URLCrawlErrorsSamplesList'
    { _uceslPlatform = pUceslPlatform_
    , _uceslCategory = pUceslCategory_
    , _uceslSiteURL = pUceslSiteURL_
    , _uceslFields = Nothing
    }

-- | The user agent type (platform) that made the request. For example: web
uceslPlatform :: Lens' URLCrawlErrorsSamplesList URLCrawlErrorsSamplesListPlatform
uceslPlatform
  = lens _uceslPlatform
      (\ s a -> s{_uceslPlatform = a})

-- | The crawl error category. For example: authPermissions
uceslCategory :: Lens' URLCrawlErrorsSamplesList URLCrawlErrorsSamplesListCategory
uceslCategory
  = lens _uceslCategory
      (\ s a -> s{_uceslCategory = a})

-- | The site\'s URL, including protocol. For example:
-- http:\/\/www.example.com\/
uceslSiteURL :: Lens' URLCrawlErrorsSamplesList Text
uceslSiteURL
  = lens _uceslSiteURL (\ s a -> s{_uceslSiteURL = a})

-- | Selector specifying which fields to include in a partial response.
uceslFields :: Lens' URLCrawlErrorsSamplesList (Maybe Text)
uceslFields
  = lens _uceslFields (\ s a -> s{_uceslFields = a})

instance GoogleRequest URLCrawlErrorsSamplesList
         where
        type Rs URLCrawlErrorsSamplesList =
             URLCrawlErrorsSamplesListResponse
        type Scopes URLCrawlErrorsSamplesList =
             '["https://www.googleapis.com/auth/webmasters",
               "https://www.googleapis.com/auth/webmasters.readonly"]
        requestClient URLCrawlErrorsSamplesList'{..}
          = go _uceslSiteURL (Just _uceslCategory)
              (Just _uceslPlatform)
              _uceslFields
              (Just AltJSON)
              webmasterToolsService
          where go
                  = buildClient
                      (Proxy :: Proxy URLCrawlErrorsSamplesListResource)
                      mempty
