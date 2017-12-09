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
-- Module      : Network.Google.Resource.AdExchangeBuyer.Products.Search
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the requested product.
--
-- /See:/ <https://developers.google.com/ad-exchange/buyer-rest Ad Exchange Buyer API Reference> for @adexchangebuyer.products.search@.
module Network.Google.Resource.AdExchangeBuyer.Products.Search
    (
    -- * REST Resource
      ProductsSearchResource

    -- * Creating a Request
    , productsSearch
    , ProductsSearch

    -- * Request Lenses
    , psPqlQuery
    , psFields
    ) where

import Network.Google.AdExchangeBuyer.Types
import Network.Google.Prelude

-- | A resource alias for @adexchangebuyer.products.search@ method which the
-- 'ProductsSearch' request conforms to.
type ProductsSearchResource =
     "adexchangebuyer" :>
       "v1.4" :>
         "products" :>
           "search" :>
             QueryParam "pqlQuery" Text :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   Get '[JSON] GetOffersResponse

-- | Gets the requested product.
--
-- /See:/ 'productsSearch' smart constructor.
data ProductsSearch = ProductsSearch'
    { _psPqlQuery :: !(Maybe Text)
    , _psFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductsSearch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psPqlQuery'
--
-- * 'psFields'
productsSearch
    :: ProductsSearch
productsSearch = 
    ProductsSearch'
    { _psPqlQuery = Nothing
    , _psFields = Nothing
    }

-- | The pql query used to query for products.
psPqlQuery :: Lens' ProductsSearch (Maybe Text)
psPqlQuery
  = lens _psPqlQuery (\ s a -> s{_psPqlQuery = a})

-- | Selector specifying which fields to include in a partial response.
psFields :: Lens' ProductsSearch (Maybe Text)
psFields = lens _psFields (\ s a -> s{_psFields = a})

instance GoogleRequest ProductsSearch where
        type Rs ProductsSearch = GetOffersResponse
        type Scopes ProductsSearch =
             '["https://www.googleapis.com/auth/adexchange.buyer"]
        requestClient ProductsSearch'{..}
          = go _psPqlQuery _psFields (Just AltJSON)
              adExchangeBuyerService
          where go
                  = buildClient (Proxy :: Proxy ProductsSearchResource)
                      mempty
