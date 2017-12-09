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
-- Module      : Network.Google.Resource.AdExchangeBuyer.Products.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the requested product by id.
--
-- /See:/ <https://developers.google.com/ad-exchange/buyer-rest Ad Exchange Buyer API Reference> for @adexchangebuyer.products.get@.
module Network.Google.Resource.AdExchangeBuyer.Products.Get
    (
    -- * REST Resource
      ProductsGetResource

    -- * Creating a Request
    , productsGet
    , ProductsGet

    -- * Request Lenses
    , pggProductId
    , pggFields
    ) where

import Network.Google.AdExchangeBuyer.Types
import Network.Google.Prelude

-- | A resource alias for @adexchangebuyer.products.get@ method which the
-- 'ProductsGet' request conforms to.
type ProductsGetResource =
     "adexchangebuyer" :>
       "v1.4" :>
         "products" :>
           Capture "productId" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] Product

-- | Gets the requested product by id.
--
-- /See:/ 'productsGet' smart constructor.
data ProductsGet = ProductsGet'
    { _pggProductId :: !Text
    , _pggFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pggProductId'
--
-- * 'pggFields'
productsGet
    :: Text -- ^ 'pggProductId'
    -> ProductsGet
productsGet pPggProductId_ = 
    ProductsGet'
    { _pggProductId = pPggProductId_
    , _pggFields = Nothing
    }

-- | The id for the product to get the head revision for.
pggProductId :: Lens' ProductsGet Text
pggProductId
  = lens _pggProductId (\ s a -> s{_pggProductId = a})

-- | Selector specifying which fields to include in a partial response.
pggFields :: Lens' ProductsGet (Maybe Text)
pggFields
  = lens _pggFields (\ s a -> s{_pggFields = a})

instance GoogleRequest ProductsGet where
        type Rs ProductsGet = Product
        type Scopes ProductsGet =
             '["https://www.googleapis.com/auth/adexchange.buyer"]
        requestClient ProductsGet'{..}
          = go _pggProductId _pggFields (Just AltJSON)
              adExchangeBuyerService
          where go
                  = buildClient (Proxy :: Proxy ProductsGetResource)
                      mempty
