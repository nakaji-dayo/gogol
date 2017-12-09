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
-- Module      : Network.Google.Resource.Content.Productstatuses.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of a product from your Merchant Center account.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.productstatuses.get@.
module Network.Google.Resource.Content.Productstatuses.Get
    (
    -- * REST Resource
      ProductstatusesGetResource

    -- * Creating a Request
    , productstatusesGet
    , ProductstatusesGet

    -- * Request Lenses
    , pgMerchantId
    , pgIncludeAttributes
    , pgProductId
    , pgFields
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.productstatuses.get@ method which the
-- 'ProductstatusesGet' request conforms to.
type ProductstatusesGetResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "productstatuses" :>
             Capture "productId" Text :>
               QueryParam "includeAttributes" Bool :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Get '[JSON] ProductStatus

-- | Gets the status of a product from your Merchant Center account.
--
-- /See:/ 'productstatusesGet' smart constructor.
data ProductstatusesGet = ProductstatusesGet'
    { _pgMerchantId :: !(Textual Word64)
    , _pgIncludeAttributes :: !(Maybe Bool)
    , _pgProductId :: !Text
    , _pgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProductstatusesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgMerchantId'
--
-- * 'pgIncludeAttributes'
--
-- * 'pgProductId'
--
-- * 'pgFields'
productstatusesGet
    :: Word64 -- ^ 'pgMerchantId'
    -> Text -- ^ 'pgProductId'
    -> ProductstatusesGet
productstatusesGet pPgMerchantId_ pPgProductId_ = 
    ProductstatusesGet'
    { _pgMerchantId = _Coerce # pPgMerchantId_
    , _pgIncludeAttributes = Nothing
    , _pgProductId = pPgProductId_
    , _pgFields = Nothing
    }

-- | The ID of the account that contains the product. This account cannot be
-- a multi-client account.
pgMerchantId :: Lens' ProductstatusesGet Word64
pgMerchantId
  = lens _pgMerchantId (\ s a -> s{_pgMerchantId = a})
      . _Coerce

-- | Flag to include full product data in the result of this get request. The
-- default value is false.
pgIncludeAttributes :: Lens' ProductstatusesGet (Maybe Bool)
pgIncludeAttributes
  = lens _pgIncludeAttributes
      (\ s a -> s{_pgIncludeAttributes = a})

-- | The REST id of the product.
pgProductId :: Lens' ProductstatusesGet Text
pgProductId
  = lens _pgProductId (\ s a -> s{_pgProductId = a})

-- | Selector specifying which fields to include in a partial response.
pgFields :: Lens' ProductstatusesGet (Maybe Text)
pgFields = lens _pgFields (\ s a -> s{_pgFields = a})

instance GoogleRequest ProductstatusesGet where
        type Rs ProductstatusesGet = ProductStatus
        type Scopes ProductstatusesGet =
             '["https://www.googleapis.com/auth/content"]
        requestClient ProductstatusesGet'{..}
          = go _pgMerchantId _pgProductId _pgIncludeAttributes
              _pgFields
              (Just AltJSON)
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy ProductstatusesGetResource)
                      mempty
