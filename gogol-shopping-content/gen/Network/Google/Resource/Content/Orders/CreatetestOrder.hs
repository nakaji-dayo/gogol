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
-- Module      : Network.Google.Resource.Content.Orders.CreatetestOrder
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sandbox only. Creates a test order.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.orders.createtestorder@.
module Network.Google.Resource.Content.Orders.CreatetestOrder
    (
    -- * REST Resource
      OrdersCreatetestOrderResource

    -- * Creating a Request
    , ordersCreatetestOrder
    , OrdersCreatetestOrder

    -- * Request Lenses
    , ocoMerchantId
    , ocoPayload
    , ocoFields
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.orders.createtestorder@ method which the
-- 'OrdersCreatetestOrder' request conforms to.
type OrdersCreatetestOrderResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "testorders" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 ReqBody '[JSON] OrdersCreateTestOrderRequest :>
                   Post '[JSON] OrdersCreateTestOrderResponse

-- | Sandbox only. Creates a test order.
--
-- /See:/ 'ordersCreatetestOrder' smart constructor.
data OrdersCreatetestOrder = OrdersCreatetestOrder'
    { _ocoMerchantId :: !(Textual Word64)
    , _ocoPayload :: !OrdersCreateTestOrderRequest
    , _ocoFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCreatetestOrder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocoMerchantId'
--
-- * 'ocoPayload'
--
-- * 'ocoFields'
ordersCreatetestOrder
    :: Word64 -- ^ 'ocoMerchantId'
    -> OrdersCreateTestOrderRequest -- ^ 'ocoPayload'
    -> OrdersCreatetestOrder
ordersCreatetestOrder pOcoMerchantId_ pOcoPayload_ = 
    OrdersCreatetestOrder'
    { _ocoMerchantId = _Coerce # pOcoMerchantId_
    , _ocoPayload = pOcoPayload_
    , _ocoFields = Nothing
    }

-- | The ID of the account that should manage the order. This cannot be a
-- multi-client account.
ocoMerchantId :: Lens' OrdersCreatetestOrder Word64
ocoMerchantId
  = lens _ocoMerchantId
      (\ s a -> s{_ocoMerchantId = a})
      . _Coerce

-- | Multipart request metadata.
ocoPayload :: Lens' OrdersCreatetestOrder OrdersCreateTestOrderRequest
ocoPayload
  = lens _ocoPayload (\ s a -> s{_ocoPayload = a})

-- | Selector specifying which fields to include in a partial response.
ocoFields :: Lens' OrdersCreatetestOrder (Maybe Text)
ocoFields
  = lens _ocoFields (\ s a -> s{_ocoFields = a})

instance GoogleRequest OrdersCreatetestOrder where
        type Rs OrdersCreatetestOrder =
             OrdersCreateTestOrderResponse
        type Scopes OrdersCreatetestOrder =
             '["https://www.googleapis.com/auth/content"]
        requestClient OrdersCreatetestOrder'{..}
          = go _ocoMerchantId _ocoFields (Just AltJSON)
              _ocoPayload
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy OrdersCreatetestOrderResource)
                      mempty
