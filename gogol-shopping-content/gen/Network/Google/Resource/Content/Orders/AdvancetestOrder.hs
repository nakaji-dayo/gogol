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
-- Module      : Network.Google.Resource.Content.Orders.AdvancetestOrder
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sandbox only. Moves a test order from state \"inProgress\" to state
-- \"pendingShipment\".
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.orders.advancetestorder@.
module Network.Google.Resource.Content.Orders.AdvancetestOrder
    (
    -- * REST Resource
      OrdersAdvancetestOrderResource

    -- * Creating a Request
    , ordersAdvancetestOrder
    , OrdersAdvancetestOrder

    -- * Request Lenses
    , oaoMerchantId
    , oaoOrderId
    , oaoFields
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.orders.advancetestorder@ method which the
-- 'OrdersAdvancetestOrder' request conforms to.
type OrdersAdvancetestOrderResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "testorders" :>
             Capture "orderId" Text :>
               "advance" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Post '[JSON] OrdersAdvanceTestOrderResponse

-- | Sandbox only. Moves a test order from state \"inProgress\" to state
-- \"pendingShipment\".
--
-- /See:/ 'ordersAdvancetestOrder' smart constructor.
data OrdersAdvancetestOrder = OrdersAdvancetestOrder'
    { _oaoMerchantId :: !(Textual Word64)
    , _oaoOrderId :: !Text
    , _oaoFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersAdvancetestOrder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oaoMerchantId'
--
-- * 'oaoOrderId'
--
-- * 'oaoFields'
ordersAdvancetestOrder
    :: Word64 -- ^ 'oaoMerchantId'
    -> Text -- ^ 'oaoOrderId'
    -> OrdersAdvancetestOrder
ordersAdvancetestOrder pOaoMerchantId_ pOaoOrderId_ = 
    OrdersAdvancetestOrder'
    { _oaoMerchantId = _Coerce # pOaoMerchantId_
    , _oaoOrderId = pOaoOrderId_
    , _oaoFields = Nothing
    }

-- | The ID of the account that manages the order. This cannot be a
-- multi-client account.
oaoMerchantId :: Lens' OrdersAdvancetestOrder Word64
oaoMerchantId
  = lens _oaoMerchantId
      (\ s a -> s{_oaoMerchantId = a})
      . _Coerce

-- | The ID of the test order to modify.
oaoOrderId :: Lens' OrdersAdvancetestOrder Text
oaoOrderId
  = lens _oaoOrderId (\ s a -> s{_oaoOrderId = a})

-- | Selector specifying which fields to include in a partial response.
oaoFields :: Lens' OrdersAdvancetestOrder (Maybe Text)
oaoFields
  = lens _oaoFields (\ s a -> s{_oaoFields = a})

instance GoogleRequest OrdersAdvancetestOrder where
        type Rs OrdersAdvancetestOrder =
             OrdersAdvanceTestOrderResponse
        type Scopes OrdersAdvancetestOrder =
             '["https://www.googleapis.com/auth/content"]
        requestClient OrdersAdvancetestOrder'{..}
          = go _oaoMerchantId _oaoOrderId _oaoFields
              (Just AltJSON)
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy OrdersAdvancetestOrderResource)
                      mempty
