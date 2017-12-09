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
-- Module      : Network.Google.Resource.Content.Orders.Returnlineitem
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a line item.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.orders.returnlineitem@.
module Network.Google.Resource.Content.Orders.Returnlineitem
    (
    -- * REST Resource
      OrdersReturnlineitemResource

    -- * Creating a Request
    , ordersReturnlineitem
    , OrdersReturnlineitem

    -- * Request Lenses
    , oMerchantId
    , oPayload
    , oOrderId
    , oFields
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.orders.returnlineitem@ method which the
-- 'OrdersReturnlineitem' request conforms to.
type OrdersReturnlineitemResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "orders" :>
             Capture "orderId" Text :>
               "returnLineItem" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] OrdersReturnLineItemRequest :>
                       Post '[JSON] OrdersReturnLineItemResponse

-- | Returns a line item.
--
-- /See:/ 'ordersReturnlineitem' smart constructor.
data OrdersReturnlineitem = OrdersReturnlineitem'
    { _oMerchantId :: !(Textual Word64)
    , _oPayload :: !OrdersReturnLineItemRequest
    , _oOrderId :: !Text
    , _oFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersReturnlineitem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oMerchantId'
--
-- * 'oPayload'
--
-- * 'oOrderId'
--
-- * 'oFields'
ordersReturnlineitem
    :: Word64 -- ^ 'oMerchantId'
    -> OrdersReturnLineItemRequest -- ^ 'oPayload'
    -> Text -- ^ 'oOrderId'
    -> OrdersReturnlineitem
ordersReturnlineitem pOMerchantId_ pOPayload_ pOOrderId_ = 
    OrdersReturnlineitem'
    { _oMerchantId = _Coerce # pOMerchantId_
    , _oPayload = pOPayload_
    , _oOrderId = pOOrderId_
    , _oFields = Nothing
    }

-- | The ID of the account that manages the order. This cannot be a
-- multi-client account.
oMerchantId :: Lens' OrdersReturnlineitem Word64
oMerchantId
  = lens _oMerchantId (\ s a -> s{_oMerchantId = a}) .
      _Coerce

-- | Multipart request metadata.
oPayload :: Lens' OrdersReturnlineitem OrdersReturnLineItemRequest
oPayload = lens _oPayload (\ s a -> s{_oPayload = a})

-- | The ID of the order.
oOrderId :: Lens' OrdersReturnlineitem Text
oOrderId = lens _oOrderId (\ s a -> s{_oOrderId = a})

-- | Selector specifying which fields to include in a partial response.
oFields :: Lens' OrdersReturnlineitem (Maybe Text)
oFields = lens _oFields (\ s a -> s{_oFields = a})

instance GoogleRequest OrdersReturnlineitem where
        type Rs OrdersReturnlineitem =
             OrdersReturnLineItemResponse
        type Scopes OrdersReturnlineitem =
             '["https://www.googleapis.com/auth/content"]
        requestClient OrdersReturnlineitem'{..}
          = go _oMerchantId _oOrderId _oFields (Just AltJSON)
              _oPayload
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy OrdersReturnlineitemResource)
                      mempty
