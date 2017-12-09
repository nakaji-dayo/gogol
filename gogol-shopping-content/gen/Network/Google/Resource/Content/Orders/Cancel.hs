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
-- Module      : Network.Google.Resource.Content.Orders.Cancel
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels all line items in an order, making a full refund.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.orders.cancel@.
module Network.Google.Resource.Content.Orders.Cancel
    (
    -- * REST Resource
      OrdersCancelResource

    -- * Creating a Request
    , ordersCancel
    , OrdersCancel

    -- * Request Lenses
    , occMerchantId
    , occPayload
    , occOrderId
    , occFields
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.orders.cancel@ method which the
-- 'OrdersCancel' request conforms to.
type OrdersCancelResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "orders" :>
             Capture "orderId" Text :>
               "cancel" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] OrdersCancelRequest :>
                       Post '[JSON] OrdersCancelResponse

-- | Cancels all line items in an order, making a full refund.
--
-- /See:/ 'ordersCancel' smart constructor.
data OrdersCancel = OrdersCancel'
    { _occMerchantId :: !(Textual Word64)
    , _occPayload :: !OrdersCancelRequest
    , _occOrderId :: !Text
    , _occFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrdersCancel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'occMerchantId'
--
-- * 'occPayload'
--
-- * 'occOrderId'
--
-- * 'occFields'
ordersCancel
    :: Word64 -- ^ 'occMerchantId'
    -> OrdersCancelRequest -- ^ 'occPayload'
    -> Text -- ^ 'occOrderId'
    -> OrdersCancel
ordersCancel pOccMerchantId_ pOccPayload_ pOccOrderId_ = 
    OrdersCancel'
    { _occMerchantId = _Coerce # pOccMerchantId_
    , _occPayload = pOccPayload_
    , _occOrderId = pOccOrderId_
    , _occFields = Nothing
    }

-- | The ID of the account that manages the order. This cannot be a
-- multi-client account.
occMerchantId :: Lens' OrdersCancel Word64
occMerchantId
  = lens _occMerchantId
      (\ s a -> s{_occMerchantId = a})
      . _Coerce

-- | Multipart request metadata.
occPayload :: Lens' OrdersCancel OrdersCancelRequest
occPayload
  = lens _occPayload (\ s a -> s{_occPayload = a})

-- | The ID of the order to cancel.
occOrderId :: Lens' OrdersCancel Text
occOrderId
  = lens _occOrderId (\ s a -> s{_occOrderId = a})

-- | Selector specifying which fields to include in a partial response.
occFields :: Lens' OrdersCancel (Maybe Text)
occFields
  = lens _occFields (\ s a -> s{_occFields = a})

instance GoogleRequest OrdersCancel where
        type Rs OrdersCancel = OrdersCancelResponse
        type Scopes OrdersCancel =
             '["https://www.googleapis.com/auth/content"]
        requestClient OrdersCancel'{..}
          = go _occMerchantId _occOrderId _occFields
              (Just AltJSON)
              _occPayload
              shoppingContentService
          where go
                  = buildClient (Proxy :: Proxy OrdersCancelResource)
                      mempty
