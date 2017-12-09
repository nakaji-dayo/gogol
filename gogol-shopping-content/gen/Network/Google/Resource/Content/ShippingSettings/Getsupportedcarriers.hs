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
-- Module      : Network.Google.Resource.Content.ShippingSettings.Getsupportedcarriers
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves supported carriers and carrier services for an account.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.shippingsettings.getsupportedcarriers@.
module Network.Google.Resource.Content.ShippingSettings.Getsupportedcarriers
    (
    -- * REST Resource
      ShippingSettingsGetsupportedcarriersResource

    -- * Creating a Request
    , shippingSettingsGetsupportedcarriers
    , ShippingSettingsGetsupportedcarriers

    -- * Request Lenses
    , ssgMerchantId
    , ssgFields
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.shippingsettings.getsupportedcarriers@ method which the
-- 'ShippingSettingsGetsupportedcarriers' request conforms to.
type ShippingSettingsGetsupportedcarriersResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "supportedCarriers" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 Get '[JSON]
                   ShippingSettingsGetSupportedCarriersResponse

-- | Retrieves supported carriers and carrier services for an account.
--
-- /See:/ 'shippingSettingsGetsupportedcarriers' smart constructor.
data ShippingSettingsGetsupportedcarriers = ShippingSettingsGetsupportedcarriers'
    { _ssgMerchantId :: !(Textual Word64)
    , _ssgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ShippingSettingsGetsupportedcarriers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssgMerchantId'
--
-- * 'ssgFields'
shippingSettingsGetsupportedcarriers
    :: Word64 -- ^ 'ssgMerchantId'
    -> ShippingSettingsGetsupportedcarriers
shippingSettingsGetsupportedcarriers pSsgMerchantId_ = 
    ShippingSettingsGetsupportedcarriers'
    { _ssgMerchantId = _Coerce # pSsgMerchantId_
    , _ssgFields = Nothing
    }

-- | The ID of the account for which to retrieve the supported carriers.
ssgMerchantId :: Lens' ShippingSettingsGetsupportedcarriers Word64
ssgMerchantId
  = lens _ssgMerchantId
      (\ s a -> s{_ssgMerchantId = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
ssgFields :: Lens' ShippingSettingsGetsupportedcarriers (Maybe Text)
ssgFields
  = lens _ssgFields (\ s a -> s{_ssgFields = a})

instance GoogleRequest
         ShippingSettingsGetsupportedcarriers where
        type Rs ShippingSettingsGetsupportedcarriers =
             ShippingSettingsGetSupportedCarriersResponse
        type Scopes ShippingSettingsGetsupportedcarriers =
             '["https://www.googleapis.com/auth/content"]
        requestClient
          ShippingSettingsGetsupportedcarriers'{..}
          = go _ssgMerchantId _ssgFields (Just AltJSON)
              shoppingContentService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ShippingSettingsGetsupportedcarriersResource)
                      mempty
