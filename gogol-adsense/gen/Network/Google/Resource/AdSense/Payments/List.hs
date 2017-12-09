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
-- Module      : Network.Google.Resource.AdSense.Payments.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the payments for this AdSense account.
--
-- /See:/ <https://developers.google.com/adsense/management/ AdSense Management API Reference> for @adsense.payments.list@.
module Network.Google.Resource.AdSense.Payments.List
    (
    -- * REST Resource
      PaymentsListResource

    -- * Creating a Request
    , paymentsList
    , PaymentsList

    -- * Request Lenses
    , plFields
    ) where

import Network.Google.AdSense.Types
import Network.Google.Prelude

-- | A resource alias for @adsense.payments.list@ method which the
-- 'PaymentsList' request conforms to.
type PaymentsListResource =
     "adsense" :>
       "v1.4" :>
         "payments" :>
           QueryParam "fields" Text :>
             QueryParam "alt" AltJSON :> Get '[JSON] Payments

-- | List the payments for this AdSense account.
--
-- /See:/ 'paymentsList' smart constructor.
newtype PaymentsList = PaymentsList'
    { _plFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PaymentsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plFields'
paymentsList
    :: PaymentsList
paymentsList = 
    PaymentsList'
    { _plFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
plFields :: Lens' PaymentsList (Maybe Text)
plFields = lens _plFields (\ s a -> s{_plFields = a})

instance GoogleRequest PaymentsList where
        type Rs PaymentsList = Payments
        type Scopes PaymentsList =
             '["https://www.googleapis.com/auth/adsense",
               "https://www.googleapis.com/auth/adsense.readonly"]
        requestClient PaymentsList'{..}
          = go _plFields (Just AltJSON) adSenseService
          where go
                  = buildClient (Proxy :: Proxy PaymentsListResource)
                      mempty
