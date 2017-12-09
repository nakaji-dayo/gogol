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
-- Module      : Network.Google.Resource.Reseller.Customers.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a customer account\'s settings. This method supports patch
-- semantics.
--
-- /See:/ <https://developers.google.com/google-apps/reseller/ Enterprise Apps Reseller API Reference> for @reseller.customers.patch@.
module Network.Google.Resource.Reseller.Customers.Patch
    (
    -- * REST Resource
      CustomersPatchResource

    -- * Creating a Request
    , customersPatch
    , CustomersPatch

    -- * Request Lenses
    , cpPayload
    , cpCustomerId
    , cpFields
    ) where

import Network.Google.AppsReseller.Types
import Network.Google.Prelude

-- | A resource alias for @reseller.customers.patch@ method which the
-- 'CustomersPatch' request conforms to.
type CustomersPatchResource =
     "apps" :>
       "reseller" :>
         "v1" :>
           "customers" :>
             Capture "customerId" Text :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] Customer :> Patch '[JSON] Customer

-- | Update a customer account\'s settings. This method supports patch
-- semantics.
--
-- /See:/ 'customersPatch' smart constructor.
data CustomersPatch = CustomersPatch'
    { _cpPayload :: !Customer
    , _cpCustomerId :: !Text
    , _cpFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CustomersPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPayload'
--
-- * 'cpCustomerId'
--
-- * 'cpFields'
customersPatch
    :: Customer -- ^ 'cpPayload'
    -> Text -- ^ 'cpCustomerId'
    -> CustomersPatch
customersPatch pCpPayload_ pCpCustomerId_ = 
    CustomersPatch'
    { _cpPayload = pCpPayload_
    , _cpCustomerId = pCpCustomerId_
    , _cpFields = Nothing
    }

-- | Multipart request metadata.
cpPayload :: Lens' CustomersPatch Customer
cpPayload
  = lens _cpPayload (\ s a -> s{_cpPayload = a})

-- | Either the customer\'s primary domain name or the customer\'s unique
-- identifier. If using the domain name, we do not recommend using a
-- customerId as a key for persistent data. If the domain name for a
-- customerId is changed, the Google system automatically updates.
cpCustomerId :: Lens' CustomersPatch Text
cpCustomerId
  = lens _cpCustomerId (\ s a -> s{_cpCustomerId = a})

-- | Selector specifying which fields to include in a partial response.
cpFields :: Lens' CustomersPatch (Maybe Text)
cpFields = lens _cpFields (\ s a -> s{_cpFields = a})

instance GoogleRequest CustomersPatch where
        type Rs CustomersPatch = Customer
        type Scopes CustomersPatch =
             '["https://www.googleapis.com/auth/apps.order"]
        requestClient CustomersPatch'{..}
          = go _cpCustomerId _cpFields (Just AltJSON)
              _cpPayload
              appsResellerService
          where go
                  = buildClient (Proxy :: Proxy CustomersPatchResource)
                      mempty
