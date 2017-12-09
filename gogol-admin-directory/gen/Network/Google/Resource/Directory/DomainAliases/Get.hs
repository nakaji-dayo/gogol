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
-- Module      : Network.Google.Resource.Directory.DomainAliases.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a domain alias of the customer.
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.domainAliases.get@.
module Network.Google.Resource.Directory.DomainAliases.Get
    (
    -- * REST Resource
      DomainAliasesGetResource

    -- * Creating a Request
    , domainAliasesGet
    , DomainAliasesGet

    -- * Request Lenses
    , dagDomainAliasName
    , dagCustomer
    , dagFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.domainAliases.get@ method which the
-- 'DomainAliasesGet' request conforms to.
type DomainAliasesGetResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "customer" :>
             Capture "customer" Text :>
               "domainaliases" :>
                 Capture "domainAliasName" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :> Get '[JSON] DomainAlias

-- | Retrieves a domain alias of the customer.
--
-- /See:/ 'domainAliasesGet' smart constructor.
data DomainAliasesGet = DomainAliasesGet'
    { _dagDomainAliasName :: !Text
    , _dagCustomer :: !Text
    , _dagFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DomainAliasesGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dagDomainAliasName'
--
-- * 'dagCustomer'
--
-- * 'dagFields'
domainAliasesGet
    :: Text -- ^ 'dagDomainAliasName'
    -> Text -- ^ 'dagCustomer'
    -> DomainAliasesGet
domainAliasesGet pDagDomainAliasName_ pDagCustomer_ = 
    DomainAliasesGet'
    { _dagDomainAliasName = pDagDomainAliasName_
    , _dagCustomer = pDagCustomer_
    , _dagFields = Nothing
    }

-- | Name of domain alias to be retrieved.
dagDomainAliasName :: Lens' DomainAliasesGet Text
dagDomainAliasName
  = lens _dagDomainAliasName
      (\ s a -> s{_dagDomainAliasName = a})

-- | Immutable ID of the G Suite account.
dagCustomer :: Lens' DomainAliasesGet Text
dagCustomer
  = lens _dagCustomer (\ s a -> s{_dagCustomer = a})

-- | Selector specifying which fields to include in a partial response.
dagFields :: Lens' DomainAliasesGet (Maybe Text)
dagFields
  = lens _dagFields (\ s a -> s{_dagFields = a})

instance GoogleRequest DomainAliasesGet where
        type Rs DomainAliasesGet = DomainAlias
        type Scopes DomainAliasesGet =
             '["https://www.googleapis.com/auth/admin.directory.domain",
               "https://www.googleapis.com/auth/admin.directory.domain.readonly"]
        requestClient DomainAliasesGet'{..}
          = go _dagCustomer _dagDomainAliasName _dagFields
              (Just AltJSON)
              directoryService
          where go
                  = buildClient
                      (Proxy :: Proxy DomainAliasesGetResource)
                      mempty
