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
-- Module      : Network.Google.Resource.Directory.DomainAliases.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the domain aliases of the customer.
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.domainAliases.list@.
module Network.Google.Resource.Directory.DomainAliases.List
    (
    -- * REST Resource
      DomainAliasesListResource

    -- * Creating a Request
    , domainAliasesList
    , DomainAliasesList

    -- * Request Lenses
    , dalCustomer
    , dalParentDomainName
    , dalFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.domainAliases.list@ method which the
-- 'DomainAliasesList' request conforms to.
type DomainAliasesListResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "customer" :>
             Capture "customer" Text :>
               "domainaliases" :>
                 QueryParam "parentDomainName" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :> Get '[JSON] DomainAliases

-- | Lists the domain aliases of the customer.
--
-- /See:/ 'domainAliasesList' smart constructor.
data DomainAliasesList = DomainAliasesList'
    { _dalCustomer :: !Text
    , _dalParentDomainName :: !(Maybe Text)
    , _dalFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DomainAliasesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dalCustomer'
--
-- * 'dalParentDomainName'
--
-- * 'dalFields'
domainAliasesList
    :: Text -- ^ 'dalCustomer'
    -> DomainAliasesList
domainAliasesList pDalCustomer_ = 
    DomainAliasesList'
    { _dalCustomer = pDalCustomer_
    , _dalParentDomainName = Nothing
    , _dalFields = Nothing
    }

-- | Immutable ID of the G Suite account.
dalCustomer :: Lens' DomainAliasesList Text
dalCustomer
  = lens _dalCustomer (\ s a -> s{_dalCustomer = a})

-- | Name of the parent domain for which domain aliases are to be fetched.
dalParentDomainName :: Lens' DomainAliasesList (Maybe Text)
dalParentDomainName
  = lens _dalParentDomainName
      (\ s a -> s{_dalParentDomainName = a})

-- | Selector specifying which fields to include in a partial response.
dalFields :: Lens' DomainAliasesList (Maybe Text)
dalFields
  = lens _dalFields (\ s a -> s{_dalFields = a})

instance GoogleRequest DomainAliasesList where
        type Rs DomainAliasesList = DomainAliases
        type Scopes DomainAliasesList =
             '["https://www.googleapis.com/auth/admin.directory.domain",
               "https://www.googleapis.com/auth/admin.directory.domain.readonly"]
        requestClient DomainAliasesList'{..}
          = go _dalCustomer _dalParentDomainName _dalFields
              (Just AltJSON)
              directoryService
          where go
                  = buildClient
                      (Proxy :: Proxy DomainAliasesListResource)
                      mempty
