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
-- Module      : Network.Google.Resource.Directory.Privileges.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of all privileges for a customer.
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.privileges.list@.
module Network.Google.Resource.Directory.Privileges.List
    (
    -- * REST Resource
      PrivilegesListResource

    -- * Creating a Request
    , privilegesList
    , PrivilegesList

    -- * Request Lenses
    , plCustomer
    , plFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.privileges.list@ method which the
-- 'PrivilegesList' request conforms to.
type PrivilegesListResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "customer" :>
             Capture "customer" Text :>
               "roles" :>
                 "ALL" :>
                   "privileges" :>
                     QueryParam "fields" Text :>
                       QueryParam "alt" AltJSON :> Get '[JSON] Privileges

-- | Retrieves a paginated list of all privileges for a customer.
--
-- /See:/ 'privilegesList' smart constructor.
data PrivilegesList = PrivilegesList'
    { _plCustomer :: !Text
    , _plFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'PrivilegesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plCustomer'
--
-- * 'plFields'
privilegesList
    :: Text -- ^ 'plCustomer'
    -> PrivilegesList
privilegesList pPlCustomer_ = 
    PrivilegesList'
    { _plCustomer = pPlCustomer_
    , _plFields = Nothing
    }

-- | Immutable ID of the G Suite account.
plCustomer :: Lens' PrivilegesList Text
plCustomer
  = lens _plCustomer (\ s a -> s{_plCustomer = a})

-- | Selector specifying which fields to include in a partial response.
plFields :: Lens' PrivilegesList (Maybe Text)
plFields = lens _plFields (\ s a -> s{_plFields = a})

instance GoogleRequest PrivilegesList where
        type Rs PrivilegesList = Privileges
        type Scopes PrivilegesList =
             '["https://www.googleapis.com/auth/admin.directory.rolemanagement",
               "https://www.googleapis.com/auth/admin.directory.rolemanagement.readonly"]
        requestClient PrivilegesList'{..}
          = go _plCustomer _plFields (Just AltJSON)
              directoryService
          where go
                  = buildClient (Proxy :: Proxy PrivilegesListResource)
                      mempty
