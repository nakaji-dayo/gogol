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
-- Module      : Network.Google.Resource.Directory.Roles.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a role.
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.roles.delete@.
module Network.Google.Resource.Directory.Roles.Delete
    (
    -- * REST Resource
      RolesDeleteResource

    -- * Creating a Request
    , rolesDelete
    , RolesDelete

    -- * Request Lenses
    , rdRoleId
    , rdCustomer
    , rdFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.roles.delete@ method which the
-- 'RolesDelete' request conforms to.
type RolesDeleteResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "customer" :>
             Capture "customer" Text :>
               "roles" :>
                 Capture "roleId" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Deletes a role.
--
-- /See:/ 'rolesDelete' smart constructor.
data RolesDelete = RolesDelete'
    { _rdRoleId :: !Text
    , _rdCustomer :: !Text
    , _rdFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RolesDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdRoleId'
--
-- * 'rdCustomer'
--
-- * 'rdFields'
rolesDelete
    :: Text -- ^ 'rdRoleId'
    -> Text -- ^ 'rdCustomer'
    -> RolesDelete
rolesDelete pRdRoleId_ pRdCustomer_ = 
    RolesDelete'
    { _rdRoleId = pRdRoleId_
    , _rdCustomer = pRdCustomer_
    , _rdFields = Nothing
    }

-- | Immutable ID of the role.
rdRoleId :: Lens' RolesDelete Text
rdRoleId = lens _rdRoleId (\ s a -> s{_rdRoleId = a})

-- | Immutable ID of the G Suite account.
rdCustomer :: Lens' RolesDelete Text
rdCustomer
  = lens _rdCustomer (\ s a -> s{_rdCustomer = a})

-- | Selector specifying which fields to include in a partial response.
rdFields :: Lens' RolesDelete (Maybe Text)
rdFields = lens _rdFields (\ s a -> s{_rdFields = a})

instance GoogleRequest RolesDelete where
        type Rs RolesDelete = ()
        type Scopes RolesDelete =
             '["https://www.googleapis.com/auth/admin.directory.rolemanagement"]
        requestClient RolesDelete'{..}
          = go _rdCustomer _rdRoleId _rdFields (Just AltJSON)
              directoryService
          where go
                  = buildClient (Proxy :: Proxy RolesDeleteResource)
                      mempty
