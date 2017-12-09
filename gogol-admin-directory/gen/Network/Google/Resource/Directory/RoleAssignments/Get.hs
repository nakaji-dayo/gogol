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
-- Module      : Network.Google.Resource.Directory.RoleAssignments.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a role assignment.
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.roleAssignments.get@.
module Network.Google.Resource.Directory.RoleAssignments.Get
    (
    -- * REST Resource
      RoleAssignmentsGetResource

    -- * Creating a Request
    , roleAssignmentsGet
    , RoleAssignmentsGet

    -- * Request Lenses
    , ragCustomer
    , ragRoleAssignmentId
    , ragFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.roleAssignments.get@ method which the
-- 'RoleAssignmentsGet' request conforms to.
type RoleAssignmentsGetResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "customer" :>
             Capture "customer" Text :>
               "roleassignments" :>
                 Capture "roleAssignmentId" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       Get '[JSON] RoleAssignment

-- | Retrieve a role assignment.
--
-- /See:/ 'roleAssignmentsGet' smart constructor.
data RoleAssignmentsGet = RoleAssignmentsGet'
    { _ragCustomer :: !Text
    , _ragRoleAssignmentId :: !Text
    , _ragFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RoleAssignmentsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ragCustomer'
--
-- * 'ragRoleAssignmentId'
--
-- * 'ragFields'
roleAssignmentsGet
    :: Text -- ^ 'ragCustomer'
    -> Text -- ^ 'ragRoleAssignmentId'
    -> RoleAssignmentsGet
roleAssignmentsGet pRagCustomer_ pRagRoleAssignmentId_ = 
    RoleAssignmentsGet'
    { _ragCustomer = pRagCustomer_
    , _ragRoleAssignmentId = pRagRoleAssignmentId_
    , _ragFields = Nothing
    }

-- | Immutable ID of the G Suite account.
ragCustomer :: Lens' RoleAssignmentsGet Text
ragCustomer
  = lens _ragCustomer (\ s a -> s{_ragCustomer = a})

-- | Immutable ID of the role assignment.
ragRoleAssignmentId :: Lens' RoleAssignmentsGet Text
ragRoleAssignmentId
  = lens _ragRoleAssignmentId
      (\ s a -> s{_ragRoleAssignmentId = a})

-- | Selector specifying which fields to include in a partial response.
ragFields :: Lens' RoleAssignmentsGet (Maybe Text)
ragFields
  = lens _ragFields (\ s a -> s{_ragFields = a})

instance GoogleRequest RoleAssignmentsGet where
        type Rs RoleAssignmentsGet = RoleAssignment
        type Scopes RoleAssignmentsGet =
             '["https://www.googleapis.com/auth/admin.directory.rolemanagement",
               "https://www.googleapis.com/auth/admin.directory.rolemanagement.readonly"]
        requestClient RoleAssignmentsGet'{..}
          = go _ragCustomer _ragRoleAssignmentId _ragFields
              (Just AltJSON)
              directoryService
          where go
                  = buildClient
                      (Proxy :: Proxy RoleAssignmentsGetResource)
                      mempty
