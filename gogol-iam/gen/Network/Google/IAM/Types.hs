{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.IAM.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.IAM.Types
    (
    -- * Service Configuration
      iAMService

    -- * OAuth Scopes
    , cloudPlatformScope

    -- * CreateServiceAccountKeyRequestPrivateKeyType
    , CreateServiceAccountKeyRequestPrivateKeyType (..)

    -- * UndeleteRoleRequest
    , UndeleteRoleRequest
    , undeleteRoleRequest
    , urrEtag

    -- * Expr
    , Expr
    , expr
    , eLocation
    , eExpression
    , eTitle
    , eDescription

    -- * PermissionStage
    , PermissionStage (..)

    -- * QueryGrantableRolesRequestView
    , QueryGrantableRolesRequestView (..)

    -- * Empty
    , Empty
    , empty

    -- * QueryTestablePermissionsResponse
    , QueryTestablePermissionsResponse
    , queryTestablePermissionsResponse
    , qtprNextPageToken
    , qtprPermissions

    -- * AuditData
    , AuditData
    , auditData
    , adPolicyDelta

    -- * ServiceAccountKey
    , ServiceAccountKey
    , serviceAccountKey
    , sakValidAfterTime
    , sakPrivateKeyData
    , sakPublicKeyData
    , sakName
    , sakPrivateKeyType
    , sakValidBeforeTime
    , sakKeyAlgorithm

    -- * CreateServiceAccountKeyRequest
    , CreateServiceAccountKeyRequest
    , createServiceAccountKeyRequest
    , csakrPrivateKeyType
    , csakrKeyAlgorithm

    -- * SetIAMPolicyRequest
    , SetIAMPolicyRequest
    , setIAMPolicyRequest
    , siprPolicy

    -- * SignJwtResponse
    , SignJwtResponse
    , signJwtResponse
    , sjrKeyId
    , sjrSignedJwt

    -- * BindingDelta
    , BindingDelta
    , bindingDelta
    , bdAction
    , bdRole
    , bdMember
    , bdCondition

    -- * SignBlobRequest
    , SignBlobRequest
    , signBlobRequest
    , sbrBytesToSign

    -- * ListServiceAccountKeysResponse
    , ListServiceAccountKeysResponse
    , listServiceAccountKeysResponse
    , lsakrKeys

    -- * Role
    , Role
    , role'
    , rStage
    , rEtag
    , rIncludedPermissions
    , rName
    , rDeleted
    , rTitle
    , rDescription

    -- * ServiceAccount
    , ServiceAccount
    , serviceAccount
    , saEmail
    , saEtag
    , saUniqueId
    , saName
    , saDisplayName
    , saProjectId
    , saOAuth2ClientId

    -- * QueryTestablePermissionsRequest
    , QueryTestablePermissionsRequest
    , queryTestablePermissionsRequest
    , qtprFullResourceName
    , qtprPageToken
    , qtprPageSize

    -- * BindingDeltaAction
    , BindingDeltaAction (..)

    -- * QueryGrantableRolesResponse
    , QueryGrantableRolesResponse
    , queryGrantableRolesResponse
    , qgrrRoles
    , qgrrNextPageToken

    -- * Xgafv
    , Xgafv (..)

    -- * TestIAMPermissionsRequest
    , TestIAMPermissionsRequest
    , testIAMPermissionsRequest
    , tiprPermissions

    -- * TestIAMPermissionsResponse
    , TestIAMPermissionsResponse
    , testIAMPermissionsResponse
    , tiamprPermissions

    -- * Policy
    , Policy
    , policy
    , pEtag
    , pVersion
    , pBindings

    -- * RoleStage
    , RoleStage (..)

    -- * PolicyDelta
    , PolicyDelta
    , policyDelta
    , pdBindingDeltas

    -- * QueryGrantableRolesRequest
    , QueryGrantableRolesRequest
    , queryGrantableRolesRequest
    , qgrrFullResourceName
    , qgrrView
    , qgrrPageToken
    , qgrrPageSize

    -- * SignJwtRequest
    , SignJwtRequest
    , signJwtRequest
    , sjrPayload

    -- * ServiceAccountKeyKeyAlgorithm
    , ServiceAccountKeyKeyAlgorithm (..)

    -- * Permission
    , Permission
    , permission
    , pStage
    , pOnlyInPredefinedRoles
    , pCustomRolesSupportLevel
    , pName
    , pTitle
    , pDescription

    -- * SignBlobResponse
    , SignBlobResponse
    , signBlobResponse
    , sbrSignature
    , sbrKeyId

    -- * ListServiceAccountsResponse
    , ListServiceAccountsResponse
    , listServiceAccountsResponse
    , lsarNextPageToken
    , lsarAccounts

    -- * ListRolesResponse
    , ListRolesResponse
    , listRolesResponse
    , lrrRoles
    , lrrNextPageToken

    -- * CreateServiceAccountKeyRequestKeyAlgorithm
    , CreateServiceAccountKeyRequestKeyAlgorithm (..)

    -- * PermissionCustomRolesSupportLevel
    , PermissionCustomRolesSupportLevel (..)

    -- * CreateServiceAccountRequest
    , CreateServiceAccountRequest
    , createServiceAccountRequest
    , csarServiceAccount
    , csarAccountId

    -- * CreateRoleRequest
    , CreateRoleRequest
    , createRoleRequest
    , crrRoleId
    , crrRole

    -- * ServiceAccountKeyPrivateKeyType
    , ServiceAccountKeyPrivateKeyType (..)

    -- * Binding
    , Binding
    , binding
    , bMembers
    , bRole
    ) where

import Network.Google.IAM.Types.Product
import Network.Google.IAM.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v1' of the Google Identity and Access Management (IAM) API. This contains the host and root path used as a starting point for constructing service requests.
iAMService :: ServiceConfig
iAMService
  = defaultService (ServiceId "iam:v1")
      "iam.googleapis.com"

-- | View and manage your data across Google Cloud Platform services
cloudPlatformScope :: Proxy '["https://www.googleapis.com/auth/cloud-platform"]
cloudPlatformScope = Proxy;
