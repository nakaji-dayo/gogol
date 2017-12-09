{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.IAM
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Manages identity and access control for Google Cloud Platform resources,
-- including the creation of service accounts, which you can use to
-- authenticate to Google and make API calls.
--
-- /See:/ <https://cloud.google.com/iam/ Google Identity and Access Management (IAM) API Reference>
module Network.Google.IAM
    (
    -- * Service Configuration
      iAMService

    -- * OAuth Scopes
    , cloudPlatformScope

    -- * API Declaration
    , IAMAPI

    -- * Resources

    -- ** iam.organizations.roles.create
    , module Network.Google.Resource.IAM.Organizations.Roles.Create

    -- ** iam.organizations.roles.delete
    , module Network.Google.Resource.IAM.Organizations.Roles.Delete

    -- ** iam.organizations.roles.get
    , module Network.Google.Resource.IAM.Organizations.Roles.Get

    -- ** iam.organizations.roles.list
    , module Network.Google.Resource.IAM.Organizations.Roles.List

    -- ** iam.organizations.roles.patch
    , module Network.Google.Resource.IAM.Organizations.Roles.Patch

    -- ** iam.organizations.roles.undelete
    , module Network.Google.Resource.IAM.Organizations.Roles.Undelete

    -- ** iam.permissions.queryTestablePermissions
    , module Network.Google.Resource.IAM.Permissions.QueryTestablePermissions

    -- ** iam.projects.roles.create
    , module Network.Google.Resource.IAM.Projects.Roles.Create

    -- ** iam.projects.roles.delete
    , module Network.Google.Resource.IAM.Projects.Roles.Delete

    -- ** iam.projects.roles.get
    , module Network.Google.Resource.IAM.Projects.Roles.Get

    -- ** iam.projects.roles.list
    , module Network.Google.Resource.IAM.Projects.Roles.List

    -- ** iam.projects.roles.patch
    , module Network.Google.Resource.IAM.Projects.Roles.Patch

    -- ** iam.projects.roles.undelete
    , module Network.Google.Resource.IAM.Projects.Roles.Undelete

    -- ** iam.projects.serviceAccounts.create
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.Create

    -- ** iam.projects.serviceAccounts.delete
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.Delete

    -- ** iam.projects.serviceAccounts.get
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.Get

    -- ** iam.projects.serviceAccounts.getIamPolicy
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.GetIAMPolicy

    -- ** iam.projects.serviceAccounts.keys.create
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.Keys.Create

    -- ** iam.projects.serviceAccounts.keys.delete
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.Keys.Delete

    -- ** iam.projects.serviceAccounts.keys.get
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.Keys.Get

    -- ** iam.projects.serviceAccounts.keys.list
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.Keys.List

    -- ** iam.projects.serviceAccounts.list
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.List

    -- ** iam.projects.serviceAccounts.setIamPolicy
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.SetIAMPolicy

    -- ** iam.projects.serviceAccounts.signBlob
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.SignBlob

    -- ** iam.projects.serviceAccounts.signJwt
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.SignJwt

    -- ** iam.projects.serviceAccounts.testIamPermissions
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.TestIAMPermissions

    -- ** iam.projects.serviceAccounts.update
    , module Network.Google.Resource.IAM.Projects.ServiceAccounts.Update

    -- ** iam.roles.get
    , module Network.Google.Resource.IAM.Roles.Get

    -- ** iam.roles.list
    , module Network.Google.Resource.IAM.Roles.List

    -- ** iam.roles.queryGrantableRoles
    , module Network.Google.Resource.IAM.Roles.QueryGrantableRoles

    -- * Types

    -- ** CreateServiceAccountKeyRequestPrivateKeyType
    , CreateServiceAccountKeyRequestPrivateKeyType (..)

    -- ** UndeleteRoleRequest
    , UndeleteRoleRequest
    , undeleteRoleRequest
    , urrEtag

    -- ** Expr
    , Expr
    , expr
    , eLocation
    , eExpression
    , eTitle
    , eDescription

    -- ** PermissionStage
    , PermissionStage (..)

    -- ** QueryGrantableRolesRequestView
    , QueryGrantableRolesRequestView (..)

    -- ** Empty
    , Empty
    , empty

    -- ** QueryTestablePermissionsResponse
    , QueryTestablePermissionsResponse
    , queryTestablePermissionsResponse
    , qtprNextPageToken
    , qtprPermissions

    -- ** AuditData
    , AuditData
    , auditData
    , adPolicyDelta

    -- ** ServiceAccountKey
    , ServiceAccountKey
    , serviceAccountKey
    , sakValidAfterTime
    , sakPrivateKeyData
    , sakPublicKeyData
    , sakName
    , sakPrivateKeyType
    , sakValidBeforeTime
    , sakKeyAlgorithm

    -- ** CreateServiceAccountKeyRequest
    , CreateServiceAccountKeyRequest
    , createServiceAccountKeyRequest
    , csakrPrivateKeyType
    , csakrKeyAlgorithm

    -- ** SetIAMPolicyRequest
    , SetIAMPolicyRequest
    , setIAMPolicyRequest
    , siprPolicy

    -- ** SignJwtResponse
    , SignJwtResponse
    , signJwtResponse
    , sjrKeyId
    , sjrSignedJwt

    -- ** BindingDelta
    , BindingDelta
    , bindingDelta
    , bdAction
    , bdRole
    , bdMember
    , bdCondition

    -- ** SignBlobRequest
    , SignBlobRequest
    , signBlobRequest
    , sbrBytesToSign

    -- ** ListServiceAccountKeysResponse
    , ListServiceAccountKeysResponse
    , listServiceAccountKeysResponse
    , lsakrKeys

    -- ** Role
    , Role
    , role'
    , rStage
    , rEtag
    , rIncludedPermissions
    , rName
    , rDeleted
    , rTitle
    , rDescription

    -- ** ServiceAccount
    , ServiceAccount
    , serviceAccount
    , saEmail
    , saEtag
    , saUniqueId
    , saName
    , saDisplayName
    , saProjectId
    , saOAuth2ClientId

    -- ** QueryTestablePermissionsRequest
    , QueryTestablePermissionsRequest
    , queryTestablePermissionsRequest
    , qtprFullResourceName
    , qtprPageToken
    , qtprPageSize

    -- ** BindingDeltaAction
    , BindingDeltaAction (..)

    -- ** QueryGrantableRolesResponse
    , QueryGrantableRolesResponse
    , queryGrantableRolesResponse
    , qgrrRoles
    , qgrrNextPageToken

    -- ** Xgafv
    , Xgafv (..)

    -- ** TestIAMPermissionsRequest
    , TestIAMPermissionsRequest
    , testIAMPermissionsRequest
    , tiprPermissions

    -- ** TestIAMPermissionsResponse
    , TestIAMPermissionsResponse
    , testIAMPermissionsResponse
    , tiamprPermissions

    -- ** Policy
    , Policy
    , policy
    , pEtag
    , pVersion
    , pBindings

    -- ** RoleStage
    , RoleStage (..)

    -- ** PolicyDelta
    , PolicyDelta
    , policyDelta
    , pdBindingDeltas

    -- ** QueryGrantableRolesRequest
    , QueryGrantableRolesRequest
    , queryGrantableRolesRequest
    , qgrrFullResourceName
    , qgrrView
    , qgrrPageToken
    , qgrrPageSize

    -- ** SignJwtRequest
    , SignJwtRequest
    , signJwtRequest
    , sjrPayload

    -- ** ServiceAccountKeyKeyAlgorithm
    , ServiceAccountKeyKeyAlgorithm (..)

    -- ** Permission
    , Permission
    , permission
    , pStage
    , pOnlyInPredefinedRoles
    , pCustomRolesSupportLevel
    , pName
    , pTitle
    , pDescription

    -- ** SignBlobResponse
    , SignBlobResponse
    , signBlobResponse
    , sbrSignature
    , sbrKeyId

    -- ** ListServiceAccountsResponse
    , ListServiceAccountsResponse
    , listServiceAccountsResponse
    , lsarNextPageToken
    , lsarAccounts

    -- ** ListRolesResponse
    , ListRolesResponse
    , listRolesResponse
    , lrrRoles
    , lrrNextPageToken

    -- ** CreateServiceAccountKeyRequestKeyAlgorithm
    , CreateServiceAccountKeyRequestKeyAlgorithm (..)

    -- ** PermissionCustomRolesSupportLevel
    , PermissionCustomRolesSupportLevel (..)

    -- ** CreateServiceAccountRequest
    , CreateServiceAccountRequest
    , createServiceAccountRequest
    , csarServiceAccount
    , csarAccountId

    -- ** CreateRoleRequest
    , CreateRoleRequest
    , createRoleRequest
    , crrRoleId
    , crrRole

    -- ** ServiceAccountKeyPrivateKeyType
    , ServiceAccountKeyPrivateKeyType (..)

    -- ** Binding
    , Binding
    , binding
    , bMembers
    , bRole
    ) where

import Network.Google.Prelude
import Network.Google.IAM.Types
import Network.Google.Resource.IAM.Organizations.Roles.Create
import Network.Google.Resource.IAM.Organizations.Roles.Delete
import Network.Google.Resource.IAM.Organizations.Roles.Get
import Network.Google.Resource.IAM.Organizations.Roles.List
import Network.Google.Resource.IAM.Organizations.Roles.Patch
import Network.Google.Resource.IAM.Organizations.Roles.Undelete
import Network.Google.Resource.IAM.Permissions.QueryTestablePermissions
import Network.Google.Resource.IAM.Projects.Roles.Create
import Network.Google.Resource.IAM.Projects.Roles.Delete
import Network.Google.Resource.IAM.Projects.Roles.Get
import Network.Google.Resource.IAM.Projects.Roles.List
import Network.Google.Resource.IAM.Projects.Roles.Patch
import Network.Google.Resource.IAM.Projects.Roles.Undelete
import Network.Google.Resource.IAM.Projects.ServiceAccounts.Create
import Network.Google.Resource.IAM.Projects.ServiceAccounts.Delete
import Network.Google.Resource.IAM.Projects.ServiceAccounts.Get
import Network.Google.Resource.IAM.Projects.ServiceAccounts.GetIAMPolicy
import Network.Google.Resource.IAM.Projects.ServiceAccounts.Keys.Create
import Network.Google.Resource.IAM.Projects.ServiceAccounts.Keys.Delete
import Network.Google.Resource.IAM.Projects.ServiceAccounts.Keys.Get
import Network.Google.Resource.IAM.Projects.ServiceAccounts.Keys.List
import Network.Google.Resource.IAM.Projects.ServiceAccounts.List
import Network.Google.Resource.IAM.Projects.ServiceAccounts.SetIAMPolicy
import Network.Google.Resource.IAM.Projects.ServiceAccounts.SignBlob
import Network.Google.Resource.IAM.Projects.ServiceAccounts.SignJwt
import Network.Google.Resource.IAM.Projects.ServiceAccounts.TestIAMPermissions
import Network.Google.Resource.IAM.Projects.ServiceAccounts.Update
import Network.Google.Resource.IAM.Roles.Get
import Network.Google.Resource.IAM.Roles.List
import Network.Google.Resource.IAM.Roles.QueryGrantableRoles

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Identity and Access Management (IAM) API service.
type IAMAPI =
     RolesListResource :<|> RolesGetResource :<|>
       RolesQueryGrantableRolesResource
       :<|> PermissionsQueryTestablePermissionsResource
       :<|> OrganizationsRolesListResource
       :<|> OrganizationsRolesUndeleteResource
       :<|> OrganizationsRolesPatchResource
       :<|> OrganizationsRolesGetResource
       :<|> OrganizationsRolesCreateResource
       :<|> OrganizationsRolesDeleteResource
       :<|> ProjectsRolesListResource
       :<|> ProjectsRolesUndeleteResource
       :<|> ProjectsRolesPatchResource
       :<|> ProjectsRolesGetResource
       :<|> ProjectsRolesCreateResource
       :<|> ProjectsRolesDeleteResource
       :<|> ProjectsServiceAccountsKeysListResource
       :<|> ProjectsServiceAccountsKeysGetResource
       :<|> ProjectsServiceAccountsKeysCreateResource
       :<|> ProjectsServiceAccountsKeysDeleteResource
       :<|> ProjectsServiceAccountsListResource
       :<|> ProjectsServiceAccountsSignJwtResource
       :<|> ProjectsServiceAccountsGetIAMPolicyResource
       :<|> ProjectsServiceAccountsGetResource
       :<|> ProjectsServiceAccountsCreateResource
       :<|> ProjectsServiceAccountsSetIAMPolicyResource
       :<|> ProjectsServiceAccountsSignBlobResource
       :<|>
       ProjectsServiceAccountsTestIAMPermissionsResource
       :<|> ProjectsServiceAccountsDeleteResource
       :<|> ProjectsServiceAccountsUpdateResource
