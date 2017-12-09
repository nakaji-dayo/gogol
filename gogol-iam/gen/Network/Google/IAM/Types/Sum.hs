{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.IAM.Types.Sum
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.IAM.Types.Sum where

import Network.Google.Prelude

-- | The output format of the private key. \`GOOGLE_CREDENTIALS_FILE\` is the
-- default output format.
data CreateServiceAccountKeyRequestPrivateKeyType
    = TypeUnspecified
      -- ^ @TYPE_UNSPECIFIED@
      -- Unspecified. Equivalent to \`TYPE_GOOGLE_CREDENTIALS_FILE\`.
    | TypePKCS12File
      -- ^ @TYPE_PKCS12_FILE@
      -- PKCS12 format. The password for the PKCS12 file is \`notasecret\`. For
      -- more information, see https:\/\/tools.ietf.org\/html\/rfc7292.
    | TypeGoogleCredentialsFile
      -- ^ @TYPE_GOOGLE_CREDENTIALS_FILE@
      -- Google Credentials File format.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable CreateServiceAccountKeyRequestPrivateKeyType

instance FromHttpApiData CreateServiceAccountKeyRequestPrivateKeyType where
    parseQueryParam = \case
        "TYPE_UNSPECIFIED" -> Right TypeUnspecified
        "TYPE_PKCS12_FILE" -> Right TypePKCS12File
        "TYPE_GOOGLE_CREDENTIALS_FILE" -> Right TypeGoogleCredentialsFile
        x -> Left ("Unable to parse CreateServiceAccountKeyRequestPrivateKeyType from: " <> x)

instance ToHttpApiData CreateServiceAccountKeyRequestPrivateKeyType where
    toQueryParam = \case
        TypeUnspecified -> "TYPE_UNSPECIFIED"
        TypePKCS12File -> "TYPE_PKCS12_FILE"
        TypeGoogleCredentialsFile -> "TYPE_GOOGLE_CREDENTIALS_FILE"

instance FromJSON CreateServiceAccountKeyRequestPrivateKeyType where
    parseJSON = parseJSONText "CreateServiceAccountKeyRequestPrivateKeyType"

instance ToJSON CreateServiceAccountKeyRequestPrivateKeyType where
    toJSON = toJSONText

-- | The current launch stage of the permission.
data PermissionStage
    = Alpha
      -- ^ @ALPHA@
      -- The permission is currently in an alpha phase.
    | Beta
      -- ^ @BETA@
      -- The permission is currently in a beta phase.
    | GA
      -- ^ @GA@
      -- The permission is generally available.
    | Deprecated
      -- ^ @DEPRECATED@
      -- The permission is being deprecated.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable PermissionStage

instance FromHttpApiData PermissionStage where
    parseQueryParam = \case
        "ALPHA" -> Right Alpha
        "BETA" -> Right Beta
        "GA" -> Right GA
        "DEPRECATED" -> Right Deprecated
        x -> Left ("Unable to parse PermissionStage from: " <> x)

instance ToHttpApiData PermissionStage where
    toQueryParam = \case
        Alpha -> "ALPHA"
        Beta -> "BETA"
        GA -> "GA"
        Deprecated -> "DEPRECATED"

instance FromJSON PermissionStage where
    parseJSON = parseJSONText "PermissionStage"

instance ToJSON PermissionStage where
    toJSON = toJSONText

data QueryGrantableRolesRequestView
    = Basic
      -- ^ @BASIC@
      -- Omits the \`included_permissions\` field. This is the default value.
    | Full
      -- ^ @FULL@
      -- Returns all fields.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable QueryGrantableRolesRequestView

instance FromHttpApiData QueryGrantableRolesRequestView where
    parseQueryParam = \case
        "BASIC" -> Right Basic
        "FULL" -> Right Full
        x -> Left ("Unable to parse QueryGrantableRolesRequestView from: " <> x)

instance ToHttpApiData QueryGrantableRolesRequestView where
    toQueryParam = \case
        Basic -> "BASIC"
        Full -> "FULL"

instance FromJSON QueryGrantableRolesRequestView where
    parseJSON = parseJSONText "QueryGrantableRolesRequestView"

instance ToJSON QueryGrantableRolesRequestView where
    toJSON = toJSONText

-- | The action that was performed on a Binding. Required
data BindingDeltaAction
    = ActionUnspecified
      -- ^ @ACTION_UNSPECIFIED@
      -- Unspecified.
    | Add
      -- ^ @ADD@
      -- Addition of a Binding.
    | Remove
      -- ^ @REMOVE@
      -- Removal of a Binding.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable BindingDeltaAction

instance FromHttpApiData BindingDeltaAction where
    parseQueryParam = \case
        "ACTION_UNSPECIFIED" -> Right ActionUnspecified
        "ADD" -> Right Add
        "REMOVE" -> Right Remove
        x -> Left ("Unable to parse BindingDeltaAction from: " <> x)

instance ToHttpApiData BindingDeltaAction where
    toQueryParam = \case
        ActionUnspecified -> "ACTION_UNSPECIFIED"
        Add -> "ADD"
        Remove -> "REMOVE"

instance FromJSON BindingDeltaAction where
    parseJSON = parseJSONText "BindingDeltaAction"

instance ToJSON BindingDeltaAction where
    toJSON = toJSONText

-- | V1 error format.
data Xgafv
    = X1
      -- ^ @1@
      -- v1 error format
    | X2
      -- ^ @2@
      -- v2 error format
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable Xgafv

instance FromHttpApiData Xgafv where
    parseQueryParam = \case
        "1" -> Right X1
        "2" -> Right X2
        x -> Left ("Unable to parse Xgafv from: " <> x)

instance ToHttpApiData Xgafv where
    toQueryParam = \case
        X1 -> "1"
        X2 -> "2"

instance FromJSON Xgafv where
    parseJSON = parseJSONText "Xgafv"

instance ToJSON Xgafv where
    toJSON = toJSONText

-- | The current launch stage of the role.
data RoleStage
    = RSAlpha
      -- ^ @ALPHA@
      -- The user has indicated this role is currently in an alpha phase.
    | RSBeta
      -- ^ @BETA@
      -- The user has indicated this role is currently in a beta phase.
    | RSGA
      -- ^ @GA@
      -- The user has indicated this role is generally available.
    | RSDeprecated
      -- ^ @DEPRECATED@
      -- The user has indicated this role is being deprecated.
    | RSDisabled
      -- ^ @DISABLED@
      -- This role is disabled and will not contribute permissions to any members
      -- it is granted to in policies.
    | RSEap
      -- ^ @EAP@
      -- The user has indicated this role is currently in an eap phase.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable RoleStage

instance FromHttpApiData RoleStage where
    parseQueryParam = \case
        "ALPHA" -> Right RSAlpha
        "BETA" -> Right RSBeta
        "GA" -> Right RSGA
        "DEPRECATED" -> Right RSDeprecated
        "DISABLED" -> Right RSDisabled
        "EAP" -> Right RSEap
        x -> Left ("Unable to parse RoleStage from: " <> x)

instance ToHttpApiData RoleStage where
    toQueryParam = \case
        RSAlpha -> "ALPHA"
        RSBeta -> "BETA"
        RSGA -> "GA"
        RSDeprecated -> "DEPRECATED"
        RSDisabled -> "DISABLED"
        RSEap -> "EAP"

instance FromJSON RoleStage where
    parseJSON = parseJSONText "RoleStage"

instance ToJSON RoleStage where
    toJSON = toJSONText

-- | Specifies the algorithm (and possibly key size) for the key.
data ServiceAccountKeyKeyAlgorithm
    = KeyAlgUnspecified
      -- ^ @KEY_ALG_UNSPECIFIED@
      -- An unspecified key algorithm.
    | KeyAlgRsa1024
      -- ^ @KEY_ALG_RSA_1024@
      -- 1k RSA Key.
    | KeyAlgRsa2048
      -- ^ @KEY_ALG_RSA_2048@
      -- 2k RSA Key.
    | KeyAlgGcsSymmetricHmac
      -- ^ @KEY_ALG_GCS_SYMMETRIC_HMAC@
      -- HMAC.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable ServiceAccountKeyKeyAlgorithm

instance FromHttpApiData ServiceAccountKeyKeyAlgorithm where
    parseQueryParam = \case
        "KEY_ALG_UNSPECIFIED" -> Right KeyAlgUnspecified
        "KEY_ALG_RSA_1024" -> Right KeyAlgRsa1024
        "KEY_ALG_RSA_2048" -> Right KeyAlgRsa2048
        "KEY_ALG_GCS_SYMMETRIC_HMAC" -> Right KeyAlgGcsSymmetricHmac
        x -> Left ("Unable to parse ServiceAccountKeyKeyAlgorithm from: " <> x)

instance ToHttpApiData ServiceAccountKeyKeyAlgorithm where
    toQueryParam = \case
        KeyAlgUnspecified -> "KEY_ALG_UNSPECIFIED"
        KeyAlgRsa1024 -> "KEY_ALG_RSA_1024"
        KeyAlgRsa2048 -> "KEY_ALG_RSA_2048"
        KeyAlgGcsSymmetricHmac -> "KEY_ALG_GCS_SYMMETRIC_HMAC"

instance FromJSON ServiceAccountKeyKeyAlgorithm where
    parseJSON = parseJSONText "ServiceAccountKeyKeyAlgorithm"

instance ToJSON ServiceAccountKeyKeyAlgorithm where
    toJSON = toJSONText

-- | Which type of key and algorithm to use for the key. The default is
-- currently a 2K RSA key. However this may change in the future.
data CreateServiceAccountKeyRequestKeyAlgorithm
    = CSAKRKAKeyAlgUnspecified
      -- ^ @KEY_ALG_UNSPECIFIED@
      -- An unspecified key algorithm.
    | CSAKRKAKeyAlgRsa1024
      -- ^ @KEY_ALG_RSA_1024@
      -- 1k RSA Key.
    | CSAKRKAKeyAlgRsa2048
      -- ^ @KEY_ALG_RSA_2048@
      -- 2k RSA Key.
    | CSAKRKAKeyAlgGcsSymmetricHmac
      -- ^ @KEY_ALG_GCS_SYMMETRIC_HMAC@
      -- HMAC.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable CreateServiceAccountKeyRequestKeyAlgorithm

instance FromHttpApiData CreateServiceAccountKeyRequestKeyAlgorithm where
    parseQueryParam = \case
        "KEY_ALG_UNSPECIFIED" -> Right CSAKRKAKeyAlgUnspecified
        "KEY_ALG_RSA_1024" -> Right CSAKRKAKeyAlgRsa1024
        "KEY_ALG_RSA_2048" -> Right CSAKRKAKeyAlgRsa2048
        "KEY_ALG_GCS_SYMMETRIC_HMAC" -> Right CSAKRKAKeyAlgGcsSymmetricHmac
        x -> Left ("Unable to parse CreateServiceAccountKeyRequestKeyAlgorithm from: " <> x)

instance ToHttpApiData CreateServiceAccountKeyRequestKeyAlgorithm where
    toQueryParam = \case
        CSAKRKAKeyAlgUnspecified -> "KEY_ALG_UNSPECIFIED"
        CSAKRKAKeyAlgRsa1024 -> "KEY_ALG_RSA_1024"
        CSAKRKAKeyAlgRsa2048 -> "KEY_ALG_RSA_2048"
        CSAKRKAKeyAlgGcsSymmetricHmac -> "KEY_ALG_GCS_SYMMETRIC_HMAC"

instance FromJSON CreateServiceAccountKeyRequestKeyAlgorithm where
    parseJSON = parseJSONText "CreateServiceAccountKeyRequestKeyAlgorithm"

instance ToJSON CreateServiceAccountKeyRequestKeyAlgorithm where
    toJSON = toJSONText

-- | The current custom role support level.
data PermissionCustomRolesSupportLevel
    = Supported
      -- ^ @SUPPORTED@
      -- Permission is fully supported for custom role use.
    | Testing
      -- ^ @TESTING@
      -- Permission is being tested to check custom role compatibility.
    | NotSupported
      -- ^ @NOT_SUPPORTED@
      -- Permission is not supported for custom role use.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable PermissionCustomRolesSupportLevel

instance FromHttpApiData PermissionCustomRolesSupportLevel where
    parseQueryParam = \case
        "SUPPORTED" -> Right Supported
        "TESTING" -> Right Testing
        "NOT_SUPPORTED" -> Right NotSupported
        x -> Left ("Unable to parse PermissionCustomRolesSupportLevel from: " <> x)

instance ToHttpApiData PermissionCustomRolesSupportLevel where
    toQueryParam = \case
        Supported -> "SUPPORTED"
        Testing -> "TESTING"
        NotSupported -> "NOT_SUPPORTED"

instance FromJSON PermissionCustomRolesSupportLevel where
    parseJSON = parseJSONText "PermissionCustomRolesSupportLevel"

instance ToJSON PermissionCustomRolesSupportLevel where
    toJSON = toJSONText

-- | The output format for the private key. Only provided in
-- \`CreateServiceAccountKey\` responses, not in \`GetServiceAccountKey\`
-- or \`ListServiceAccountKey\` responses. Google never exposes
-- system-managed private keys, and never retains user-managed private
-- keys.
data ServiceAccountKeyPrivateKeyType
    = SAKPKTTypeUnspecified
      -- ^ @TYPE_UNSPECIFIED@
      -- Unspecified. Equivalent to \`TYPE_GOOGLE_CREDENTIALS_FILE\`.
    | SAKPKTTypePKCS12File
      -- ^ @TYPE_PKCS12_FILE@
      -- PKCS12 format. The password for the PKCS12 file is \`notasecret\`. For
      -- more information, see https:\/\/tools.ietf.org\/html\/rfc7292.
    | SAKPKTTypeGoogleCredentialsFile
      -- ^ @TYPE_GOOGLE_CREDENTIALS_FILE@
      -- Google Credentials File format.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable ServiceAccountKeyPrivateKeyType

instance FromHttpApiData ServiceAccountKeyPrivateKeyType where
    parseQueryParam = \case
        "TYPE_UNSPECIFIED" -> Right SAKPKTTypeUnspecified
        "TYPE_PKCS12_FILE" -> Right SAKPKTTypePKCS12File
        "TYPE_GOOGLE_CREDENTIALS_FILE" -> Right SAKPKTTypeGoogleCredentialsFile
        x -> Left ("Unable to parse ServiceAccountKeyPrivateKeyType from: " <> x)

instance ToHttpApiData ServiceAccountKeyPrivateKeyType where
    toQueryParam = \case
        SAKPKTTypeUnspecified -> "TYPE_UNSPECIFIED"
        SAKPKTTypePKCS12File -> "TYPE_PKCS12_FILE"
        SAKPKTTypeGoogleCredentialsFile -> "TYPE_GOOGLE_CREDENTIALS_FILE"

instance FromJSON ServiceAccountKeyPrivateKeyType where
    parseJSON = parseJSONText "ServiceAccountKeyPrivateKeyType"

instance ToJSON ServiceAccountKeyPrivateKeyType where
    toJSON = toJSONText
