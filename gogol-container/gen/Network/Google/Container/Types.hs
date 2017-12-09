{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Container.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Container.Types
    (
    -- * Service Configuration
      containerService

    -- * OAuth Scopes
    , cloudPlatformScope

    -- * NetworkPolicyConfig
    , NetworkPolicyConfig
    , networkPolicyConfig
    , npcDisabled

    -- * UpdateNodePoolRequest
    , UpdateNodePoolRequest
    , updateNodePoolRequest
    , unprImageType
    , unprNodeVersion

    -- * UpdateMasterRequest
    , UpdateMasterRequest
    , updateMasterRequest
    , umrMasterVersion

    -- * StartIPRotationRequest
    , StartIPRotationRequest
    , startIPRotationRequest

    -- * SetLegacyAbacRequest
    , SetLegacyAbacRequest
    , setLegacyAbacRequest
    , slarEnabled

    -- * HorizontalPodAutoscaling
    , HorizontalPodAutoscaling
    , horizontalPodAutoscaling
    , hpaDisabled

    -- * SetMasterAuthRequest
    , SetMasterAuthRequest
    , setMasterAuthRequest
    , smarAction
    , smarUpdate

    -- * ListOperationsResponse
    , ListOperationsResponse
    , listOperationsResponse
    , lorOperations
    , lorMissingZones

    -- * CreateClusterRequest
    , CreateClusterRequest
    , createClusterRequest
    , ccrCluster

    -- * Cluster
    , Cluster
    , cluster
    , cStatus
    , cNodePools
    , cEnableKubernetesAlpha
    , cResourceLabels
    , cNodeConfig
    , cNodeIPv4CIdRSize
    , cClusterIPv4CIdR
    , cInitialNodeCount
    , cCurrentNodeVersion
    , cNetwork
    , cInitialClusterVersion
    , cZone
    , cAddonsConfig
    , cServicesIPv4CIdR
    , cIPAllocationPolicy
    , cMasterAuthorizedNetworksConfig
    , cLegacyAbac
    , cMasterAuth
    , cSelfLink
    , cName
    , cCurrentMasterVersion
    , cStatusMessage
    , cSubnetwork
    , cCurrentNodeCount
    , cMaintenancePolicy
    , cEndpoint
    , cExpireTime
    , cNetworkPolicy
    , cLocations
    , cLoggingService
    , cLabelFingerprint
    , cDescription
    , cInstanceGroupURLs
    , cMonitoringService
    , cCreateTime

    -- * CancelOperationRequest
    , CancelOperationRequest
    , cancelOperationRequest

    -- * UpdateClusterRequest
    , UpdateClusterRequest
    , updateClusterRequest
    , ucrUpdate

    -- * SetAddonsConfigRequest
    , SetAddonsConfigRequest
    , setAddonsConfigRequest
    , sacrAddonsConfig

    -- * NodeConfig
    , NodeConfig
    , nodeConfig
    , ncLocalSsdCount
    , ncDiskSizeGb
    , ncOAuthScopes
    , ncServiceAccount
    , ncAccelerators
    , ncImageType
    , ncMachineType
    , ncMetadata
    , ncLabels
    , ncMinCPUPlatform
    , ncTags
    , ncPreemptible

    -- * HTTPLoadBalancing
    , HTTPLoadBalancing
    , hTTPLoadBalancing
    , httplbDisabled

    -- * Operation
    , Operation
    , operation
    , oStatus
    , oStartTime
    , oZone
    , oSelfLink
    , oName
    , oStatusMessage
    , oEndTime
    , oOperationType
    , oTargetLink
    , oDetail

    -- * ClusterResourceLabels
    , ClusterResourceLabels
    , clusterResourceLabels
    , crlAddtional

    -- * Empty
    , Empty
    , empty

    -- * SetNodePoolAutoscalingRequest
    , SetNodePoolAutoscalingRequest
    , setNodePoolAutoscalingRequest
    , snparAutoscaling

    -- * CompleteIPRotationRequest
    , CompleteIPRotationRequest
    , completeIPRotationRequest

    -- * OperationOperationType
    , OperationOperationType (..)

    -- * NodeManagement
    , NodeManagement
    , nodeManagement
    , nmAutoUpgrade
    , nmAutoRepair
    , nmUpgradeOptions

    -- * NodePoolAutoscaling
    , NodePoolAutoscaling
    , nodePoolAutoscaling
    , npaMaxNodeCount
    , npaEnabled
    , npaMinNodeCount

    -- * SetMaintenancePolicyRequest
    , SetMaintenancePolicyRequest
    , setMaintenancePolicyRequest
    , smprMaintenancePolicy

    -- * KubernetesDashboard
    , KubernetesDashboard
    , kubernetesDashboard
    , kdDisabled

    -- * ClientCertificateConfig
    , ClientCertificateConfig
    , clientCertificateConfig
    , cccIssueClientCertificate

    -- * SetLabelsRequest
    , SetLabelsRequest
    , setLabelsRequest
    , slrResourceLabels
    , slrLabelFingerprint

    -- * OperationStatus
    , OperationStatus (..)

    -- * MaintenanceWindow
    , MaintenanceWindow
    , maintenanceWindow
    , mwDailyMaintenanceWindow

    -- * IPAllocationPolicy
    , IPAllocationPolicy
    , ipAllocationPolicy
    , iapServicesSecondaryRangeName
    , iapNodeIPv4CIdR
    , iapUseIPAliases
    , iapClusterIPv4CIdR
    , iapSubnetworkName
    , iapClusterSecondaryRangeName
    , iapNodeIPv4CIdRBlock
    , iapServicesIPv4CIdR
    , iapClusterIPv4CIdRBlock
    , iapServicesIPv4CIdRBlock
    , iapCreateSubnetwork

    -- * AddonsConfig
    , AddonsConfig
    , addonsConfig
    , acNetworkPolicyConfig
    , acHorizontalPodAutoscaling
    , acHTTPLoadBalancing
    , acKubernetesDashboard

    -- * NodePool
    , NodePool
    , nodePool
    , npStatus
    , npAutoscaling
    , npConfig
    , npInitialNodeCount
    , npManagement
    , npSelfLink
    , npName
    , npStatusMessage
    , npVersion
    , npInstanceGroupURLs

    -- * SetNodePoolManagementRequest
    , SetNodePoolManagementRequest
    , setNodePoolManagementRequest
    , snpmrManagement

    -- * MasterAuthorizedNetworksConfig
    , MasterAuthorizedNetworksConfig
    , masterAuthorizedNetworksConfig
    , mancEnabled
    , mancCIdRBlocks

    -- * LegacyAbac
    , LegacyAbac
    , legacyAbac
    , laEnabled

    -- * MasterAuth
    , MasterAuth
    , masterAuth
    , maClientKey
    , maUsername
    , maClientCertificateConfig
    , maClientCertificate
    , maPassword
    , maClusterCaCertificate

    -- * NodeConfigMetadata
    , NodeConfigMetadata
    , nodeConfigMetadata
    , ncmAddtional

    -- * NodeConfigLabels
    , NodeConfigLabels
    , nodeConfigLabels
    , nclAddtional

    -- * ServerConfig
    , ServerConfig
    , serverConfig
    , scDefaultImageType
    , scValidNodeVersions
    , scValidImageTypes
    , scDefaultClusterVersion
    , scValidMasterVersions

    -- * NetworkPolicyProvider
    , NetworkPolicyProvider (..)

    -- * AutoUpgradeOptions
    , AutoUpgradeOptions
    , autoUpgradeOptions
    , auoAutoUpgradeStartTime
    , auoDescription

    -- * SetNodePoolSizeRequest
    , SetNodePoolSizeRequest
    , setNodePoolSizeRequest
    , snpsrNodeCount

    -- * Xgafv
    , Xgafv (..)

    -- * SetMonitoringServiceRequest
    , SetMonitoringServiceRequest
    , setMonitoringServiceRequest
    , smsrMonitoringService

    -- * SetLoggingServiceRequest
    , SetLoggingServiceRequest
    , setLoggingServiceRequest
    , slsrLoggingService

    -- * MaintenancePolicy
    , MaintenancePolicy
    , maintenancePolicy
    , mpWindow

    -- * CIdRBlock
    , CIdRBlock
    , cIdRBlock
    , cirbCIdRBlock
    , cirbDisplayName

    -- * AcceleratorConfig
    , AcceleratorConfig
    , acceleratorConfig
    , acAcceleratorCount
    , acAcceleratorType

    -- * SetLocationsRequest
    , SetLocationsRequest
    , setLocationsRequest
    , slrLocations

    -- * SetNetworkPolicyRequest
    , SetNetworkPolicyRequest
    , setNetworkPolicyRequest
    , snprNetworkPolicy

    -- * DailyMaintenanceWindow
    , DailyMaintenanceWindow
    , dailyMaintenanceWindow
    , dmwStartTime
    , dmwDuration

    -- * NodePoolStatus
    , NodePoolStatus (..)

    -- * ListClustersResponse
    , ListClustersResponse
    , listClustersResponse
    , lcrClusters
    , lcrMissingZones

    -- * ClusterUpdate
    , ClusterUpdate
    , clusterUpdate
    , cuDesiredNodePoolAutoscaling
    , cuDesiredAddonsConfig
    , cuDesiredNodePoolId
    , cuDesiredMasterAuthorizedNetworksConfig
    , cuDesiredImageType
    , cuDesiredNodeVersion
    , cuDesiredMasterVersion
    , cuDesiredLocations
    , cuDesiredMonitoringService

    -- * RollbackNodePoolUpgradeRequest
    , RollbackNodePoolUpgradeRequest
    , rollbackNodePoolUpgradeRequest

    -- * NetworkPolicy
    , NetworkPolicy
    , networkPolicy
    , npEnabled
    , npProvider

    -- * ListNodePoolsResponse
    , ListNodePoolsResponse
    , listNodePoolsResponse
    , lnprNodePools

    -- * ClusterStatus
    , ClusterStatus (..)

    -- * CreateNodePoolRequest
    , CreateNodePoolRequest
    , createNodePoolRequest
    , cnprNodePool

    -- * SetLabelsRequestResourceLabels
    , SetLabelsRequestResourceLabels
    , setLabelsRequestResourceLabels
    , slrrlAddtional

    -- * SetMasterAuthRequestAction
    , SetMasterAuthRequestAction (..)
    ) where

import Network.Google.Container.Types.Product
import Network.Google.Container.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v1' of the Google Container Engine API. This contains the host and root path used as a starting point for constructing service requests.
containerService :: ServiceConfig
containerService
  = defaultService (ServiceId "container:v1")
      "container.googleapis.com"

-- | View and manage your data across Google Cloud Platform services
cloudPlatformScope :: Proxy '["https://www.googleapis.com/auth/cloud-platform"]
cloudPlatformScope = Proxy;
