{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Container
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The Google Container Engine API is used for building and managing
-- container based applications, powered by the open source Kubernetes
-- technology.
--
-- /See:/ <https://cloud.google.com/container-engine/ Google Container Engine API Reference>
module Network.Google.Container
    (
    -- * Service Configuration
      containerService

    -- * OAuth Scopes
    , cloudPlatformScope

    -- * API Declaration
    , ContainerAPI

    -- * Resources

    -- ** container.projects.zones.clusters.addons
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.Addons

    -- ** container.projects.zones.clusters.completeIpRotation
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.CompleteIPRotation

    -- ** container.projects.zones.clusters.create
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.Create

    -- ** container.projects.zones.clusters.delete
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.Delete

    -- ** container.projects.zones.clusters.get
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.Get

    -- ** container.projects.zones.clusters.legacyAbac
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.LegacyAbac

    -- ** container.projects.zones.clusters.list
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.List

    -- ** container.projects.zones.clusters.locations
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.Locations

    -- ** container.projects.zones.clusters.logging
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.Logging

    -- ** container.projects.zones.clusters.master
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.Master

    -- ** container.projects.zones.clusters.monitoring
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.Monitoring

    -- ** container.projects.zones.clusters.nodePools.autoscaling
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Autoscaling

    -- ** container.projects.zones.clusters.nodePools.create
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Create

    -- ** container.projects.zones.clusters.nodePools.delete
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Delete

    -- ** container.projects.zones.clusters.nodePools.get
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Get

    -- ** container.projects.zones.clusters.nodePools.list
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.List

    -- ** container.projects.zones.clusters.nodePools.rollback
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Rollback

    -- ** container.projects.zones.clusters.nodePools.setManagement
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.SetManagement

    -- ** container.projects.zones.clusters.nodePools.setSize
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.SetSize

    -- ** container.projects.zones.clusters.nodePools.update
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Update

    -- ** container.projects.zones.clusters.resourceLabels
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.ResourceLabels

    -- ** container.projects.zones.clusters.setMaintenancePolicy
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.SetMaintenancePolicy

    -- ** container.projects.zones.clusters.setMasterAuth
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.SetMasterAuth

    -- ** container.projects.zones.clusters.setNetworkPolicy
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.SetNetworkPolicy

    -- ** container.projects.zones.clusters.startIpRotation
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.StartIPRotation

    -- ** container.projects.zones.clusters.update
    , module Network.Google.Resource.Container.Projects.Zones.Clusters.Update

    -- ** container.projects.zones.getServerconfig
    , module Network.Google.Resource.Container.Projects.Zones.GetServerConfig

    -- ** container.projects.zones.operations.cancel
    , module Network.Google.Resource.Container.Projects.Zones.Operations.Cancel

    -- ** container.projects.zones.operations.get
    , module Network.Google.Resource.Container.Projects.Zones.Operations.Get

    -- ** container.projects.zones.operations.list
    , module Network.Google.Resource.Container.Projects.Zones.Operations.List

    -- * Types

    -- ** NetworkPolicyConfig
    , NetworkPolicyConfig
    , networkPolicyConfig
    , npcDisabled

    -- ** UpdateNodePoolRequest
    , UpdateNodePoolRequest
    , updateNodePoolRequest
    , unprImageType
    , unprNodeVersion

    -- ** UpdateMasterRequest
    , UpdateMasterRequest
    , updateMasterRequest
    , umrMasterVersion

    -- ** StartIPRotationRequest
    , StartIPRotationRequest
    , startIPRotationRequest

    -- ** SetLegacyAbacRequest
    , SetLegacyAbacRequest
    , setLegacyAbacRequest
    , slarEnabled

    -- ** HorizontalPodAutoscaling
    , HorizontalPodAutoscaling
    , horizontalPodAutoscaling
    , hpaDisabled

    -- ** SetMasterAuthRequest
    , SetMasterAuthRequest
    , setMasterAuthRequest
    , smarAction
    , smarUpdate

    -- ** ListOperationsResponse
    , ListOperationsResponse
    , listOperationsResponse
    , lorOperations
    , lorMissingZones

    -- ** CreateClusterRequest
    , CreateClusterRequest
    , createClusterRequest
    , ccrCluster

    -- ** Cluster
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

    -- ** CancelOperationRequest
    , CancelOperationRequest
    , cancelOperationRequest

    -- ** UpdateClusterRequest
    , UpdateClusterRequest
    , updateClusterRequest
    , ucrUpdate

    -- ** SetAddonsConfigRequest
    , SetAddonsConfigRequest
    , setAddonsConfigRequest
    , sacrAddonsConfig

    -- ** NodeConfig
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

    -- ** HTTPLoadBalancing
    , HTTPLoadBalancing
    , hTTPLoadBalancing
    , httplbDisabled

    -- ** Operation
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

    -- ** ClusterResourceLabels
    , ClusterResourceLabels
    , clusterResourceLabels
    , crlAddtional

    -- ** Empty
    , Empty
    , empty

    -- ** SetNodePoolAutoscalingRequest
    , SetNodePoolAutoscalingRequest
    , setNodePoolAutoscalingRequest
    , snparAutoscaling

    -- ** CompleteIPRotationRequest
    , CompleteIPRotationRequest
    , completeIPRotationRequest

    -- ** OperationOperationType
    , OperationOperationType (..)

    -- ** NodeManagement
    , NodeManagement
    , nodeManagement
    , nmAutoUpgrade
    , nmAutoRepair
    , nmUpgradeOptions

    -- ** NodePoolAutoscaling
    , NodePoolAutoscaling
    , nodePoolAutoscaling
    , npaMaxNodeCount
    , npaEnabled
    , npaMinNodeCount

    -- ** SetMaintenancePolicyRequest
    , SetMaintenancePolicyRequest
    , setMaintenancePolicyRequest
    , smprMaintenancePolicy

    -- ** KubernetesDashboard
    , KubernetesDashboard
    , kubernetesDashboard
    , kdDisabled

    -- ** ClientCertificateConfig
    , ClientCertificateConfig
    , clientCertificateConfig
    , cccIssueClientCertificate

    -- ** SetLabelsRequest
    , SetLabelsRequest
    , setLabelsRequest
    , slrResourceLabels
    , slrLabelFingerprint

    -- ** OperationStatus
    , OperationStatus (..)

    -- ** MaintenanceWindow
    , MaintenanceWindow
    , maintenanceWindow
    , mwDailyMaintenanceWindow

    -- ** IPAllocationPolicy
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

    -- ** AddonsConfig
    , AddonsConfig
    , addonsConfig
    , acNetworkPolicyConfig
    , acHorizontalPodAutoscaling
    , acHTTPLoadBalancing
    , acKubernetesDashboard

    -- ** NodePool
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

    -- ** SetNodePoolManagementRequest
    , SetNodePoolManagementRequest
    , setNodePoolManagementRequest
    , snpmrManagement

    -- ** MasterAuthorizedNetworksConfig
    , MasterAuthorizedNetworksConfig
    , masterAuthorizedNetworksConfig
    , mancEnabled
    , mancCIdRBlocks

    -- ** LegacyAbac
    , LegacyAbac
    , legacyAbac
    , laEnabled

    -- ** MasterAuth
    , MasterAuth
    , masterAuth
    , maClientKey
    , maUsername
    , maClientCertificateConfig
    , maClientCertificate
    , maPassword
    , maClusterCaCertificate

    -- ** NodeConfigMetadata
    , NodeConfigMetadata
    , nodeConfigMetadata
    , ncmAddtional

    -- ** NodeConfigLabels
    , NodeConfigLabels
    , nodeConfigLabels
    , nclAddtional

    -- ** ServerConfig
    , ServerConfig
    , serverConfig
    , scDefaultImageType
    , scValidNodeVersions
    , scValidImageTypes
    , scDefaultClusterVersion
    , scValidMasterVersions

    -- ** NetworkPolicyProvider
    , NetworkPolicyProvider (..)

    -- ** AutoUpgradeOptions
    , AutoUpgradeOptions
    , autoUpgradeOptions
    , auoAutoUpgradeStartTime
    , auoDescription

    -- ** SetNodePoolSizeRequest
    , SetNodePoolSizeRequest
    , setNodePoolSizeRequest
    , snpsrNodeCount

    -- ** Xgafv
    , Xgafv (..)

    -- ** SetMonitoringServiceRequest
    , SetMonitoringServiceRequest
    , setMonitoringServiceRequest
    , smsrMonitoringService

    -- ** SetLoggingServiceRequest
    , SetLoggingServiceRequest
    , setLoggingServiceRequest
    , slsrLoggingService

    -- ** MaintenancePolicy
    , MaintenancePolicy
    , maintenancePolicy
    , mpWindow

    -- ** CIdRBlock
    , CIdRBlock
    , cIdRBlock
    , cirbCIdRBlock
    , cirbDisplayName

    -- ** AcceleratorConfig
    , AcceleratorConfig
    , acceleratorConfig
    , acAcceleratorCount
    , acAcceleratorType

    -- ** SetLocationsRequest
    , SetLocationsRequest
    , setLocationsRequest
    , slrLocations

    -- ** SetNetworkPolicyRequest
    , SetNetworkPolicyRequest
    , setNetworkPolicyRequest
    , snprNetworkPolicy

    -- ** DailyMaintenanceWindow
    , DailyMaintenanceWindow
    , dailyMaintenanceWindow
    , dmwStartTime
    , dmwDuration

    -- ** NodePoolStatus
    , NodePoolStatus (..)

    -- ** ListClustersResponse
    , ListClustersResponse
    , listClustersResponse
    , lcrClusters
    , lcrMissingZones

    -- ** ClusterUpdate
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

    -- ** RollbackNodePoolUpgradeRequest
    , RollbackNodePoolUpgradeRequest
    , rollbackNodePoolUpgradeRequest

    -- ** NetworkPolicy
    , NetworkPolicy
    , networkPolicy
    , npEnabled
    , npProvider

    -- ** ListNodePoolsResponse
    , ListNodePoolsResponse
    , listNodePoolsResponse
    , lnprNodePools

    -- ** ClusterStatus
    , ClusterStatus (..)

    -- ** CreateNodePoolRequest
    , CreateNodePoolRequest
    , createNodePoolRequest
    , cnprNodePool

    -- ** SetLabelsRequestResourceLabels
    , SetLabelsRequestResourceLabels
    , setLabelsRequestResourceLabels
    , slrrlAddtional

    -- ** SetMasterAuthRequestAction
    , SetMasterAuthRequestAction (..)
    ) where

import Network.Google.Prelude
import Network.Google.Container.Types
import Network.Google.Resource.Container.Projects.Zones.Clusters.Addons
import Network.Google.Resource.Container.Projects.Zones.Clusters.CompleteIPRotation
import Network.Google.Resource.Container.Projects.Zones.Clusters.Create
import Network.Google.Resource.Container.Projects.Zones.Clusters.Delete
import Network.Google.Resource.Container.Projects.Zones.Clusters.Get
import Network.Google.Resource.Container.Projects.Zones.Clusters.LegacyAbac
import Network.Google.Resource.Container.Projects.Zones.Clusters.List
import Network.Google.Resource.Container.Projects.Zones.Clusters.Locations
import Network.Google.Resource.Container.Projects.Zones.Clusters.Logging
import Network.Google.Resource.Container.Projects.Zones.Clusters.Master
import Network.Google.Resource.Container.Projects.Zones.Clusters.Monitoring
import Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Autoscaling
import Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Create
import Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Delete
import Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Get
import Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.List
import Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Rollback
import Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.SetManagement
import Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.SetSize
import Network.Google.Resource.Container.Projects.Zones.Clusters.NodePools.Update
import Network.Google.Resource.Container.Projects.Zones.Clusters.ResourceLabels
import Network.Google.Resource.Container.Projects.Zones.Clusters.SetMaintenancePolicy
import Network.Google.Resource.Container.Projects.Zones.Clusters.SetMasterAuth
import Network.Google.Resource.Container.Projects.Zones.Clusters.SetNetworkPolicy
import Network.Google.Resource.Container.Projects.Zones.Clusters.StartIPRotation
import Network.Google.Resource.Container.Projects.Zones.Clusters.Update
import Network.Google.Resource.Container.Projects.Zones.GetServerConfig
import Network.Google.Resource.Container.Projects.Zones.Operations.Cancel
import Network.Google.Resource.Container.Projects.Zones.Operations.Get
import Network.Google.Resource.Container.Projects.Zones.Operations.List

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Container Engine API service.
type ContainerAPI =
     ProjectsZonesOperationsListResource :<|>
       ProjectsZonesOperationsGetResource
       :<|> ProjectsZonesOperationsCancelResource
       :<|> ProjectsZonesClustersNodePoolsSetSizeResource
       :<|> ProjectsZonesClustersNodePoolsListResource
       :<|>
       ProjectsZonesClustersNodePoolsAutoscalingResource
       :<|> ProjectsZonesClustersNodePoolsGetResource
       :<|> ProjectsZonesClustersNodePoolsRollbackResource
       :<|>
       ProjectsZonesClustersNodePoolsSetManagementResource
       :<|> ProjectsZonesClustersNodePoolsCreateResource
       :<|> ProjectsZonesClustersNodePoolsDeleteResource
       :<|> ProjectsZonesClustersNodePoolsUpdateResource
       :<|> ProjectsZonesClustersSetNetworkPolicyResource
       :<|> ProjectsZonesClustersResourceLabelsResource
       :<|> ProjectsZonesClustersListResource
       :<|> ProjectsZonesClustersStartIPRotationResource
       :<|> ProjectsZonesClustersAddonsResource
       :<|> ProjectsZonesClustersGetResource
       :<|> ProjectsZonesClustersSetMasterAuthResource
       :<|> ProjectsZonesClustersLegacyAbacResource
       :<|> ProjectsZonesClustersMasterResource
       :<|> ProjectsZonesClustersCreateResource
       :<|> ProjectsZonesClustersCompleteIPRotationResource
       :<|> ProjectsZonesClustersMonitoringResource
       :<|>
       ProjectsZonesClustersSetMaintenancePolicyResource
       :<|> ProjectsZonesClustersLoggingResource
       :<|> ProjectsZonesClustersLocationsResource
       :<|> ProjectsZonesClustersDeleteResource
       :<|> ProjectsZonesClustersUpdateResource
       :<|> ProjectsZonesGetServerConfigResource
