{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Compute.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Compute.Types
    (
    -- * Service Configuration
      computeService

    -- * OAuth Scopes
    , computeScope
    , cloudPlatformScope
    , storageReadOnlyScope
    , storageReadWriteScope
    , computeReadOnlyScope
    , storageFullControlScope

    -- * HTTPSHealthCheckListWarningCode
    , HTTPSHealthCheckListWarningCode (..)

    -- * InstanceAggregatedListWarning
    , InstanceAggregatedListWarning
    , instanceAggregatedListWarning
    , ialwData
    , ialwCode
    , ialwMessage

    -- * TargetHTTPSProxyList
    , TargetHTTPSProxyList
    , targetHTTPSProxyList
    , thplNextPageToken
    , thplKind
    , thplItems
    , thplSelfLink
    , thplWarning
    , thplId

    -- * RoutersScopedList
    , RoutersScopedList
    , routersScopedList
    , rslRouters
    , rslWarning

    -- * RouterStatusResponse
    , RouterStatusResponse
    , routerStatusResponse
    , rsrKind
    , rsrResult

    -- * RegionInstanceGroupManagersDeleteInstancesRequest
    , RegionInstanceGroupManagersDeleteInstancesRequest
    , regionInstanceGroupManagersDeleteInstancesRequest
    , rigmdirInstances

    -- * AddressesScopedList
    , AddressesScopedList
    , addressesScopedList
    , aslAddresses
    , aslWarning

    -- * OperationWarningsItemDataItem
    , OperationWarningsItemDataItem
    , operationWarningsItemDataItem
    , owidiValue
    , owidiKey

    -- * SchedulingOnHostMaintenance
    , SchedulingOnHostMaintenance (..)

    -- * RegionInstanceGroupsListInstancesRequest
    , RegionInstanceGroupsListInstancesRequest
    , regionInstanceGroupsListInstancesRequest
    , riglirInstanceState
    , riglirPortName

    -- * BackendServiceAggregatedListWarning
    , BackendServiceAggregatedListWarning
    , backendServiceAggregatedListWarning
    , bsalwData
    , bsalwCode
    , bsalwMessage

    -- * AutoscalingPolicyCustomMetricUtilizationUtilizationTargetType
    , AutoscalingPolicyCustomMetricUtilizationUtilizationTargetType (..)

    -- * BackendServiceListWarningDataItem
    , BackendServiceListWarningDataItem
    , backendServiceListWarningDataItem
    , bslwdiValue
    , bslwdiKey

    -- * FirewallDeniedItem
    , FirewallDeniedItem
    , firewallDeniedItem
    , fdiIPProtocol
    , fdiPorts

    -- * InstanceGroupManagersAbandonInstancesRequest
    , InstanceGroupManagersAbandonInstancesRequest
    , instanceGroupManagersAbandonInstancesRequest
    , igmairInstances

    -- * MachineTypeAggregatedListItems
    , MachineTypeAggregatedListItems
    , machineTypeAggregatedListItems
    , mtaliAddtional

    -- * BackendServiceListWarningCode
    , BackendServiceListWarningCode (..)

    -- * DiskTypeAggregatedListItems
    , DiskTypeAggregatedListItems
    , diskTypeAggregatedListItems
    , dtaliAddtional

    -- * InstancesSetLabelsRequest
    , InstancesSetLabelsRequest
    , instancesSetLabelsRequest
    , islrLabels
    , islrLabelFingerprint

    -- * RouterAggregatedList
    , RouterAggregatedList
    , routerAggregatedList
    , ralNextPageToken
    , ralKind
    , ralItems
    , ralSelfLink
    , ralWarning
    , ralId

    -- * FirewallList
    , FirewallList
    , firewallList
    , flNextPageToken
    , flKind
    , flItems
    , flSelfLink
    , flWarning
    , flId

    -- * InstancesScopedListWarning
    , InstancesScopedListWarning
    , instancesScopedListWarning
    , islwData
    , islwCode
    , islwMessage

    -- * RegionInstanceGroupManagersRecreateRequest
    , RegionInstanceGroupManagersRecreateRequest
    , regionInstanceGroupManagersRecreateRequest
    , rigmrrInstances

    -- * InstanceLabels
    , InstanceLabels
    , instanceLabels
    , ilAddtional

    -- * InstanceListWarningCode
    , InstanceListWarningCode (..)

    -- * BackendServicesScopedListWarning
    , BackendServicesScopedListWarning
    , backendServicesScopedListWarning
    , bsslwData
    , bsslwCode
    , bsslwMessage

    -- * InstanceGroupList
    , InstanceGroupList
    , instanceGroupList
    , iglNextPageToken
    , iglKind
    , iglItems
    , iglSelfLink
    , iglWarning
    , iglId

    -- * InstancesSetMachineTypeRequest
    , InstancesSetMachineTypeRequest
    , instancesSetMachineTypeRequest
    , ismtrMachineType

    -- * CustomerEncryptionKey
    , CustomerEncryptionKey
    , customerEncryptionKey
    , cekSha256
    , cekRawKey

    -- * AutoscalerAggregatedListItems
    , AutoscalerAggregatedListItems
    , autoscalerAggregatedListItems
    , aaliAddtional

    -- * InstanceListWarningDataItem
    , InstanceListWarningDataItem
    , instanceListWarningDataItem
    , ilwdiValue
    , ilwdiKey

    -- * InstanceGroupManagersSetInstanceTemplateRequest
    , InstanceGroupManagersSetInstanceTemplateRequest
    , instanceGroupManagersSetInstanceTemplateRequest
    , igmsitrInstanceTemplate

    -- * DeprecationStatus
    , DeprecationStatus
    , deprecationStatus
    , dsState
    , dsDeleted
    , dsReplacement
    , dsObsolete
    , dsDeprecated

    -- * HTTPSHealthCheckListWarningDataItem
    , HTTPSHealthCheckListWarningDataItem
    , httpsHealthCheckListWarningDataItem
    , hhclwdiValue
    , hhclwdiKey

    -- * OperationWarningsItemCode
    , OperationWarningsItemCode (..)

    -- * Snapshot
    , Snapshot
    , snapshot
    , sStorageBytesStatus
    , sStatus
    , sDiskSizeGb
    , sSourceDiskId
    , sKind
    , sSourceDiskEncryptionKey
    , sStorageBytes
    , sSelfLink
    , sSnapshotEncryptionKey
    , sName
    , sCreationTimestamp
    , sId
    , sLabels
    , sLicenses
    , sSourceDisk
    , sLabelFingerprint
    , sDescription

    -- * RouterStatus
    , RouterStatus
    , routerStatus
    , rsBestRoutesForRouter
    , rsBGPPeerStatus
    , rsNetwork
    , rsBestRoutes

    -- * AutoscalingPolicyCustomMetricUtilization
    , AutoscalingPolicyCustomMetricUtilization
    , autoscalingPolicyCustomMetricUtilization
    , apcmuUtilizationTarget
    , apcmuMetric
    , apcmuUtilizationTargetType

    -- * ForwardingRuleList
    , ForwardingRuleList
    , forwardingRuleList
    , frlNextPageToken
    , frlKind
    , frlItems
    , frlSelfLink
    , frlWarning
    , frlId

    -- * VPNTunnelsScopedList
    , VPNTunnelsScopedList
    , vpnTunnelsScopedList
    , vtslVPNTunnels
    , vtslWarning

    -- * SubnetworkSecondaryRange
    , SubnetworkSecondaryRange
    , subnetworkSecondaryRange
    , ssrRangeName
    , ssrIPCIdRRange

    -- * BackendServiceProtocol
    , BackendServiceProtocol (..)

    -- * RegionInstanceGroupsListInstancesWarning
    , RegionInstanceGroupsListInstancesWarning
    , regionInstanceGroupsListInstancesWarning
    , rigliwData
    , rigliwCode
    , rigliwMessage

    -- * BackendBucketListWarningCode
    , BackendBucketListWarningCode (..)

    -- * AcceleratorTypeAggregatedListWarningDataItem
    , AcceleratorTypeAggregatedListWarningDataItem
    , acceleratorTypeAggregatedListWarningDataItem
    , atalwdiValue
    , atalwdiKey

    -- * InstanceGroupsSetNamedPortsRequest
    , InstanceGroupsSetNamedPortsRequest
    , instanceGroupsSetNamedPortsRequest
    , igsnprFingerprint
    , igsnprNamedPorts

    -- * AcceleratorTypesScopedListWarningCode
    , AcceleratorTypesScopedListWarningCode (..)

    -- * OperationList
    , OperationList
    , operationList
    , olNextPageToken
    , olKind
    , olItems
    , olSelfLink
    , olWarning
    , olId

    -- * DiskList
    , DiskList
    , diskList
    , dlNextPageToken
    , dlKind
    , dlItems
    , dlSelfLink
    , dlWarning
    , dlId

    -- * TargetPoolsAddInstanceRequest
    , TargetPoolsAddInstanceRequest
    , targetPoolsAddInstanceRequest
    , tpairInstances

    -- * RegionAutoscalerList
    , RegionAutoscalerList
    , regionAutoscalerList
    , rNextPageToken
    , rKind
    , rItems
    , rSelfLink
    , rWarning
    , rId

    -- * HealthCheckListWarningDataItem
    , HealthCheckListWarningDataItem
    , healthCheckListWarningDataItem
    , hclwdiValue
    , hclwdiKey

    -- * InstanceGroupsAddInstancesRequest
    , InstanceGroupsAddInstancesRequest
    , instanceGroupsAddInstancesRequest
    , igairInstances

    -- * TargetTCPProxiesSetProxyHeaderRequestProxyHeader
    , TargetTCPProxiesSetProxyHeaderRequestProxyHeader (..)

    -- * InstanceGroupManagerList
    , InstanceGroupManagerList
    , instanceGroupManagerList
    , igmlNextPageToken
    , igmlKind
    , igmlItems
    , igmlSelfLink
    , igmlWarning
    , igmlId

    -- * SubnetworksScopedListWarning
    , SubnetworksScopedListWarning
    , subnetworksScopedListWarning
    , sslwData
    , sslwCode
    , sslwMessage

    -- * AcceleratorTypeListWarning
    , AcceleratorTypeListWarning
    , acceleratorTypeListWarning
    , atlwData
    , atlwCode
    , atlwMessage

    -- * AttachedDiskType
    , AttachedDiskType (..)

    -- * Image
    , Image
    , image
    , iStatus
    , iImageEncryptionKey
    , iSourceImage
    , iDiskSizeGb
    , iSourceType
    , iSourceDiskId
    , iKind
    , iSourceDiskEncryptionKey
    , iGuestOSFeatures
    , iArchiveSizeBytes
    , iFamily
    , iRawDisk
    , iSelfLink
    , iName
    , iSourceImageId
    , iCreationTimestamp
    , iSourceImageEncryptionKey
    , iId
    , iLabels
    , iLicenses
    , iSourceDisk
    , iLabelFingerprint
    , iDescription
    , iDeprecated

    -- * AcceleratorTypeAggregatedListWarningCode
    , AcceleratorTypeAggregatedListWarningCode (..)

    -- * NetworksAddPeeringRequest
    , NetworksAddPeeringRequest
    , networksAddPeeringRequest
    , naprPeerNetwork
    , naprName
    , naprAutoCreateRoutes

    -- * URLMapListWarning
    , URLMapListWarning
    , urlMapListWarning
    , umlwData
    , umlwCode
    , umlwMessage

    -- * URLMap
    , URLMap
    , urlMap
    , umTests
    , umKind
    , umFingerprint
    , umDefaultService
    , umSelfLink
    , umName
    , umCreationTimestamp
    , umPathMatchers
    , umId
    , umHostRules
    , umDescription

    -- * ImageListWarning
    , ImageListWarning
    , imageListWarning
    , ilwData
    , ilwCode
    , ilwMessage

    -- * HealthCheckListWarningCode
    , HealthCheckListWarningCode (..)

    -- * InstanceGroupAggregatedListItems
    , InstanceGroupAggregatedListItems
    , instanceGroupAggregatedListItems
    , igaliAddtional

    -- * TargetPoolList
    , TargetPoolList
    , targetPoolList
    , tplNextPageToken
    , tplKind
    , tplItems
    , tplSelfLink
    , tplWarning
    , tplId

    -- * AcceleratorType
    , AcceleratorType
    , acceleratorType
    , atKind
    , atZone
    , atMaximumCardsPerInstance
    , atSelfLink
    , atName
    , atCreationTimestamp
    , atId
    , atDescription
    , atDeprecated

    -- * BackendBucketListWarningDataItem
    , BackendBucketListWarningDataItem
    , backendBucketListWarningDataItem
    , bblwdiValue
    , bblwdiKey

    -- * AcceleratorTypesScopedListWarningDataItem
    , AcceleratorTypesScopedListWarningDataItem
    , acceleratorTypesScopedListWarningDataItem
    , atslwdiValue
    , atslwdiKey

    -- * InterconnectOutageNotificationSource
    , InterconnectOutageNotificationSource (..)

    -- * TargetInstanceAggregatedList
    , TargetInstanceAggregatedList
    , targetInstanceAggregatedList
    , tialNextPageToken
    , tialKind
    , tialItems
    , tialSelfLink
    , tialWarning
    , tialId

    -- * InterconnectLinkType
    , InterconnectLinkType (..)

    -- * DisksScopedList
    , DisksScopedList
    , disksScopedList
    , dslWarning
    , dslDisks

    -- * InterconnectLocationListWarningDataItem
    , InterconnectLocationListWarningDataItem
    , interconnectLocationListWarningDataItem
    , illwdiValue
    , illwdiKey

    -- * InstanceGroupManagersScopedList
    , InstanceGroupManagersScopedList
    , instanceGroupManagersScopedList
    , igmslWarning
    , igmslInstanceGroupManagers

    -- * SubnetworkListWarning
    , SubnetworkListWarning
    , subnetworkListWarning
    , slwData
    , slwCode
    , slwMessage

    -- * HealthCheck
    , HealthCheck
    , healthCheck
    , hcHealthyThreshold
    , hcTCPHealthCheck
    , hcKind
    , hcSSLHealthCheck
    , hcSelfLink
    , hcCheckIntervalSec
    , hcName
    , hcCreationTimestamp
    , hcHTTPHealthCheck
    , hcId
    , hcType
    , hcTimeoutSec
    , hcDescription
    , hcUnhealthyThreshold
    , hcHTTPSHealthCheck

    -- * TargetSSLProxyProxyHeader
    , TargetSSLProxyProxyHeader (..)

    -- * HTTPHealthCheckListWarning
    , HTTPHealthCheckListWarning
    , hTTPHealthCheckListWarning
    , httphclwData
    , httphclwCode
    , httphclwMessage

    -- * CommitmentAggregatedListItems
    , CommitmentAggregatedListItems
    , commitmentAggregatedListItems
    , caliAddtional

    -- * DiskAggregatedList
    , DiskAggregatedList
    , diskAggregatedList
    , dalNextPageToken
    , dalKind
    , dalItems
    , dalSelfLink
    , dalWarning
    , dalId

    -- * InstanceGroupsListInstancesWarningDataItem
    , InstanceGroupsListInstancesWarningDataItem
    , instanceGroupsListInstancesWarningDataItem
    , igliwdiValue
    , igliwdiKey

    -- * InstanceWithNamedPorts
    , InstanceWithNamedPorts
    , instanceWithNamedPorts
    , iwnpStatus
    , iwnpNamedPorts
    , iwnpInstance

    -- * InterconnectListWarning
    , InterconnectListWarning
    , interconnectListWarning
    , iData
    , iCode
    , iMessage

    -- * ForwardingRulesScopedList
    , ForwardingRulesScopedList
    , forwardingRulesScopedList
    , frslWarning
    , frslForwardingRules

    -- * InstanceReference
    , InstanceReference
    , instanceReference
    , iInstance

    -- * OperationAggregatedList
    , OperationAggregatedList
    , operationAggregatedList
    , oalNextPageToken
    , oalKind
    , oalItems
    , oalSelfLink
    , oalWarning
    , oalId

    -- * OperationsScopedList
    , OperationsScopedList
    , operationsScopedList
    , oslWarning
    , oslOperations

    -- * NamedPort
    , NamedPort
    , namedPort
    , npName
    , npPort

    -- * InterconnectLocationListWarningCode
    , InterconnectLocationListWarningCode (..)

    -- * InstanceGroupsListInstancesWarningCode
    , InstanceGroupsListInstancesWarningCode (..)

    -- * RegionInstanceGroupsListInstancesRequestInstanceState
    , RegionInstanceGroupsListInstancesRequestInstanceState (..)

    -- * SubnetworkAggregatedListWarningDataItem
    , SubnetworkAggregatedListWarningDataItem
    , subnetworkAggregatedListWarningDataItem
    , salwdiValue
    , salwdiKey

    -- * ProjectXpnProjectStatus
    , ProjectXpnProjectStatus (..)

    -- * TargetInstanceList
    , TargetInstanceList
    , targetInstanceList
    , tilNextPageToken
    , tilKind
    , tilItems
    , tilSelfLink
    , tilWarning
    , tilId

    -- * TargetTCPProxyListWarning
    , TargetTCPProxyListWarning
    , targetTCPProxyListWarning
    , ttplwData
    , ttplwCode
    , ttplwMessage

    -- * InstanceGroupManagerAggregatedList
    , InstanceGroupManagerAggregatedList
    , instanceGroupManagerAggregatedList
    , igmalNextPageToken
    , igmalKind
    , igmalItems
    , igmalSelfLink
    , igmalWarning
    , igmalId

    -- * BackendBucket
    , BackendBucket
    , backendBucket
    , bbKind
    , bbEnableCdn
    , bbBucketName
    , bbSelfLink
    , bbName
    , bbCreationTimestamp
    , bbId
    , bbDescription

    -- * ProjectsEnableXpnResourceRequest
    , ProjectsEnableXpnResourceRequest
    , projectsEnableXpnResourceRequest
    , pexrrXpnResource

    -- * ImageSourceType
    , ImageSourceType (..)

    -- * TargetPoolsScopedList
    , TargetPoolsScopedList
    , targetPoolsScopedList
    , tpslWarning
    , tpslTargetPools

    -- * InterconnectAttachmentAggregatedListItems
    , InterconnectAttachmentAggregatedListItems
    , interconnectAttachmentAggregatedListItems
    , iaaliAddtional

    -- * SubnetworkAggregatedListWarningCode
    , SubnetworkAggregatedListWarningCode (..)

    -- * InstancesSetMinCPUPlatformRequest
    , InstancesSetMinCPUPlatformRequest
    , instancesSetMinCPUPlatformRequest
    , ismcprMinCPUPlatform

    -- * ForwardingRuleAggregatedList
    , ForwardingRuleAggregatedList
    , forwardingRuleAggregatedList
    , fralNextPageToken
    , fralKind
    , fralItems
    , fralSelfLink
    , fralWarning
    , fralId

    -- * TargetReference
    , TargetReference
    , targetReference
    , trTarget

    -- * TargetPoolAggregatedList
    , TargetPoolAggregatedList
    , targetPoolAggregatedList
    , tpalNextPageToken
    , tpalKind
    , tpalItems
    , tpalSelfLink
    , tpalWarning
    , tpalId

    -- * OperationsScopedListWarningDataItem
    , OperationsScopedListWarningDataItem
    , operationsScopedListWarningDataItem
    , oslwdiValue
    , oslwdiKey

    -- * BackendServiceSessionAffinity
    , BackendServiceSessionAffinity (..)

    -- * GlobalSetLabelsRequest
    , GlobalSetLabelsRequest
    , globalSetLabelsRequest
    , gslrLabels
    , gslrLabelFingerprint

    -- * TargetPool
    , TargetPool
    , targetPool
    , tpSessionAffinity
    , tpBackupPool
    , tpKind
    , tpSelfLink
    , tpName
    , tpCreationTimestamp
    , tpInstances
    , tpId
    , tpFailoverRatio
    , tpRegion
    , tpDescription
    , tpHealthChecks

    -- * ImageList
    , ImageList
    , imageList
    , ilNextPageToken
    , ilKind
    , ilItems
    , ilSelfLink
    , ilWarning
    , ilId

    -- * OperationAggregatedListWarningDataItem
    , OperationAggregatedListWarningDataItem
    , operationAggregatedListWarningDataItem
    , oalwdiValue
    , oalwdiKey

    -- * ForwardingRuleListWarning
    , ForwardingRuleListWarning
    , forwardingRuleListWarning
    , frlwData
    , frlwCode
    , frlwMessage

    -- * VPNTunnelsScopedListWarning
    , VPNTunnelsScopedListWarning
    , vpnTunnelsScopedListWarning
    , vtslwData
    , vtslwCode
    , vtslwMessage

    -- * ForwardingRulesScopedListWarningCode
    , ForwardingRulesScopedListWarningCode (..)

    -- * OperationsScopedListWarningCode
    , OperationsScopedListWarningCode (..)

    -- * OperationAggregatedListWarningCode
    , OperationAggregatedListWarningCode (..)

    -- * TargetSSLProxiesSetBackendServiceRequest
    , TargetSSLProxiesSetBackendServiceRequest
    , targetSSLProxiesSetBackendServiceRequest
    , tspsbsrService

    -- * ForwardingRule
    , ForwardingRule
    , forwardingRule
    , frIPAddress
    , frLoadBalancingScheme
    , frKind
    , frIPVersion
    , frNetwork
    , frPortRange
    , frSelfLink
    , frName
    , frIPProtocol
    , frCreationTimestamp
    , frSubnetwork
    , frPorts
    , frId
    , frRegion
    , frDescription
    , frTarget
    , frBackendService

    -- * URLMapList
    , URLMapList
    , urlMapList
    , umlNextPageToken
    , umlKind
    , umlItems
    , umlSelfLink
    , umlWarning
    , umlId

    -- * ForwardingRulesScopedListWarningDataItem
    , ForwardingRulesScopedListWarningDataItem
    , forwardingRulesScopedListWarningDataItem
    , frslwdiValue
    , frslwdiKey

    -- * TargetInstanceAggregatedListWarning
    , TargetInstanceAggregatedListWarning
    , targetInstanceAggregatedListWarning
    , tialwData
    , tialwCode
    , tialwMessage

    -- * InstanceGroupManagersScopedListWarningDataItem
    , InstanceGroupManagersScopedListWarningDataItem
    , instanceGroupManagersScopedListWarningDataItem
    , igmslwdiValue
    , igmslwdiKey

    -- * SubnetworksScopedList
    , SubnetworksScopedList
    , subnetworksScopedList
    , sslSubnetworks
    , sslWarning

    -- * DiskAggregatedListWarningCode
    , DiskAggregatedListWarningCode (..)

    -- * AcceleratorTypeList
    , AcceleratorTypeList
    , acceleratorTypeList
    , atlNextPageToken
    , atlKind
    , atlItems
    , atlSelfLink
    , atlWarning
    , atlId

    -- * DiskAggregatedListWarningDataItem
    , DiskAggregatedListWarningDataItem
    , diskAggregatedListWarningDataItem
    , dalwdiValue
    , dalwdiKey

    -- * TargetPoolListWarning
    , TargetPoolListWarning
    , targetPoolListWarning
    , tplwData
    , tplwCode
    , tplwMessage

    -- * TargetPoolAggregatedListWarningDataItem
    , TargetPoolAggregatedListWarningDataItem
    , targetPoolAggregatedListWarningDataItem
    , tpalwdiValue
    , tpalwdiKey

    -- * DisksScopedListWarningCode
    , DisksScopedListWarningCode (..)

    -- * Project
    , Project
    , project
    , pXpnProjectStatus
    , pKind
    , pUsageExportLocation
    , pSelfLink
    , pName
    , pDefaultServiceAccount
    , pCreationTimestamp
    , pEnabledFeatures
    , pQuotas
    , pId
    , pDescription
    , pCommonInstanceMetadata

    -- * RegionInstanceGroupManagersListInstancesResponse
    , RegionInstanceGroupManagersListInstancesResponse
    , regionInstanceGroupManagersListInstancesResponse
    , rigmlirManagedInstances

    -- * Operation
    , Operation
    , operation
    , oTargetId
    , oStatus
    , oInsertTime
    , oProgress
    , oStartTime
    , oKind
    , oError
    , oHTTPErrorMessage
    , oZone
    , oWarnings
    , oHTTPErrorStatusCode
    , oUser
    , oSelfLink
    , oName
    , oStatusMessage
    , oCreationTimestamp
    , oEndTime
    , oId
    , oOperationType
    , oRegion
    , oDescription
    , oTargetLink
    , oClientOperationId

    -- * DisksScopedListWarningDataItem
    , DisksScopedListWarningDataItem
    , disksScopedListWarningDataItem
    , dslwdiValue
    , dslwdiKey

    -- * InstanceGroupManagersScopedListWarningCode
    , InstanceGroupManagersScopedListWarningCode (..)

    -- * Disk
    , Disk
    , disk
    , dStatus
    , dSourceSnapshotId
    , dLastAttachTimestamp
    , dUsers
    , dSourceImage
    , dDiskEncryptionKey
    , dSizeGb
    , dKind
    , dLastDetachTimestamp
    , dZone
    , dSelfLink
    , dName
    , dSourceImageId
    , dCreationTimestamp
    , dSourceImageEncryptionKey
    , dId
    , dLabels
    , dLicenses
    , dOptions
    , dType
    , dLabelFingerprint
    , dDescription
    , dSourceSnapshotEncryptionKey
    , dSourceSnapshot

    -- * DiskMoveRequest
    , DiskMoveRequest
    , diskMoveRequest
    , dmrTargetDisk
    , dmrDestinationZone

    -- * ForwardingRuleAggregatedListWarningCode
    , ForwardingRuleAggregatedListWarningCode (..)

    -- * AutoscalingPolicyLoadBalancingUtilization
    , AutoscalingPolicyLoadBalancingUtilization
    , autoscalingPolicyLoadBalancingUtilization
    , aplbuUtilizationTarget

    -- * TargetPoolAggregatedListWarningCode
    , TargetPoolAggregatedListWarningCode (..)

    -- * ForwardingRuleAggregatedListWarningDataItem
    , ForwardingRuleAggregatedListWarningDataItem
    , forwardingRuleAggregatedListWarningDataItem
    , fralwdiValue
    , fralwdiKey

    -- * TargetPoolsScopedListWarningDataItem
    , TargetPoolsScopedListWarningDataItem
    , targetPoolsScopedListWarningDataItem
    , tpslwdiValue
    , tpslwdiKey

    -- * InstanceGroupManager
    , InstanceGroupManager
    , instanceGroupManager
    , igmKind
    , igmFingerprint
    , igmBaseInstanceName
    , igmZone
    , igmInstanceTemplate
    , igmTargetSize
    , igmSelfLink
    , igmCurrentActions
    , igmName
    , igmCreationTimestamp
    , igmId
    , igmRegion
    , igmTargetPools
    , igmDescription
    , igmInstanceGroup
    , igmNamedPorts

    -- * InstanceGroupManagerListWarning
    , InstanceGroupManagerListWarning
    , instanceGroupManagerListWarning
    , igmlwData
    , igmlwCode
    , igmlwMessage

    -- * RegionInstanceGroupsListInstances
    , RegionInstanceGroupsListInstances
    , regionInstanceGroupsListInstances
    , rigliNextPageToken
    , rigliKind
    , rigliItems
    , rigliSelfLink
    , rigliWarning
    , rigliId

    -- * DiskListWarning
    , DiskListWarning
    , diskListWarning
    , dlwData
    , dlwCode
    , dlwMessage

    -- * TCPHealthCheck
    , TCPHealthCheck
    , tcpHealthCheck
    , thcResponse
    , thcProxyHeader
    , thcPortName
    , thcPort
    , thcRequest

    -- * InstanceGroupManagerAggregatedListWarningDataItem
    , InstanceGroupManagerAggregatedListWarningDataItem
    , instanceGroupManagerAggregatedListWarningDataItem
    , igmalwdiValue
    , igmalwdiKey

    -- * TargetPoolsScopedListWarningCode
    , TargetPoolsScopedListWarningCode (..)

    -- * RegionAutoscalerListWarning
    , RegionAutoscalerListWarning
    , regionAutoscalerListWarning
    , ralwData
    , ralwCode
    , ralwMessage

    -- * TargetInstanceListWarningCode
    , TargetInstanceListWarningCode (..)

    -- * InterconnectLocationRegionInfoLocationPresence
    , InterconnectLocationRegionInfoLocationPresence (..)

    -- * SSLHealthCheckProxyHeader
    , SSLHealthCheckProxyHeader (..)

    -- * TargetVPNGatewayStatus
    , TargetVPNGatewayStatus (..)

    -- * TargetInstanceListWarningDataItem
    , TargetInstanceListWarningDataItem
    , targetInstanceListWarningDataItem
    , tilwdiValue
    , tilwdiKey

    -- * InstanceGroupManagerAggregatedListWarningCode
    , InstanceGroupManagerAggregatedListWarningCode (..)

    -- * OperationListWarning
    , OperationListWarning
    , operationListWarning
    , olwData
    , olwCode
    , olwMessage

    -- * InstanceGroupsRemoveInstancesRequest
    , InstanceGroupsRemoveInstancesRequest
    , instanceGroupsRemoveInstancesRequest
    , igrirInstances

    -- * SnapshotStatus
    , SnapshotStatus (..)

    -- * MachineTypeListWarningDataItem
    , MachineTypeListWarningDataItem
    , machineTypeListWarningDataItem
    , mtlwdiValue
    , mtlwdiKey

    -- * NetworksRemovePeeringRequest
    , NetworksRemovePeeringRequest
    , networksRemovePeeringRequest
    , nrprName

    -- * TargetHTTPProxyListWarningDataItem
    , TargetHTTPProxyListWarningDataItem
    , targetHTTPProxyListWarningDataItem
    , thttpplwdiValue
    , thttpplwdiKey

    -- * MachineTypeAggregatedListWarning
    , MachineTypeAggregatedListWarning
    , machineTypeAggregatedListWarning
    , mtalwData
    , mtalwCode
    , mtalwMessage

    -- * AutoscalerListWarningCode
    , AutoscalerListWarningCode (..)

    -- * BackendBucketList
    , BackendBucketList
    , backendBucketList
    , bblNextPageToken
    , bblKind
    , bblItems
    , bblSelfLink
    , bblWarning
    , bblId

    -- * InterconnectAttachmentPrivateInfo
    , InterconnectAttachmentPrivateInfo
    , interconnectAttachmentPrivateInfo
    , iapiTag8021q

    -- * AcceleratorTypesScopedList
    , AcceleratorTypesScopedList
    , acceleratorTypesScopedList
    , atslAcceleratorTypes
    , atslWarning

    -- * BackendServiceCdnPolicy
    , BackendServiceCdnPolicy
    , backendServiceCdnPolicy
    , bscpCacheKeyPolicy

    -- * DiskTypeListWarningCode
    , DiskTypeListWarningCode (..)

    -- * RouteWarningsItemDataItem
    , RouteWarningsItemDataItem
    , routeWarningsItemDataItem
    , rwidiValue
    , rwidiKey

    -- * TargetInstancesScopedListWarningCode
    , TargetInstancesScopedListWarningCode (..)

    -- * VPNTunnelListWarning
    , VPNTunnelListWarning
    , vpnTunnelListWarning
    , vtlwData
    , vtlwCode
    , vtlwMessage

    -- * BackendServiceAggregatedListItems
    , BackendServiceAggregatedListItems
    , backendServiceAggregatedListItems
    , bsaliAddtional

    -- * VPNTunnelAggregatedListWarningCode
    , VPNTunnelAggregatedListWarningCode (..)

    -- * InstanceAggregatedListItems
    , InstanceAggregatedListItems
    , instanceAggregatedListItems
    , ialiAddtional

    -- * AutoscalersScopedListWarning
    , AutoscalersScopedListWarning
    , autoscalersScopedListWarning
    , aslwData
    , aslwCode
    , aslwMessage

    -- * DiskTypeAggregatedListWarning
    , DiskTypeAggregatedListWarning
    , diskTypeAggregatedListWarning
    , dtalwData
    , dtalwCode
    , dtalwMessage

    -- * DiskTypeListWarningDataItem
    , DiskTypeListWarningDataItem
    , diskTypeListWarningDataItem
    , dtlwdiValue
    , dtlwdiKey

    -- * VPNTunnelAggregatedListWarningDataItem
    , VPNTunnelAggregatedListWarningDataItem
    , vpnTunnelAggregatedListWarningDataItem
    , vtalwdiValue
    , vtalwdiKey

    -- * TargetHTTPProxyListWarningCode
    , TargetHTTPProxyListWarningCode (..)

    -- * AutoscalerAggregatedListWarning
    , AutoscalerAggregatedListWarning
    , autoscalerAggregatedListWarning
    , aalwData
    , aalwCode
    , aalwMessage

    -- * AutoscalerListWarningDataItem
    , AutoscalerListWarningDataItem
    , autoscalerListWarningDataItem
    , alwdiValue
    , alwdiKey

    -- * MachineTypeListWarningCode
    , MachineTypeListWarningCode (..)

    -- * HealthCheckList
    , HealthCheckList
    , healthCheckList
    , hclNextPageToken
    , hclKind
    , hclItems
    , hclSelfLink
    , hclWarning
    , hclId

    -- * ManagedInstanceLastAttemptErrors
    , ManagedInstanceLastAttemptErrors
    , managedInstanceLastAttemptErrors
    , milaeErrors

    -- * GuestOSFeatureType
    , GuestOSFeatureType (..)

    -- * RouteWarningsItemCode
    , RouteWarningsItemCode (..)

    -- * TargetPoolsRemoveInstanceRequest
    , TargetPoolsRemoveInstanceRequest
    , targetPoolsRemoveInstanceRequest
    , tprirInstances

    -- * TargetInstancesScopedListWarningDataItem
    , TargetInstancesScopedListWarningDataItem
    , targetInstancesScopedListWarningDataItem
    , tislwdiValue
    , tislwdiKey

    -- * MachineTypesScopedListWarning
    , MachineTypesScopedListWarning
    , machineTypesScopedListWarning
    , mtslwData
    , mtslwCode
    , mtslwMessage

    -- * ZoneSetLabelsRequest
    , ZoneSetLabelsRequest
    , zoneSetLabelsRequest
    , zslrLabels
    , zslrLabelFingerprint

    -- * TargetInstance
    , TargetInstance
    , targetInstance
    , tiKind
    , tiNATPolicy
    , tiZone
    , tiSelfLink
    , tiName
    , tiCreationTimestamp
    , tiId
    , tiDescription
    , tiInstance

    -- * TargetPoolInstanceHealth
    , TargetPoolInstanceHealth
    , targetPoolInstanceHealth
    , tpihKind
    , tpihHealthStatus

    -- * RegionInstanceGroupManagerListWarningCode
    , RegionInstanceGroupManagerListWarningCode (..)

    -- * AcceleratorTypeAggregatedList
    , AcceleratorTypeAggregatedList
    , acceleratorTypeAggregatedList
    , atalNextPageToken
    , atalKind
    , atalItems
    , atalSelfLink
    , atalWarning
    , atalId

    -- * SnapshotStorageBytesStatus
    , SnapshotStorageBytesStatus (..)

    -- * NetworkRoutingConfig
    , NetworkRoutingConfig
    , networkRoutingConfig
    , nrcRoutingMode

    -- * InstanceGroupManagersListManagedInstancesResponse
    , InstanceGroupManagersListManagedInstancesResponse
    , instanceGroupManagersListManagedInstancesResponse
    , igmlmirManagedInstances

    -- * InstanceProperties
    , InstanceProperties
    , instanceProperties
    , ipServiceAccounts
    , ipNetworkInterfaces
    , ipGuestAccelerators
    , ipMachineType
    , ipMetadata
    , ipLabels
    , ipScheduling
    , ipMinCPUPlatform
    , ipDisks
    , ipCanIPForward
    , ipDescription
    , ipTags

    -- * ProjectsListXpnHostsRequest
    , ProjectsListXpnHostsRequest
    , projectsListXpnHostsRequest
    , plxhrOrganization

    -- * RegionListWarning
    , RegionListWarning
    , regionListWarning
    , rlwData
    , rlwCode
    , rlwMessage

    -- * DiskTypesScopedListWarning
    , DiskTypesScopedListWarning
    , diskTypesScopedListWarning
    , dtslwData
    , dtslwCode
    , dtslwMessage

    -- * RegionInstanceGroupManagerListWarningDataItem
    , RegionInstanceGroupManagerListWarningDataItem
    , regionInstanceGroupManagerListWarningDataItem
    , rigmlwdiValue
    , rigmlwdiKey

    -- * AddressesScopedListWarningCode
    , AddressesScopedListWarningCode (..)

    -- * AttachedDiskInitializeParams
    , AttachedDiskInitializeParams
    , attachedDiskInitializeParams
    , adipSourceImage
    , adipDiskSizeGb
    , adipDiskName
    , adipSourceImageEncryptionKey
    , adipDiskType

    -- * AddressesScopedListWarningDataItem
    , AddressesScopedListWarningDataItem
    , addressesScopedListWarningDataItem
    , aslwdiValue
    , aslwdiKey

    -- * ImageStatus
    , ImageStatus (..)

    -- * InstanceGroupListWarningCode
    , InstanceGroupListWarningCode (..)

    -- * InstanceGroupListWarningDataItem
    , InstanceGroupListWarningDataItem
    , instanceGroupListWarningDataItem
    , iglwdiValue
    , iglwdiKey

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niKind
    , niAliasIPRanges
    , niNetwork
    , niName
    , niNetworkIP
    , niSubnetwork
    , niAccessConfigs

    -- * FirewallListWarningCode
    , FirewallListWarningCode (..)

    -- * RouterListWarning
    , RouterListWarning
    , routerListWarning
    , rData
    , rCode
    , rMessage

    -- * FirewallListWarningDataItem
    , FirewallListWarningDataItem
    , firewallListWarningDataItem
    , flwdiValue
    , flwdiKey

    -- * TargetPoolsRemoveHealthCheckRequest
    , TargetPoolsRemoveHealthCheckRequest
    , targetPoolsRemoveHealthCheckRequest
    , tprhcrHealthChecks

    -- * AutoscalerStatusDetailsType
    , AutoscalerStatusDetailsType (..)

    -- * FirewallDirection
    , FirewallDirection (..)

    -- * RegionInstanceGroupManagersSetTargetPoolsRequest
    , RegionInstanceGroupManagersSetTargetPoolsRequest
    , regionInstanceGroupManagersSetTargetPoolsRequest
    , rigmstprFingerprint
    , rigmstprTargetPools

    -- * TargetSSLProxyList
    , TargetSSLProxyList
    , targetSSLProxyList
    , tsplNextPageToken
    , tsplKind
    , tsplItems
    , tsplSelfLink
    , tsplWarning
    , tsplId

    -- * CustomerEncryptionKeyProtectedDisk
    , CustomerEncryptionKeyProtectedDisk
    , customerEncryptionKeyProtectedDisk
    , cekpdDiskEncryptionKey
    , cekpdSource

    -- * HealthStatusHealthState
    , HealthStatusHealthState (..)

    -- * InstanceTemplateList
    , InstanceTemplateList
    , instanceTemplateList
    , itlNextPageToken
    , itlKind
    , itlItems
    , itlSelfLink
    , itlWarning
    , itlId

    -- * InstanceGroupAggregatedListWarning
    , InstanceGroupAggregatedListWarning
    , instanceGroupAggregatedListWarning
    , igalwData
    , igalwCode
    , igalwMessage

    -- * RouteList
    , RouteList
    , routeList
    , rlNextPageToken
    , rlKind
    , rlItems
    , rlSelfLink
    , rlWarning
    , rlId

    -- * DeprecationStatusState
    , DeprecationStatusState (..)

    -- * Router
    , Router
    , router
    , rouBGPPeers
    , rouBGP
    , rouKind
    , rouNetwork
    , rouInterfaces
    , rouSelfLink
    , rouName
    , rouCreationTimestamp
    , rouId
    , rouRegion
    , rouDescription

    -- * RouterAggregatedListWarningCode
    , RouterAggregatedListWarningCode (..)

    -- * RoutersScopedListWarningCode
    , RoutersScopedListWarningCode (..)

    -- * RouterAggregatedListWarningDataItem
    , RouterAggregatedListWarningDataItem
    , routerAggregatedListWarningDataItem
    , ralwdiValue
    , ralwdiKey

    -- * TargetHTTPSProxyListWarningCode
    , TargetHTTPSProxyListWarningCode (..)

    -- * RoutersScopedListWarningDataItem
    , RoutersScopedListWarningDataItem
    , routersScopedListWarningDataItem
    , rslwdiValue
    , rslwdiKey

    -- * SubnetworksSetPrivateIPGoogleAccessRequest
    , SubnetworksSetPrivateIPGoogleAccessRequest
    , subnetworksSetPrivateIPGoogleAccessRequest
    , sspigarPrivateIPGoogleAccess

    -- * TargetHTTPSProxyListWarningDataItem
    , TargetHTTPSProxyListWarningDataItem
    , targetHTTPSProxyListWarningDataItem
    , thplwdiValue
    , thplwdiKey

    -- * ManagedInstanceCurrentAction
    , ManagedInstanceCurrentAction (..)

    -- * TargetVPNGatewayList
    , TargetVPNGatewayList
    , targetVPNGatewayList
    , tvglNextPageToken
    , tvglKind
    , tvglItems
    , tvglSelfLink
    , tvglWarning
    , tvglId

    -- * TargetInstanceNATPolicy
    , TargetInstanceNATPolicy (..)

    -- * SSLCertificateList
    , SSLCertificateList
    , sslCertificateList
    , sclNextPageToken
    , sclKind
    , sclItems
    , sclSelfLink
    , sclWarning
    , sclId

    -- * FirewallAllowedItem
    , FirewallAllowedItem
    , firewallAllowedItem
    , faiIPProtocol
    , faiPorts

    -- * RouterListWarningDataItem
    , RouterListWarningDataItem
    , routerListWarningDataItem
    , rlwdiValue
    , rlwdiKey

    -- * BackendServiceAggregatedList
    , BackendServiceAggregatedList
    , backendServiceAggregatedList
    , bsalNextPageToken
    , bsalKind
    , bsalItems
    , bsalSelfLink
    , bsalWarning
    , bsalId

    -- * Network
    , Network
    , network
    , nAutoCreateSubnetworks
    , nKind
    , nRoutingConfig
    , nSubnetworks
    , nIPv4Range
    , nSelfLink
    , nName
    , nCreationTimestamp
    , nId
    , nGatewayIPv4
    , nDescription
    , nPeerings

    -- * RoutersScopedListWarning
    , RoutersScopedListWarning
    , routersScopedListWarning
    , rslwData
    , rslwCode
    , rslwMessage

    -- * AccessConfigType
    , AccessConfigType (..)

    -- * TargetHTTPSProxyListWarning
    , TargetHTTPSProxyListWarning
    , targetHTTPSProxyListWarning
    , thplwData
    , thplwCode
    , thplwMessage

    -- * AddressesScopedListWarning
    , AddressesScopedListWarning
    , addressesScopedListWarning
    , aData
    , aCode
    , aMessage

    -- * InstanceGroupAggregatedListWarningDataItem
    , InstanceGroupAggregatedListWarningDataItem
    , instanceGroupAggregatedListWarningDataItem
    , igalwdiValue
    , igalwdiKey

    -- * ForwardingRuleIPVersion
    , ForwardingRuleIPVersion (..)

    -- * RouterListWarningCode
    , RouterListWarningCode (..)

    -- * ImageRawDisk
    , ImageRawDisk
    , imageRawDisk
    , irdContainerType
    , irdSource
    , irdSha1Checksum

    -- * InstanceAggregatedList
    , InstanceAggregatedList
    , instanceAggregatedList
    , ialNextPageToken
    , ialKind
    , ialItems
    , ialSelfLink
    , ialWarning
    , ialId

    -- * TargetTCPProxiesSetBackendServiceRequest
    , TargetTCPProxiesSetBackendServiceRequest
    , targetTCPProxiesSetBackendServiceRequest
    , ttpsbsrService

    -- * SSLHealthCheck
    , SSLHealthCheck
    , sslHealthCheck
    , shcResponse
    , shcProxyHeader
    , shcPortName
    , shcPort
    , shcRequest

    -- * Address
    , Address
    , address
    , aStatus
    , aUsers
    , aKind
    , aIPVersion
    , aAddress
    , aSelfLink
    , aName
    , aCreationTimestamp
    , aSubnetwork
    , aId
    , aRegion
    , aDescription
    , aAddressType

    -- * InstanceGroupAggregatedListWarningCode
    , InstanceGroupAggregatedListWarningCode (..)

    -- * InstanceGroupListWarning
    , InstanceGroupListWarning
    , instanceGroupListWarning
    , iglwData
    , iglwCode
    , iglwMessage

    -- * Zone
    , Zone
    , zone
    , zStatus
    , zKind
    , zSelfLink
    , zName
    , zCreationTimestamp
    , zId
    , zRegion
    , zAvailableCPUPlatforms
    , zDescription
    , zDeprecated

    -- * RouterBGP
    , RouterBGP
    , routerBGP
    , rbASN

    -- * FirewallListWarning
    , FirewallListWarning
    , firewallListWarning
    , flwData
    , flwCode
    , flwMessage

    -- * BackendServicesScopedList
    , BackendServicesScopedList
    , backendServicesScopedList
    , bsslWarning
    , bsslBackendServices

    -- * InstanceGroupManagersRecreateInstancesRequest
    , InstanceGroupManagersRecreateInstancesRequest
    , instanceGroupManagersRecreateInstancesRequest
    , igmrirInstances

    -- * TargetSSLProxiesSetSSLCertificatesRequest
    , TargetSSLProxiesSetSSLCertificatesRequest
    , targetSSLProxiesSetSSLCertificatesRequest
    , tspsscrSSLCertificates

    -- * RouterAggregatedListWarning
    , RouterAggregatedListWarning
    , routerAggregatedListWarning
    , rouData
    , rouCode
    , rouMessage

    -- * InstancesScopedList
    , InstancesScopedList
    , instancesScopedList
    , islWarning
    , islInstances

    -- * AcceleratorTypeAggregatedListItems
    , AcceleratorTypeAggregatedListItems
    , acceleratorTypeAggregatedListItems
    , ataliAddtional

    -- * TargetVPNGatewayAggregatedListWarningDataItem
    , TargetVPNGatewayAggregatedListWarningDataItem
    , targetVPNGatewayAggregatedListWarningDataItem
    , tvgalwdiValue
    , tvgalwdiKey

    -- * BackendServiceLoadBalancingScheme
    , BackendServiceLoadBalancingScheme (..)

    -- * HealthCheckReference
    , HealthCheckReference
    , healthCheckReference
    , hcrHealthCheck

    -- * TargetInstanceAggregatedListItems
    , TargetInstanceAggregatedListItems
    , targetInstanceAggregatedListItems
    , tialiAddtional

    -- * TargetSSLProxyListWarning
    , TargetSSLProxyListWarning
    , targetSSLProxyListWarning
    , tsplwData
    , tsplwCode
    , tsplwMessage

    -- * InstanceTemplateListWarning
    , InstanceTemplateListWarning
    , instanceTemplateListWarning
    , itlwData
    , itlwCode
    , itlwMessage

    -- * RouteListWarning
    , RouteListWarning
    , routeListWarning
    , rlwlData
    , rlwlCode
    , rlwlMessage

    -- * TargetTCPProxyProxyHeader
    , TargetTCPProxyProxyHeader (..)

    -- * InstanceGroupAggregatedList
    , InstanceGroupAggregatedList
    , instanceGroupAggregatedList
    , igalNextPageToken
    , igalKind
    , igalItems
    , igalSelfLink
    , igalWarning
    , igalId

    -- * TargetVPNGatewayAggregatedListWarningCode
    , TargetVPNGatewayAggregatedListWarningCode (..)

    -- * OperationStatus
    , OperationStatus (..)

    -- * Route
    , Route
    , route
    , rrNextHopPeering
    , rrPriority
    , rrKind
    , rrNextHopGateway
    , rrNextHopNetwork
    , rrNetwork
    , rrWarnings
    , rrNextHopIP
    , rrDestRange
    , rrSelfLink
    , rrName
    , rrCreationTimestamp
    , rrId
    , rrNextHopVPNTunnel
    , rrDescription
    , rrTags
    , rrNextHopInstance

    -- * TargetVPNGatewaysScopedListWarningDataItem
    , TargetVPNGatewaysScopedListWarningDataItem
    , targetVPNGatewaysScopedListWarningDataItem
    , tvgslwdiValue
    , tvgslwdiKey

    -- * InterconnectLocationRegionInfo
    , InterconnectLocationRegionInfo
    , interconnectLocationRegionInfo
    , ilriLocationPresence
    , ilriExpectedRttMs
    , ilriRegion

    -- * TargetVPNGatewaysScopedListWarningCode
    , TargetVPNGatewaysScopedListWarningCode (..)

    -- * TargetHTTPSProxiesSetSSLCertificatesRequest
    , TargetHTTPSProxiesSetSSLCertificatesRequest
    , targetHTTPSProxiesSetSSLCertificatesRequest
    , thpsscrSSLCertificates

    -- * InstanceTemplate
    , InstanceTemplate
    , instanceTemplate
    , itKind
    , itSelfLink
    , itName
    , itCreationTimestamp
    , itId
    , itDescription
    , itProperties

    -- * XpnResourceId
    , XpnResourceId
    , xpnResourceId
    , xriId
    , xriType

    -- * RouterList
    , RouterList
    , routerList
    , rllNextPageToken
    , rllKind
    , rllItems
    , rllSelfLink
    , rllWarning
    , rllId

    -- * TargetSSLProxy
    , TargetSSLProxy
    , targetSSLProxy
    , tspSSLCertificates
    , tspService
    , tspKind
    , tspSelfLink
    , tspName
    , tspCreationTimestamp
    , tspId
    , tspProxyHeader
    , tspDescription

    -- * SSLCertificateListWarning
    , SSLCertificateListWarning
    , sslCertificateListWarning
    , sclwData
    , sclwCode
    , sclwMessage

    -- * SnapshotListWarningDataItem
    , SnapshotListWarningDataItem
    , snapshotListWarningDataItem
    , slwdiValue
    , slwdiKey

    -- * TargetVPNGateway
    , TargetVPNGateway
    , targetVPNGateway
    , tvgStatus
    , tvgKind
    , tvgNetwork
    , tvgSelfLink
    , tvgName
    , tvgCreationTimestamp
    , tvgId
    , tvgRegion
    , tvgTunnels
    , tvgDescription
    , tvgForwardingRules

    -- * DiskStatus
    , DiskStatus (..)

    -- * ResourceCommitment
    , ResourceCommitment
    , resourceCommitment
    , rcAmount
    , rcType

    -- * SnapshotListWarningCode
    , SnapshotListWarningCode (..)

    -- * BackendServiceIAP
    , BackendServiceIAP
    , backendServiceIAP
    , bsiapEnabled
    , bsiapOAuth2ClientSecretSha256
    , bsiapOAuth2ClientSecret
    , bsiapOAuth2ClientId

    -- * TargetVPNGatewayListWarning
    , TargetVPNGatewayListWarning
    , targetVPNGatewayListWarning
    , tvglwData
    , tvglwCode
    , tvglwMessage

    -- * ManagedInstanceInstanceStatus
    , ManagedInstanceInstanceStatus (..)

    -- * HTTPHealthCheckProxyHeader
    , HTTPHealthCheckProxyHeader (..)

    -- * InterconnectLocationContinent
    , InterconnectLocationContinent (..)

    -- * URLMapsValidateResponse
    , URLMapsValidateResponse
    , urlMapsValidateResponse
    , umvrResult

    -- * SSLCertificate
    , SSLCertificate
    , sslCertificate
    , scPrivateKey
    , scKind
    , scSelfLink
    , scName
    , scCreationTimestamp
    , scId
    , scCertificate
    , scDescription

    -- * RouterStatusBGPPeerStatus
    , RouterStatusBGPPeerStatus
    , routerStatusBGPPeerStatus
    , rsbpsStatus
    , rsbpsIPAddress
    , rsbpsState
    , rsbpsPeerIPAddress
    , rsbpsUptime
    , rsbpsNumLearnedRoutes
    , rsbpsName
    , rsbpsUptimeSeconds
    , rsbpsAdvertisedRoutes
    , rsbpsLinkedVPNTunnel

    -- * URLMapReference
    , URLMapReference
    , urlMapReference
    , umrURLMap

    -- * AttachedDiskMode
    , AttachedDiskMode (..)

    -- * TargetPoolsAddHealthCheckRequest
    , TargetPoolsAddHealthCheckRequest
    , targetPoolsAddHealthCheckRequest
    , tpahcrHealthChecks

    -- * CommitmentsScopedList
    , CommitmentsScopedList
    , commitmentsScopedList
    , cslWarning
    , cslCommitments

    -- * TargetVPNGatewayListWarningCode
    , TargetVPNGatewayListWarningCode (..)

    -- * DiskAggregatedListItems
    , DiskAggregatedListItems
    , diskAggregatedListItems
    , daliAddtional

    -- * TargetVPNGatewayAggregatedListWarning
    , TargetVPNGatewayAggregatedListWarning
    , targetVPNGatewayAggregatedListWarning
    , tvgalwData
    , tvgalwCode
    , tvgalwMessage

    -- * UsageExportLocation
    , UsageExportLocation
    , usageExportLocation
    , uelReportNamePrefix
    , uelBucketName

    -- * InstanceTemplateListWarningCode
    , InstanceTemplateListWarningCode (..)

    -- * ZoneList
    , ZoneList
    , zoneList
    , zlNextPageToken
    , zlKind
    , zlItems
    , zlSelfLink
    , zlWarning
    , zlId

    -- * SSLCertificateListWarningDataItem
    , SSLCertificateListWarningDataItem
    , sslCertificateListWarningDataItem
    , sclwdiValue
    , sclwdiKey

    -- * TargetSSLProxyListWarningCode
    , TargetSSLProxyListWarningCode (..)

    -- * RegionStatus
    , RegionStatus (..)

    -- * TargetTCPProxiesSetProxyHeaderRequest
    , TargetTCPProxiesSetProxyHeaderRequest
    , targetTCPProxiesSetProxyHeaderRequest
    , ttpsphrProxyHeader

    -- * RouterBGPPeer
    , RouterBGPPeer
    , routerBGPPeer
    , rbpIPAddress
    , rbpInterfaceName
    , rbpPeerIPAddress
    , rbpAdvertisedRoutePriority
    , rbpPeerASN
    , rbpName

    -- * SubnetworksExpandIPCIdRRangeRequest
    , SubnetworksExpandIPCIdRRangeRequest
    , subnetworksExpandIPCIdRRangeRequest
    , seicirrrIPCIdRRange

    -- * ManagedInstance
    , ManagedInstance
    , managedInstance
    , miLastAttempt
    , miCurrentAction
    , miId
    , miInstanceStatus
    , miInstance

    -- * InstanceGroupManagerAggregatedListItems
    , InstanceGroupManagerAggregatedListItems
    , instanceGroupManagerAggregatedListItems
    , igmaliAddtional

    -- * InstanceGroupManagersDeleteInstancesRequest
    , InstanceGroupManagersDeleteInstancesRequest
    , instanceGroupManagersDeleteInstancesRequest
    , igmdirInstances

    -- * Backend
    , Backend
    , backend
    , bGroup
    , bBalancingMode
    , bMaxUtilization
    , bMaxRate
    , bMaxConnections
    , bMaxConnectionsPerInstance
    , bMaxRatePerInstance
    , bDescription
    , bCapacityScaler

    -- * TargetVPNGatewaysScopedListWarning
    , TargetVPNGatewaysScopedListWarning
    , targetVPNGatewaysScopedListWarning
    , tvgslwData
    , tvgslwCode
    , tvgslwMessage

    -- * TargetSSLProxiesSetProxyHeaderRequestProxyHeader
    , TargetSSLProxiesSetProxyHeaderRequestProxyHeader (..)

    -- * AddressList
    , AddressList
    , addressList
    , alNextPageToken
    , alKind
    , alItems
    , alSelfLink
    , alWarning
    , alId

    -- * TargetVPNGatewayListWarningDataItem
    , TargetVPNGatewayListWarningDataItem
    , targetVPNGatewayListWarningDataItem
    , tvglwdiValue
    , tvglwdiKey

    -- * ForwardingRuleAggregatedListItems
    , ForwardingRuleAggregatedListItems
    , forwardingRuleAggregatedListItems
    , fraliAddtional

    -- * InterconnectAttachmentAggregatedList
    , InterconnectAttachmentAggregatedList
    , interconnectAttachmentAggregatedList
    , iaalNextPageToken
    , iaalKind
    , iaalItems
    , iaalSelfLink
    , iaalWarning
    , iaalId

    -- * RouteListWarningCode
    , RouteListWarningCode (..)

    -- * OperationAggregatedListItems
    , OperationAggregatedListItems
    , operationAggregatedListItems
    , oaliAddtional

    -- * InstanceGroupManagerActionsSummary
    , InstanceGroupManagerActionsSummary
    , instanceGroupManagerActionsSummary
    , igmasDeleting
    , igmasRestarting
    , igmasNone
    , igmasCreating
    , igmasRefreshing
    , igmasCreatingWithoutRetries
    , igmasRecreating
    , igmasAbandoning

    -- * XpnHostList
    , XpnHostList
    , xpnHostList
    , xhlNextPageToken
    , xhlKind
    , xhlItems
    , xhlSelfLink
    , xhlWarning
    , xhlId

    -- * VPNTunnelStatus
    , VPNTunnelStatus (..)

    -- * InstanceTemplateListWarningDataItem
    , InstanceTemplateListWarningDataItem
    , instanceTemplateListWarningDataItem
    , itlwdiValue
    , itlwdiKey

    -- * SSLCertificateListWarningCode
    , SSLCertificateListWarningCode (..)

    -- * ServiceAccount
    , ServiceAccount
    , serviceAccount
    , saEmail
    , saScopes

    -- * SnapshotListWarning
    , SnapshotListWarning
    , snapshotListWarning
    , sData
    , sCode
    , sMessage

    -- * RegionInstanceGroupManagersAbandonInstancesRequest
    , RegionInstanceGroupManagersAbandonInstancesRequest
    , regionInstanceGroupManagersAbandonInstancesRequest
    , rigmairInstances

    -- * NetworkList
    , NetworkList
    , networkList
    , nlNextPageToken
    , nlKind
    , nlItems
    , nlSelfLink
    , nlWarning
    , nlId

    -- * NetworkPeering
    , NetworkPeering
    , networkPeering
    , netState
    , netStateDetails
    , netNetwork
    , netName
    , netAutoCreateRoutes

    -- * TargetSSLProxyListWarningDataItem
    , TargetSSLProxyListWarningDataItem
    , targetSSLProxyListWarningDataItem
    , tsplwdiValue
    , tsplwdiKey

    -- * InstanceGroupsListInstancesRequest
    , InstanceGroupsListInstancesRequest
    , instanceGroupsListInstancesRequest
    , iglirInstanceState

    -- * RouteListWarningDataItem
    , RouteListWarningDataItem
    , routeListWarningDataItem
    , rValue
    , rKey

    -- * InterconnectAttachmentsScopedList
    , InterconnectAttachmentsScopedList
    , interconnectAttachmentsScopedList
    , iaslWarning
    , iaslInterconnectAttachments

    -- * BackendBalancingMode
    , BackendBalancingMode (..)

    -- * CommitmentAggregatedList
    , CommitmentAggregatedList
    , commitmentAggregatedList
    , calNextPageToken
    , calKind
    , calItems
    , calSelfLink
    , calWarning
    , calId

    -- * RegionInstanceGroupList
    , RegionInstanceGroupList
    , regionInstanceGroupList
    , riglNextPageToken
    , riglKind
    , riglItems
    , riglSelfLink
    , riglWarning
    , riglId

    -- * TargetPoolAggregatedListItems
    , TargetPoolAggregatedListItems
    , targetPoolAggregatedListItems
    , tpaliAddtional

    -- * TargetInstancesScopedList
    , TargetInstancesScopedList
    , targetInstancesScopedList
    , tislWarning
    , tislTargetInstances

    -- * NetworkRoutingConfigRoutingMode
    , NetworkRoutingConfigRoutingMode (..)

    -- * SubnetworkListWarningCode
    , SubnetworkListWarningCode (..)

    -- * SubnetworkListWarningDataItem
    , SubnetworkListWarningDataItem
    , subnetworkListWarningDataItem
    , sValue
    , sKey

    -- * ProjectsDisableXpnResourceRequest
    , ProjectsDisableXpnResourceRequest
    , projectsDisableXpnResourceRequest
    , pdxrrXpnResource

    -- * CommitmentStatus
    , CommitmentStatus (..)

    -- * AddressAggregatedListItems
    , AddressAggregatedListItems
    , addressAggregatedListItems
    , aAddtional

    -- * InterconnectListWarningCode
    , InterconnectListWarningCode (..)

    -- * AutoscalerList
    , AutoscalerList
    , autoscalerList
    , autNextPageToken
    , autKind
    , autItems
    , autSelfLink
    , autWarning
    , autId

    -- * InterconnectListWarningDataItem
    , InterconnectListWarningDataItem
    , interconnectListWarningDataItem
    , iValue
    , iKey

    -- * SubnetworkAggregatedListWarning
    , SubnetworkAggregatedListWarning
    , subnetworkAggregatedListWarning
    , salwData
    , salwCode
    , salwMessage

    -- * InterconnectLocation
    , InterconnectLocation
    , interconnectLocation
    , intFacilityProviderFacilityId
    , intRegionInfos
    , intKind
    , intAddress
    , intFacilityProvider
    , intSelfLink
    , intPeeringdbFacilityId
    , intName
    , intCity
    , intAvailabilityZone
    , intCreationTimestamp
    , intId
    , intContinent
    , intDescription

    -- * TargetSSLProxiesSetProxyHeaderRequest
    , TargetSSLProxiesSetProxyHeaderRequest
    , targetSSLProxiesSetProxyHeaderRequest
    , tspsphrProxyHeader

    -- * HTTPHealthCheckListWarningCode
    , HTTPHealthCheckListWarningCode (..)

    -- * VPNTunnelAggregatedList
    , VPNTunnelAggregatedList
    , vpnTunnelAggregatedList
    , vtalNextPageToken
    , vtalKind
    , vtalItems
    , vtalSelfLink
    , vtalWarning
    , vtalId

    -- * InterconnectCircuitInfo
    , InterconnectCircuitInfo
    , interconnectCircuitInfo
    , iciGoogleCircuitId
    , iciCustomerDemarcId
    , iciGoogleDemarcId

    -- * AttachedDisk
    , AttachedDisk
    , attachedDisk
    , adDiskEncryptionKey
    , adKind
    , adMode
    , adBoot
    , adAutoDelete
    , adInitializeParams
    , adDeviceName
    , adInterface
    , adSource
    , adLicenses
    , adType
    , adIndex

    -- * HTTPHealthCheckListWarningDataItem
    , HTTPHealthCheckListWarningDataItem
    , hTTPHealthCheckListWarningDataItem
    , httphclwdiValue
    , httphclwdiKey

    -- * InterconnectAttachmentOperationalStatus
    , InterconnectAttachmentOperationalStatus (..)

    -- * DiskTypeList
    , DiskTypeList
    , diskTypeList
    , dtlNextPageToken
    , dtlKind
    , dtlItems
    , dtlSelfLink
    , dtlWarning
    , dtlId

    -- * TargetTCPProxyListWarningCode
    , TargetTCPProxyListWarningCode (..)

    -- * RegionInstanceGroupsSetNamedPortsRequest
    , RegionInstanceGroupsSetNamedPortsRequest
    , regionInstanceGroupsSetNamedPortsRequest
    , rigsnprFingerprint
    , rigsnprNamedPorts

    -- * TargetTCPProxyListWarningDataItem
    , TargetTCPProxyListWarningDataItem
    , targetTCPProxyListWarningDataItem
    , ttplwdiValue
    , ttplwdiKey

    -- * ProjectsGetXpnResources
    , ProjectsGetXpnResources
    , projectsGetXpnResources
    , pgxrNextPageToken
    , pgxrKind
    , pgxrResources

    -- * MachineTypeList
    , MachineTypeList
    , machineTypeList
    , mtlNextPageToken
    , mtlKind
    , mtlItems
    , mtlSelfLink
    , mtlWarning
    , mtlId

    -- * TargetHTTPProxyList
    , TargetHTTPProxyList
    , targetHTTPProxyList
    , thttpplNextPageToken
    , thttpplKind
    , thttpplItems
    , thttpplSelfLink
    , thttpplWarning
    , thttpplId

    -- * InterconnectLocationListWarning
    , InterconnectLocationListWarning
    , interconnectLocationListWarning
    , illwData
    , illwCode
    , illwMessage

    -- * InstanceGroupsListInstancesWarning
    , InstanceGroupsListInstancesWarning
    , instanceGroupsListInstancesWarning
    , igliwData
    , igliwCode
    , igliwMessage

    -- * RegionInstanceGroupManagerList
    , RegionInstanceGroupManagerList
    , regionInstanceGroupManagerList
    , rigmlNextPageToken
    , rigmlKind
    , rigmlItems
    , rigmlSelfLink
    , rigmlWarning
    , rigmlId

    -- * ForwardingRuleIPProtocol
    , ForwardingRuleIPProtocol (..)

    -- * DiskTypesScopedList
    , DiskTypesScopedList
    , diskTypesScopedList
    , dtslDiskTypes
    , dtslWarning

    -- * ImageListWarningCode
    , ImageListWarningCode (..)

    -- * AddressStatus
    , AddressStatus (..)

    -- * ImageListWarningDataItem
    , ImageListWarningDataItem
    , imageListWarningDataItem
    , imaValue
    , imaKey

    -- * AcceleratorTypeListWarningDataItem
    , AcceleratorTypeListWarningDataItem
    , acceleratorTypeListWarningDataItem
    , atlwdiValue
    , atlwdiKey

    -- * InterconnectOutageNotification
    , InterconnectOutageNotification
    , interconnectOutageNotification
    , ionState
    , ionAffectedCircuits
    , ionStartTime
    , ionIssueType
    , ionName
    , ionEndTime
    , ionSource
    , ionDescription

    -- * AcceleratorTypeListWarningCode
    , AcceleratorTypeListWarningCode (..)

    -- * DiskTypeAggregatedList
    , DiskTypeAggregatedList
    , diskTypeAggregatedList
    , dtalNextPageToken
    , dtalKind
    , dtalItems
    , dtalSelfLink
    , dtalWarning
    , dtalId

    -- * HTTPHealthCheck
    , HTTPHealthCheck
    , hTTPHealthCheck
    , httphcRequestPath
    , httphcHost
    , httphcProxyHeader
    , httphcPortName
    , httphcPort

    -- * URLMapListWarningDataItem
    , URLMapListWarningDataItem
    , urlMapListWarningDataItem
    , umlwdiValue
    , umlwdiKey

    -- * BackendServiceGroupHealth
    , BackendServiceGroupHealth
    , backendServiceGroupHealth
    , bsghKind
    , bsghHealthStatus

    -- * URLMapListWarningCode
    , URLMapListWarningCode (..)

    -- * InstanceGroupsListInstancesRequestInstanceState
    , InstanceGroupsListInstancesRequestInstanceState (..)

    -- * AutoscalersScopedList
    , AutoscalersScopedList
    , autoscalersScopedList
    , aAutoscalers
    , aWarning

    -- * AutoscalerAggregatedList
    , AutoscalerAggregatedList
    , autoscalerAggregatedList
    , aalNextPageToken
    , aalKind
    , aalItems
    , aalSelfLink
    , aalWarning
    , aalId

    -- * RouterAggregatedListItems
    , RouterAggregatedListItems
    , routerAggregatedListItems
    , raliAddtional

    -- * AcceleratorTypesScopedListWarning
    , AcceleratorTypesScopedListWarning
    , acceleratorTypesScopedListWarning
    , atslwData
    , atslwCode
    , atslwMessage

    -- * TargetTCPProxy
    , TargetTCPProxy
    , targetTCPProxy
    , ttpService
    , ttpKind
    , ttpSelfLink
    , ttpName
    , ttpCreationTimestamp
    , ttpId
    , ttpProxyHeader
    , ttpDescription

    -- * BackendBucketListWarning
    , BackendBucketListWarning
    , backendBucketListWarning
    , bblwData
    , bblwCode
    , bblwMessage

    -- * ImageLabels
    , ImageLabels
    , imageLabels
    , iAddtional

    -- * HTTPSHealthCheckProxyHeader
    , HTTPSHealthCheckProxyHeader (..)

    -- * AutoscalingPolicy
    , AutoscalingPolicy
    , autoscalingPolicy
    , apCustomMetricUtilizations
    , apMaxNumReplicas
    , apCPUUtilization
    , apLoadBalancingUtilization
    , apMinNumReplicas
    , apCoolDownPeriodSec

    -- * RegionList
    , RegionList
    , regionList
    , regNextPageToken
    , regKind
    , regItems
    , regSelfLink
    , regWarning
    , regId

    -- * AttachedDiskInterface
    , AttachedDiskInterface (..)

    -- * HealthCheckType
    , HealthCheckType (..)

    -- * RegionInstanceGroupsListInstancesWarningCode
    , RegionInstanceGroupsListInstancesWarningCode (..)

    -- * ZoneStatus
    , ZoneStatus (..)

    -- * VPNTunnelList
    , VPNTunnelList
    , vpnTunnelList
    , vtlNextPageToken
    , vtlKind
    , vtlItems
    , vtlSelfLink
    , vtlWarning
    , vtlId

    -- * AcceleratorTypeAggregatedListWarning
    , AcceleratorTypeAggregatedListWarning
    , acceleratorTypeAggregatedListWarning
    , atalwData
    , atalwCode
    , atalwMessage

    -- * RegionInstanceGroupsListInstancesWarningDataItem
    , RegionInstanceGroupsListInstancesWarningDataItem
    , regionInstanceGroupsListInstancesWarningDataItem
    , rigliwdiValue
    , rigliwdiKey

    -- * Interconnect
    , Interconnect
    , interconnect
    , iiInterconnectType
    , iiLocation
    , iiPeerIPAddress
    , iiKind
    , iiExpectedOutages
    , iiProvisionedLinkCount
    , iiCustomerName
    , iiRequestedLinkCount
    , iiOperationalStatus
    , iiSelfLink
    , iiName
    , iiGoogleReferenceId
    , iiCreationTimestamp
    , iiAdminEnabled
    , iiId
    , iiInterconnectAttachments
    , iiLinkType
    , iiGoogleIPAddress
    , iiDescription
    , iiNocContactEmail
    , iiCircuitInfos

    -- * MachineTypeScratchDisksItem
    , MachineTypeScratchDisksItem
    , machineTypeScratchDisksItem
    , mtsdiDiskGb

    -- * SubnetworksScopedListWarningDataItem
    , SubnetworksScopedListWarningDataItem
    , subnetworksScopedListWarningDataItem
    , sslwdiValue
    , sslwdiKey

    -- * MachineTypesScopedList
    , MachineTypesScopedList
    , machineTypesScopedList
    , mtslMachineTypes
    , mtslWarning

    -- * SubnetworksScopedListWarningCode
    , SubnetworksScopedListWarningCode (..)

    -- * Subnetwork
    , Subnetwork
    , subnetwork
    , subKind
    , subNetwork
    , subGatewayAddress
    , subSelfLink
    , subName
    , subSecondaryIPRanges
    , subCreationTimestamp
    , subIPCIdRRange
    , subId
    , subRegion
    , subDescription
    , subPrivateIPGoogleAccess

    -- * HealthCheckListWarning
    , HealthCheckListWarning
    , healthCheckListWarning
    , hclwData
    , hclwCode
    , hclwMessage

    -- * MachineTypeAggregatedList
    , MachineTypeAggregatedList
    , machineTypeAggregatedList
    , mtalNextPageToken
    , mtalKind
    , mtalItems
    , mtalSelfLink
    , mtalWarning
    , mtalId

    -- * QuotaMetric
    , QuotaMetric (..)

    -- * DiskType
    , DiskType
    , diskType
    , dtKind
    , dtZone
    , dtSelfLink
    , dtName
    , dtCreationTimestamp
    , dtId
    , dtValidDiskSize
    , dtDescription
    , dtDefaultDiskSizeGb
    , dtDeprecated

    -- * AutoscalerAggregatedListWarningDataItem
    , AutoscalerAggregatedListWarningDataItem
    , autoscalerAggregatedListWarningDataItem
    , aalwdiValue
    , aalwdiKey

    -- * ZoneSetLabelsRequestLabels
    , ZoneSetLabelsRequestLabels
    , zoneSetLabelsRequestLabels
    , zslrlAddtional

    -- * URLMapValidationResult
    , URLMapValidationResult
    , urlMapValidationResult
    , umvrLoadErrors
    , umvrLoadSucceeded
    , umvrTestPassed
    , umvrTestFailures

    -- * Metadata
    , Metadata
    , metadata
    , mKind
    , mFingerprint
    , mItems

    -- * RouteWarningsItem
    , RouteWarningsItem
    , routeWarningsItem
    , rwiData
    , rwiCode
    , rwiMessage

    -- * InterconnectLocationList
    , InterconnectLocationList
    , interconnectLocationList
    , illNextPageToken
    , illKind
    , illItems
    , illSelfLink
    , illWarning
    , illId

    -- * InstancePropertiesLabels
    , InstancePropertiesLabels
    , instancePropertiesLabels
    , iplAddtional

    -- * AutoscalerAggregatedListWarningCode
    , AutoscalerAggregatedListWarningCode (..)

    -- * AddressIPVersion
    , AddressIPVersion (..)

    -- * MachineTypesScopedListWarningDataItem
    , MachineTypesScopedListWarningDataItem
    , machineTypesScopedListWarningDataItem
    , mtslwdiValue
    , mtslwdiKey

    -- * InstanceStatus
    , InstanceStatus (..)

    -- * DiskTypeListWarning
    , DiskTypeListWarning
    , diskTypeListWarning
    , dtlwData
    , dtlwCode
    , dtlwMessage

    -- * RegionListWarningDataItem
    , RegionListWarningDataItem
    , regionListWarningDataItem
    , regValue
    , regKey

    -- * MachineTypesScopedListWarningCode
    , MachineTypesScopedListWarningCode (..)

    -- * InstancesSetMachineResourcesRequest
    , InstancesSetMachineResourcesRequest
    , instancesSetMachineResourcesRequest
    , ismrrGuestAccelerators

    -- * InstancesSetServiceAccountRequest
    , InstancesSetServiceAccountRequest
    , instancesSetServiceAccountRequest
    , issarEmail
    , issarScopes

    -- * DiskTypesScopedListWarningDataItem
    , DiskTypesScopedListWarningDataItem
    , diskTypesScopedListWarningDataItem
    , dtslwdiValue
    , dtslwdiKey

    -- * VPNTunnelAggregatedListWarning
    , VPNTunnelAggregatedListWarning
    , vpnTunnelAggregatedListWarning
    , vtalwData
    , vtalwCode
    , vtalwMessage

    -- * TargetHTTPProxy
    , TargetHTTPProxy
    , targetHTTPProxy
    , thttppURLMap
    , thttppKind
    , thttppSelfLink
    , thttppName
    , thttppCreationTimestamp
    , thttppId
    , thttppDescription

    -- * MachineType
    , MachineType
    , machineType
    , mtIsSharedCPU
    , mtKind
    , mtImageSpaceGb
    , mtZone
    , mtSelfLink
    , mtName
    , mtCreationTimestamp
    , mtScratchDisks
    , mtId
    , mtGuestCPUs
    , mtMaximumPersistentDisksSizeGb
    , mtMaximumPersistentDisks
    , mtMemoryMb
    , mtDescription
    , mtDeprecated

    -- * AcceleratorConfig
    , AcceleratorConfig
    , acceleratorConfig
    , acAcceleratorCount
    , acAcceleratorType

    -- * AutoscalerListWarning
    , AutoscalerListWarning
    , autoscalerListWarning
    , alwData
    , alwCode
    , alwMessage

    -- * RegionListWarningCode
    , RegionListWarningCode (..)

    -- * DiskTypesScopedListWarningCode
    , DiskTypesScopedListWarningCode (..)

    -- * MachineTypeAggregatedListWarningDataItem
    , MachineTypeAggregatedListWarningDataItem
    , machineTypeAggregatedListWarningDataItem
    , mtalwdiValue
    , mtalwdiKey

    -- * OperationError
    , OperationError
    , operationError
    , oeErrors

    -- * TargetInstancesScopedListWarning
    , TargetInstancesScopedListWarning
    , targetInstancesScopedListWarning
    , tislwData
    , tislwCode
    , tislwMessage

    -- * SubnetworkAggregatedList
    , SubnetworkAggregatedList
    , subnetworkAggregatedList
    , salNextPageToken
    , salKind
    , salItems
    , salSelfLink
    , salWarning
    , salId

    -- * MachineTypeAggregatedListWarningCode
    , MachineTypeAggregatedListWarningCode (..)

    -- * VPNTunnelListWarningDataItem
    , VPNTunnelListWarningDataItem
    , vpnTunnelListWarningDataItem
    , vtlwdiValue
    , vtlwdiKey

    -- * DisksResizeRequest
    , DisksResizeRequest
    , disksResizeRequest
    , drrSizeGb

    -- * AutoscalersScopedListWarningDataItem
    , AutoscalersScopedListWarningDataItem
    , autoscalersScopedListWarningDataItem
    , aValue
    , aKey

    -- * InterconnectOutageNotificationState
    , InterconnectOutageNotificationState (..)

    -- * VPNTunnelListWarningCode
    , VPNTunnelListWarningCode (..)

    -- * AutoscalersScopedListWarningCode
    , AutoscalersScopedListWarningCode (..)

    -- * DiskTypeAggregatedListWarningDataItem
    , DiskTypeAggregatedListWarningDataItem
    , diskTypeAggregatedListWarningDataItem
    , dtalwdiValue
    , dtalwdiKey

    -- * ForwardingRuleLoadBalancingScheme
    , ForwardingRuleLoadBalancingScheme (..)

    -- * RegionInstanceGroupManagerListWarning
    , RegionInstanceGroupManagerListWarning
    , regionInstanceGroupManagerListWarning
    , rigmlwData
    , rigmlwCode
    , rigmlwMessage

    -- * RegionInstanceGroupManagersSetTemplateRequest
    , RegionInstanceGroupManagersSetTemplateRequest
    , regionInstanceGroupManagersSetTemplateRequest
    , rigmstrInstanceTemplate

    -- * InstanceGroupsListInstances
    , InstanceGroupsListInstances
    , instanceGroupsListInstances
    , igliNextPageToken
    , igliKind
    , igliItems
    , igliSelfLink
    , igliWarning
    , igliId

    -- * DiskTypeAggregatedListWarningCode
    , DiskTypeAggregatedListWarningCode (..)

    -- * Autoscaler
    , Autoscaler
    , autoscaler
    , aaStatus
    , aaKind
    , aaZone
    , aaStatusDetails
    , aaSelfLink
    , aaName
    , aaCreationTimestamp
    , aaAutoscalingPolicy
    , aaId
    , aaRegion
    , aaDescription
    , aaTarget

    -- * MachineTypeListWarning
    , MachineTypeListWarning
    , machineTypeListWarning
    , mtlwData
    , mtlwCode
    , mtlwMessage

    -- * TargetHTTPProxyListWarning
    , TargetHTTPProxyListWarning
    , targetHTTPProxyListWarning
    , thttpplwData
    , thttpplwCode
    , thttpplwMessage

    -- * DiskAggregatedListWarning
    , DiskAggregatedListWarning
    , diskAggregatedListWarning
    , dalwData
    , dalwCode
    , dalwMessage

    -- * TargetPoolAggregatedListWarning
    , TargetPoolAggregatedListWarning
    , targetPoolAggregatedListWarning
    , tpalwData
    , tpalwCode
    , tpalwMessage

    -- * DisksScopedListWarning
    , DisksScopedListWarning
    , disksScopedListWarning
    , dslwData
    , dslwCode
    , dslwMessage

    -- * TargetVPNGatewayAggregatedListItems
    , TargetVPNGatewayAggregatedListItems
    , targetVPNGatewayAggregatedListItems
    , tvgaliAddtional

    -- * DiskLabels
    , DiskLabels
    , diskLabels
    , dlAddtional

    -- * InstanceGroupManagerListWarningDataItem
    , InstanceGroupManagerListWarningDataItem
    , instanceGroupManagerListWarningDataItem
    , igmlwdiValue
    , igmlwdiKey

    -- * InterconnectInterconnectType
    , InterconnectInterconnectType (..)

    -- * ForwardingRuleAggregatedListWarning
    , ForwardingRuleAggregatedListWarning
    , forwardingRuleAggregatedListWarning
    , fralwData
    , fralwCode
    , fralwMessage

    -- * InstanceGroupManagerListWarningCode
    , InstanceGroupManagerListWarningCode (..)

    -- * RouterStatusBGPPeerStatusStatus
    , RouterStatusBGPPeerStatusStatus (..)

    -- * RegionAutoscalerListWarningDataItem
    , RegionAutoscalerListWarningDataItem
    , regionAutoscalerListWarningDataItem
    , ralwdiaValue
    , ralwdiaKey

    -- * DiskListWarningCode
    , DiskListWarningCode (..)

    -- * GlobalSetLabelsRequestLabels
    , GlobalSetLabelsRequestLabels
    , globalSetLabelsRequestLabels
    , gslrlAddtional

    -- * TargetPoolsScopedListWarning
    , TargetPoolsScopedListWarning
    , targetPoolsScopedListWarning
    , tpslwData
    , tpslwCode
    , tpslwMessage

    -- * HealthStatus
    , HealthStatus
    , healthStatus
    , hsIPAddress
    , hsHealthState
    , hsPort
    , hsInstance

    -- * TargetTCPProxyList
    , TargetTCPProxyList
    , targetTCPProxyList
    , ttplNextPageToken
    , ttplKind
    , ttplItems
    , ttplSelfLink
    , ttplWarning
    , ttplId

    -- * Region
    , Region
    , region
    , regeStatus
    , regeZones
    , regeKind
    , regeSelfLink
    , regeName
    , regeCreationTimestamp
    , regeQuotas
    , regeId
    , regeDescription
    , regeDeprecated

    -- * RegionAutoscalerListWarningCode
    , RegionAutoscalerListWarningCode (..)

    -- * OperationListWarningDataItem
    , OperationListWarningDataItem
    , operationListWarningDataItem
    , olwdiValue
    , olwdiKey

    -- * GuestOSFeature
    , GuestOSFeature
    , guestOSFeature
    , gofType

    -- * VPNTunnel
    , VPNTunnel
    , vpnTunnel
    , vtDetailedStatus
    , vtStatus
    , vtLocalTrafficSelector
    , vtKind
    , vtPeerIP
    , vtRouter
    , vtTargetVPNGateway
    , vtRemoteTrafficSelector
    , vtSelfLink
    , vtSharedSecret
    , vtName
    , vtCreationTimestamp
    , vtSharedSecretHash
    , vtId
    , vtIkeVersion
    , vtRegion
    , vtDescription

    -- * InstanceGroupManagerAggregatedListWarning
    , InstanceGroupManagerAggregatedListWarning
    , instanceGroupManagerAggregatedListWarning
    , igmalwData
    , igmalwCode
    , igmalwMessage

    -- * DiskListWarningDataItem
    , DiskListWarningDataItem
    , diskListWarningDataItem
    , dlwdiValue
    , dlwdiKey

    -- * CommitmentPlan
    , CommitmentPlan (..)

    -- * OperationListWarningCode
    , OperationListWarningCode (..)

    -- * AliasIPRange
    , AliasIPRange
    , aliasIPRange
    , airIPCIdRRange
    , airSubnetworkRangeName

    -- * ForwardingRuleListWarningCode
    , ForwardingRuleListWarningCode (..)

    -- * VPNTunnelsScopedListWarningCode
    , VPNTunnelsScopedListWarningCode (..)

    -- * TargetInstanceListWarning
    , TargetInstanceListWarning
    , targetInstanceListWarning
    , tilwData
    , tilwCode
    , tilwMessage

    -- * OperationAggregatedListWarning
    , OperationAggregatedListWarning
    , operationAggregatedListWarning
    , oalwData
    , oalwCode
    , oalwMessage

    -- * TargetInstanceAggregatedListWarningDataItem
    , TargetInstanceAggregatedListWarningDataItem
    , targetInstanceAggregatedListWarningDataItem
    , tialwdiValue
    , tialwdiKey

    -- * OperationsScopedListWarning
    , OperationsScopedListWarning
    , operationsScopedListWarning
    , oslwData
    , oslwCode
    , oslwMessage

    -- * ForwardingRuleListWarningDataItem
    , ForwardingRuleListWarningDataItem
    , forwardingRuleListWarningDataItem
    , frlwdiValue
    , frlwdiKey

    -- * Scheduling
    , Scheduling
    , scheduling
    , sAutomaticRestart
    , sOnHostMaintenance
    , sPreemptible

    -- * TargetInstanceAggregatedListWarningCode
    , TargetInstanceAggregatedListWarningCode (..)

    -- * VPNTunnelsScopedListWarningDataItem
    , VPNTunnelsScopedListWarningDataItem
    , vpnTunnelsScopedListWarningDataItem
    , vtslwdiValue
    , vtslwdiKey

    -- * InterconnectList
    , InterconnectList
    , interconnectList
    , intnNextPageToken
    , intnKind
    , intnItems
    , intnSelfLink
    , intnWarning
    , intnId

    -- * TargetPoolListWarningDataItem
    , TargetPoolListWarningDataItem
    , targetPoolListWarningDataItem
    , tplwdiValue
    , tplwdiKey

    -- * SubnetworkList
    , SubnetworkList
    , subnetworkList
    , slNextPageToken
    , slKind
    , slItems
    , slSelfLink
    , slWarning
    , slId

    -- * AddressAddressType
    , AddressAddressType (..)

    -- * TargetPoolListWarningCode
    , TargetPoolListWarningCode (..)

    -- * ForwardingRulesScopedListWarning
    , ForwardingRulesScopedListWarning
    , forwardingRulesScopedListWarning
    , frslwData
    , frslwCode
    , frslwMessage

    -- * HTTPHealthCheckList
    , HTTPHealthCheckList
    , hTTPHealthCheckList
    , httphclNextPageToken
    , httphclKind
    , httphclItems
    , httphclSelfLink
    , httphclWarning
    , httphclId

    -- * InstanceGroupManagersScopedListWarning
    , InstanceGroupManagersScopedListWarning
    , instanceGroupManagersScopedListWarning
    , igmslwData
    , igmslwCode
    , igmslwMessage

    -- * URLMapsValidateRequest
    , URLMapsValidateRequest
    , urlMapsValidateRequest
    , umvrResource

    -- * InstanceGroupManagersSetTargetPoolsRequest
    , InstanceGroupManagersSetTargetPoolsRequest
    , instanceGroupManagersSetTargetPoolsRequest
    , igmstprFingerprint
    , igmstprTargetPools

    -- * NetworkListWarningCode
    , NetworkListWarningCode (..)

    -- * Commitment
    , Commitment
    , commitment
    , cStatus
    , cKind
    , cPlan
    , cResources
    , cEndTimestamp
    , cSelfLink
    , cName
    , cStatusMessage
    , cCreationTimestamp
    , cId
    , cRegion
    , cStartTimestamp
    , cDescription

    -- * HTTPSHealthCheckList
    , HTTPSHealthCheckList
    , httpsHealthCheckList
    , hhclNextPageToken
    , hhclKind
    , hhclItems
    , hhclSelfLink
    , hhclWarning
    , hhclId

    -- * AddressListWarningDataItem
    , AddressListWarningDataItem
    , addressListWarningDataItem
    , addValue
    , addKey

    -- * OperationErrorErrorsItem
    , OperationErrorErrorsItem
    , operationErrorErrorsItem
    , oeeiLocation
    , oeeiCode
    , oeeiMessage

    -- * CommitmentListWarning
    , CommitmentListWarning
    , commitmentListWarning
    , clwData
    , clwCode
    , clwMessage

    -- * License
    , License
    , license
    , lChargesUseFee
    , lKind
    , lSelfLink
    , lName

    -- * PathRule
    , PathRule
    , pathRule
    , prService
    , prPaths

    -- * XpnHostListWarningCode
    , XpnHostListWarningCode (..)

    -- * InterconnectAttachmentsScopedListWarningDataItem
    , InterconnectAttachmentsScopedListWarningDataItem
    , interconnectAttachmentsScopedListWarningDataItem
    , iaslwdiValue
    , iaslwdiKey

    -- * InterconnectAttachmentAggregatedListWarningCode
    , InterconnectAttachmentAggregatedListWarningCode (..)

    -- * CommitmentsScopedListWarningDataItem
    , CommitmentsScopedListWarningDataItem
    , commitmentsScopedListWarningDataItem
    , cslwdiValue
    , cslwdiKey

    -- * InterconnectAttachment
    , InterconnectAttachment
    , interconnectAttachment
    , iaKind
    , iaCustomerRouterIPAddress
    , iaRouter
    , iaOperationalStatus
    , iaSelfLink
    , iaName
    , iaGoogleReferenceId
    , iaCreationTimestamp
    , iaInterconnect
    , iaCloudRouterIPAddress
    , iaId
    , iaRegion
    , iaDescription
    , iaPrivateInterconnectInfo

    -- * InstanceList
    , InstanceList
    , instanceList
    , insNextPageToken
    , insKind
    , insItems
    , insSelfLink
    , insWarning
    , insId

    -- * NetworkListWarningDataItem
    , NetworkListWarningDataItem
    , networkListWarningDataItem
    , nlwdiValue
    , nlwdiKey

    -- * AddressListWarningCode
    , AddressListWarningCode (..)

    -- * NetworkPeeringState
    , NetworkPeeringState (..)

    -- * CacheKeyPolicy
    , CacheKeyPolicy
    , cacheKeyPolicy
    , ckpQueryStringWhiteList
    , ckpIncludeHost
    , ckpIncludeProtocol
    , ckpQueryStringBlackList
    , ckpIncludeQueryString

    -- * ZoneListWarningCode
    , ZoneListWarningCode (..)

    -- * CommitmentAggregatedListWarningDataItem
    , CommitmentAggregatedListWarningDataItem
    , commitmentAggregatedListWarningDataItem
    , calwdiValue
    , calwdiKey

    -- * RegionInstanceGroupListWarningDataItem
    , RegionInstanceGroupListWarningDataItem
    , regionInstanceGroupListWarningDataItem
    , riglwdiValue
    , riglwdiKey

    -- * SubnetworkAggregatedListItems
    , SubnetworkAggregatedListItems
    , subnetworkAggregatedListItems
    , saliAddtional

    -- * InterconnectAttachmentAggregatedListWarningDataItem
    , InterconnectAttachmentAggregatedListWarningDataItem
    , interconnectAttachmentAggregatedListWarningDataItem
    , iaalwdiValue
    , iaalwdiKey

    -- * InterconnectAttachmentListWarning
    , InterconnectAttachmentListWarning
    , interconnectAttachmentListWarning
    , intData
    , intCode
    , intMessage

    -- * InterconnectOutageNotificationIssueType
    , InterconnectOutageNotificationIssueType (..)

    -- * CommitmentsScopedListWarningCode
    , CommitmentsScopedListWarningCode (..)

    -- * AddressAggregatedListWarning
    , AddressAggregatedListWarning
    , addressAggregatedListWarning
    , addData
    , addCode
    , addMessage

    -- * ManagedInstanceLastAttempt
    , ManagedInstanceLastAttempt
    , managedInstanceLastAttempt
    , milaErrors

    -- * AutoscalerStatusDetails
    , AutoscalerStatusDetails
    , autoscalerStatusDetails
    , asdType
    , asdMessage

    -- * BackendServiceList
    , BackendServiceList
    , backendServiceList
    , bslNextPageToken
    , bslKind
    , bslItems
    , bslSelfLink
    , bslWarning
    , bslId

    -- * XpnHostListWarningDataItem
    , XpnHostListWarningDataItem
    , xpnHostListWarningDataItem
    , xhlwdiValue
    , xhlwdiKey

    -- * InterconnectAttachmentsScopedListWarningCode
    , InterconnectAttachmentsScopedListWarningCode (..)

    -- * ZoneListWarningDataItem
    , ZoneListWarningDataItem
    , zoneListWarningDataItem
    , zlwdiValue
    , zlwdiKey

    -- * CommitmentAggregatedListWarningCode
    , CommitmentAggregatedListWarningCode (..)

    -- * RegionInstanceGroupListWarningCode
    , RegionInstanceGroupListWarningCode (..)

    -- * InstanceGroupsScopedListWarning
    , InstanceGroupsScopedListWarning
    , instanceGroupsScopedListWarning
    , igslwData
    , igslwCode
    , igslwMessage

    -- * AutoscalingPolicyCPUUtilization
    , AutoscalingPolicyCPUUtilization
    , autoscalingPolicyCPUUtilization
    , apcuUtilizationTarget

    -- * InstanceGroupsScopedListWarningCode
    , InstanceGroupsScopedListWarningCode (..)

    -- * InterconnectOperationalStatus
    , InterconnectOperationalStatus (..)

    -- * InstanceGroupsScopedListWarningDataItem
    , InstanceGroupsScopedListWarningDataItem
    , instanceGroupsScopedListWarningDataItem
    , igslwdiValue
    , igslwdiKey

    -- * XpnHostListWarning
    , XpnHostListWarning
    , xpnHostListWarning
    , xhlwData
    , xhlwCode
    , xhlwMessage

    -- * ResourceGroupReference
    , ResourceGroupReference
    , resourceGroupReference
    , rgrGroup

    -- * Firewall
    , Firewall
    , firewall
    , fSourceTags
    , fSourceServiceAccounts
    , fPriority
    , fDirection
    , fKind
    , fTargetTags
    , fNetwork
    , fSourceRanges
    , fSelfLink
    , fName
    , fDenied
    , fCreationTimestamp
    , fId
    , fAllowed
    , fDestinationRanges
    , fTargetServiceAccounts
    , fDescription

    -- * InterconnectAttachmentsScopedListWarning
    , InterconnectAttachmentsScopedListWarning
    , interconnectAttachmentsScopedListWarning
    , iaslwData
    , iaslwCode
    , iaslwMessage

    -- * HostRule
    , HostRule
    , hostRule
    , hrHosts
    , hrDescription
    , hrPathMatcher

    -- * Quota
    , Quota
    , quota
    , qMetric
    , qLimit
    , qUsage

    -- * InstanceGroup
    , InstanceGroup
    , instanceGroup
    , ig1Size
    , ig1Kind
    , ig1Fingerprint
    , ig1Network
    , ig1Zone
    , ig1SelfLink
    , ig1Name
    , ig1CreationTimestamp
    , ig1Subnetwork
    , ig1Id
    , ig1Region
    , ig1Description
    , ig1NamedPorts

    -- * AddressListWarning
    , AddressListWarning
    , addressListWarning
    , alwlData
    , alwlCode
    , alwlMessage

    -- * RouterInterface
    , RouterInterface
    , routerInterface
    , riLinkedInterconnectAttachment
    , riName
    , riIPRange
    , riLinkedVPNTunnel

    -- * InstanceWithNamedPortsStatus
    , InstanceWithNamedPortsStatus (..)

    -- * NetworkListWarning
    , NetworkListWarning
    , networkListWarning
    , nlwData
    , nlwCode
    , nlwMessage

    -- * TCPHealthCheckProxyHeader
    , TCPHealthCheckProxyHeader (..)

    -- * SnapshotList
    , SnapshotList
    , snapshotList
    , snaNextPageToken
    , snaKind
    , snaItems
    , snaSelfLink
    , snaWarning
    , snaId

    -- * CommitmentListWarningCode
    , CommitmentListWarningCode (..)

    -- * TestFailure
    , TestFailure
    , testFailure
    , tfPath
    , tfExpectedService
    , tfHost
    , tfActualService

    -- * CommitmentListWarningDataItem
    , CommitmentListWarningDataItem
    , commitmentListWarningDataItem
    , clwdiValue
    , clwdiKey

    -- * CommitmentsScopedListWarning
    , CommitmentsScopedListWarning
    , commitmentsScopedListWarning
    , cslwData
    , cslwCode
    , cslwMessage

    -- * SerialPortOutput
    , SerialPortOutput
    , serialPortOutput
    , spoNext
    , spoContents
    , spoKind
    , spoStart
    , spoSelfLink

    -- * TargetVPNGatewayAggregatedList
    , TargetVPNGatewayAggregatedList
    , targetVPNGatewayAggregatedList
    , tvgalNextPageToken
    , tvgalKind
    , tvgalItems
    , tvgalSelfLink
    , tvgalWarning
    , tvgalId

    -- * InterconnectAttachmentAggregatedListWarning
    , InterconnectAttachmentAggregatedListWarning
    , interconnectAttachmentAggregatedListWarning
    , iaalwData
    , iaalwCode
    , iaalwMessage

    -- * MetadataItemsItem
    , MetadataItemsItem
    , metadataItemsItem
    , miiValue
    , miiKey

    -- * TargetHTTPSProxy
    , TargetHTTPSProxy
    , targetHTTPSProxy
    , thpURLMap
    , thpSSLCertificates
    , thpKind
    , thpSelfLink
    , thpName
    , thpCreationTimestamp
    , thpId
    , thpDescription

    -- * ConnectionDraining
    , ConnectionDraining
    , connectionDraining
    , cdDrainingTimeoutSec

    -- * InterconnectAttachmentListWarningDataItem
    , InterconnectAttachmentListWarningDataItem
    , interconnectAttachmentListWarningDataItem
    , ialwdiValue
    , ialwdiKey

    -- * AddressAggregatedListWarningCode
    , AddressAggregatedListWarningCode (..)

    -- * CacheInvalidationRule
    , CacheInvalidationRule
    , cacheInvalidationRule
    , cirPath
    , cirHost

    -- * AddressAggregatedListWarningDataItem
    , AddressAggregatedListWarningDataItem
    , addressAggregatedListWarningDataItem
    , aalwdiaValue
    , aalwdiaKey

    -- * InterconnectAttachmentListWarningCode
    , InterconnectAttachmentListWarningCode (..)

    -- * TargetVPNGatewaysScopedList
    , TargetVPNGatewaysScopedList
    , targetVPNGatewaysScopedList
    , tvgslTargetVPNGateways
    , tvgslWarning

    -- * CommitmentAggregatedListWarning
    , CommitmentAggregatedListWarning
    , commitmentAggregatedListWarning
    , calwData
    , calwCode
    , calwMessage

    -- * RegionInstanceGroupListWarning
    , RegionInstanceGroupListWarning
    , regionInstanceGroupListWarning
    , riglwData
    , riglwCode
    , riglwMessage

    -- * AccessConfig
    , AccessConfig
    , accessConfig
    , acKind
    , acName
    , acNATIP
    , acType

    -- * ZoneListWarning
    , ZoneListWarning
    , zoneListWarning
    , zlwData
    , zlwCode
    , zlwMessage

    -- * ManagedInstanceLastAttemptErrorsErrorsItem
    , ManagedInstanceLastAttemptErrorsErrorsItem
    , managedInstanceLastAttemptErrorsErrorsItem
    , milaeeiLocation
    , milaeeiCode
    , milaeeiMessage

    -- * InstancesScopedListWarningCode
    , InstancesScopedListWarningCode (..)

    -- * InstancesScopedListWarningDataItem
    , InstancesScopedListWarningDataItem
    , instancesScopedListWarningDataItem
    , islwdiValue
    , islwdiKey

    -- * SnapshotLabels
    , SnapshotLabels
    , snapshotLabels
    , slAddtional

    -- * BackendServicesScopedListWarningDataItem
    , BackendServicesScopedListWarningDataItem
    , backendServicesScopedListWarningDataItem
    , bsslwdiValue
    , bsslwdiKey

    -- * BackendService
    , BackendService
    , backendService
    , bsSessionAffinity
    , bsBackends
    , bsAffinityCookieTtlSec
    , bsIap
    , bsLoadBalancingScheme
    , bsKind
    , bsEnableCDN
    , bsFingerprint
    , bsProtocol
    , bsCdnPolicy
    , bsSelfLink
    , bsName
    , bsCreationTimestamp
    , bsId
    , bsRegion
    , bsConnectionDraining
    , bsTimeoutSec
    , bsDescription
    , bsPortName
    , bsHealthChecks
    , bsPort

    -- * InstanceListWarning
    , InstanceListWarning
    , instanceListWarning
    , insData
    , insCode
    , insMessage

    -- * InstanceMoveRequest
    , InstanceMoveRequest
    , instanceMoveRequest
    , imrTargetInstance
    , imrDestinationZone

    -- * XpnResourceIdType
    , XpnResourceIdType (..)

    -- * BackendServicesScopedListWarningCode
    , BackendServicesScopedListWarningCode (..)

    -- * CommitmentList
    , CommitmentList
    , commitmentList
    , clNextPageToken
    , clKind
    , clItems
    , clSelfLink
    , clWarning
    , clId

    -- * TargetPoolSessionAffinity
    , TargetPoolSessionAffinity (..)

    -- * InstancesSetLabelsRequestLabels
    , InstancesSetLabelsRequestLabels
    , instancesSetLabelsRequestLabels
    , islrlAddtional

    -- * InstanceGroupsScopedList
    , InstanceGroupsScopedList
    , instanceGroupsScopedList
    , igslWarning
    , igslInstanceGroups

    -- * InstancesStartWithEncryptionKeyRequest
    , InstancesStartWithEncryptionKeyRequest
    , instancesStartWithEncryptionKeyRequest
    , iswekrDisks

    -- * HTTPSHealthCheck
    , HTTPSHealthCheck
    , httpsHealthCheck
    , hhcRequestPath
    , hhcHost
    , hhcProxyHeader
    , hhcPortName
    , hhcPort

    -- * AutoscalerStatus
    , AutoscalerStatus (..)

    -- * ImageRawDiskContainerType
    , ImageRawDiskContainerType (..)

    -- * InstanceAggregatedListWarningCode
    , InstanceAggregatedListWarningCode (..)

    -- * VPNTunnelAggregatedListItems
    , VPNTunnelAggregatedListItems
    , vpnTunnelAggregatedListItems
    , vtaliAddtional

    -- * ResourceCommitmentType
    , ResourceCommitmentType (..)

    -- * InstanceAggregatedListWarningDataItem
    , InstanceAggregatedListWarningDataItem
    , instanceAggregatedListWarningDataItem
    , insValue
    , insKey

    -- * Tags
    , Tags
    , tags
    , tFingerprint
    , tItems

    -- * AddressAggregatedList
    , AddressAggregatedList
    , addressAggregatedList
    , addNextPageToken
    , addKind
    , addItems
    , addSelfLink
    , addWarning
    , addId

    -- * InterconnectAttachmentList
    , InterconnectAttachmentList
    , interconnectAttachmentList
    , ialaNextPageToken
    , ialaKind
    , ialaItems
    , ialaSelfLink
    , ialaWarning
    , ialaId

    -- * OperationWarningsItem
    , OperationWarningsItem
    , operationWarningsItem
    , owiData
    , owiCode
    , owiMessage

    -- * URLMapTest
    , URLMapTest
    , urlMapTest
    , umtPath
    , umtService
    , umtHost
    , umtDescription

    -- * HTTPSHealthCheckListWarning
    , HTTPSHealthCheckListWarning
    , httpsHealthCheckListWarning
    , hhclwData
    , hhclwCode
    , hhclwMessage

    -- * RoutersPreviewResponse
    , RoutersPreviewResponse
    , routersPreviewResponse
    , rprResource

    -- * BackendServiceAggregatedListWarningDataItem
    , BackendServiceAggregatedListWarningDataItem
    , backendServiceAggregatedListWarningDataItem
    , bsalwdiValue
    , bsalwdiKey

    -- * Instance
    , Instance
    , instance'
    , i1Status
    , i1ServiceAccounts
    , i1DeletionProtection
    , i1NetworkInterfaces
    , i1Kind
    , i1Zone
    , i1CPUPlatform
    , i1SelfLink
    , i1GuestAccelerators
    , i1Name
    , i1StatusMessage
    , i1CreationTimestamp
    , i1MachineType
    , i1Metadata
    , i1Id
    , i1Labels
    , i1StartRestricted
    , i1Scheduling
    , i1MinCPUPlatform
    , i1Disks
    , i1CanIPForward
    , i1LabelFingerprint
    , i1Description
    , i1Tags

    -- * PathMatcher
    , PathMatcher
    , pathMatcher
    , pmDefaultService
    , pmName
    , pmPathRules
    , pmDescription

    -- * BackendServiceListWarning
    , BackendServiceListWarning
    , backendServiceListWarning
    , bslwData
    , bslwCode
    , bslwMessage

    -- * BackendServiceAggregatedListWarningCode
    , BackendServiceAggregatedListWarningCode (..)
    ) where

import Network.Google.Compute.Types.Product
import Network.Google.Compute.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v1' of the Compute Engine API. This contains the host and root path used as a starting point for constructing service requests.
computeService :: ServiceConfig
computeService
  = defaultService (ServiceId "compute:v1")
      "www.googleapis.com"

-- | View and manage your Google Compute Engine resources
computeScope :: Proxy '["https://www.googleapis.com/auth/compute"]
computeScope = Proxy;

-- | View and manage your data across Google Cloud Platform services
cloudPlatformScope :: Proxy '["https://www.googleapis.com/auth/cloud-platform"]
cloudPlatformScope = Proxy;

-- | View your data in Google Cloud Storage
storageReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/devstorage.read_only"]
storageReadOnlyScope = Proxy;

-- | Manage your data in Google Cloud Storage
storageReadWriteScope :: Proxy '["https://www.googleapis.com/auth/devstorage.read_write"]
storageReadWriteScope = Proxy;

-- | View your Google Compute Engine resources
computeReadOnlyScope :: Proxy '["https://www.googleapis.com/auth/compute.readonly"]
computeReadOnlyScope = Proxy;

-- | Manage your data and permissions in Google Cloud Storage
storageFullControlScope :: Proxy '["https://www.googleapis.com/auth/devstorage.full_control"]
storageFullControlScope = Proxy;
