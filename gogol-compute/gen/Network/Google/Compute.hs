{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Compute
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and runs virtual machines on Google Cloud Platform.
--
-- /See:/ <https://developers.google.com/compute/docs/reference/latest/ Compute Engine API Reference>
module Network.Google.Compute
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

    -- * API Declaration
    , ComputeAPI

    -- * Resources

    -- ** compute.acceleratorTypes.aggregatedList
    , module Network.Google.Resource.Compute.AcceleratorTypes.AggregatedList

    -- ** compute.acceleratorTypes.get
    , module Network.Google.Resource.Compute.AcceleratorTypes.Get

    -- ** compute.acceleratorTypes.list
    , module Network.Google.Resource.Compute.AcceleratorTypes.List

    -- ** compute.addresses.aggregatedList
    , module Network.Google.Resource.Compute.Addresses.AggregatedList

    -- ** compute.addresses.delete
    , module Network.Google.Resource.Compute.Addresses.Delete

    -- ** compute.addresses.get
    , module Network.Google.Resource.Compute.Addresses.Get

    -- ** compute.addresses.insert
    , module Network.Google.Resource.Compute.Addresses.Insert

    -- ** compute.addresses.list
    , module Network.Google.Resource.Compute.Addresses.List

    -- ** compute.autoscalers.aggregatedList
    , module Network.Google.Resource.Compute.Autoscalers.AggregatedList

    -- ** compute.autoscalers.delete
    , module Network.Google.Resource.Compute.Autoscalers.Delete

    -- ** compute.autoscalers.get
    , module Network.Google.Resource.Compute.Autoscalers.Get

    -- ** compute.autoscalers.insert
    , module Network.Google.Resource.Compute.Autoscalers.Insert

    -- ** compute.autoscalers.list
    , module Network.Google.Resource.Compute.Autoscalers.List

    -- ** compute.autoscalers.patch
    , module Network.Google.Resource.Compute.Autoscalers.Patch

    -- ** compute.autoscalers.update
    , module Network.Google.Resource.Compute.Autoscalers.Update

    -- ** compute.backendBuckets.delete
    , module Network.Google.Resource.Compute.BackendBuckets.Delete

    -- ** compute.backendBuckets.get
    , module Network.Google.Resource.Compute.BackendBuckets.Get

    -- ** compute.backendBuckets.insert
    , module Network.Google.Resource.Compute.BackendBuckets.Insert

    -- ** compute.backendBuckets.list
    , module Network.Google.Resource.Compute.BackendBuckets.List

    -- ** compute.backendBuckets.patch
    , module Network.Google.Resource.Compute.BackendBuckets.Patch

    -- ** compute.backendBuckets.update
    , module Network.Google.Resource.Compute.BackendBuckets.Update

    -- ** compute.backendServices.aggregatedList
    , module Network.Google.Resource.Compute.BackendServices.AggregatedList

    -- ** compute.backendServices.delete
    , module Network.Google.Resource.Compute.BackendServices.Delete

    -- ** compute.backendServices.get
    , module Network.Google.Resource.Compute.BackendServices.Get

    -- ** compute.backendServices.getHealth
    , module Network.Google.Resource.Compute.BackendServices.GetHealth

    -- ** compute.backendServices.insert
    , module Network.Google.Resource.Compute.BackendServices.Insert

    -- ** compute.backendServices.list
    , module Network.Google.Resource.Compute.BackendServices.List

    -- ** compute.backendServices.patch
    , module Network.Google.Resource.Compute.BackendServices.Patch

    -- ** compute.backendServices.update
    , module Network.Google.Resource.Compute.BackendServices.Update

    -- ** compute.diskTypes.aggregatedList
    , module Network.Google.Resource.Compute.DiskTypes.AggregatedList

    -- ** compute.diskTypes.get
    , module Network.Google.Resource.Compute.DiskTypes.Get

    -- ** compute.diskTypes.list
    , module Network.Google.Resource.Compute.DiskTypes.List

    -- ** compute.disks.aggregatedList
    , module Network.Google.Resource.Compute.Disks.AggregatedList

    -- ** compute.disks.createSnapshot
    , module Network.Google.Resource.Compute.Disks.CreateSnapshot

    -- ** compute.disks.delete
    , module Network.Google.Resource.Compute.Disks.Delete

    -- ** compute.disks.get
    , module Network.Google.Resource.Compute.Disks.Get

    -- ** compute.disks.insert
    , module Network.Google.Resource.Compute.Disks.Insert

    -- ** compute.disks.list
    , module Network.Google.Resource.Compute.Disks.List

    -- ** compute.disks.resize
    , module Network.Google.Resource.Compute.Disks.Resize

    -- ** compute.disks.setLabels
    , module Network.Google.Resource.Compute.Disks.SetLabels

    -- ** compute.firewalls.delete
    , module Network.Google.Resource.Compute.Firewalls.Delete

    -- ** compute.firewalls.get
    , module Network.Google.Resource.Compute.Firewalls.Get

    -- ** compute.firewalls.insert
    , module Network.Google.Resource.Compute.Firewalls.Insert

    -- ** compute.firewalls.list
    , module Network.Google.Resource.Compute.Firewalls.List

    -- ** compute.firewalls.patch
    , module Network.Google.Resource.Compute.Firewalls.Patch

    -- ** compute.firewalls.update
    , module Network.Google.Resource.Compute.Firewalls.Update

    -- ** compute.forwardingRules.aggregatedList
    , module Network.Google.Resource.Compute.ForwardingRules.AggregatedList

    -- ** compute.forwardingRules.delete
    , module Network.Google.Resource.Compute.ForwardingRules.Delete

    -- ** compute.forwardingRules.get
    , module Network.Google.Resource.Compute.ForwardingRules.Get

    -- ** compute.forwardingRules.insert
    , module Network.Google.Resource.Compute.ForwardingRules.Insert

    -- ** compute.forwardingRules.list
    , module Network.Google.Resource.Compute.ForwardingRules.List

    -- ** compute.forwardingRules.setTarget
    , module Network.Google.Resource.Compute.ForwardingRules.SetTarget

    -- ** compute.globalAddresses.delete
    , module Network.Google.Resource.Compute.GlobalAddresses.Delete

    -- ** compute.globalAddresses.get
    , module Network.Google.Resource.Compute.GlobalAddresses.Get

    -- ** compute.globalAddresses.insert
    , module Network.Google.Resource.Compute.GlobalAddresses.Insert

    -- ** compute.globalAddresses.list
    , module Network.Google.Resource.Compute.GlobalAddresses.List

    -- ** compute.globalForwardingRules.delete
    , module Network.Google.Resource.Compute.GlobalForwardingRules.Delete

    -- ** compute.globalForwardingRules.get
    , module Network.Google.Resource.Compute.GlobalForwardingRules.Get

    -- ** compute.globalForwardingRules.insert
    , module Network.Google.Resource.Compute.GlobalForwardingRules.Insert

    -- ** compute.globalForwardingRules.list
    , module Network.Google.Resource.Compute.GlobalForwardingRules.List

    -- ** compute.globalForwardingRules.setTarget
    , module Network.Google.Resource.Compute.GlobalForwardingRules.SetTarget

    -- ** compute.globalOperations.aggregatedList
    , module Network.Google.Resource.Compute.GlobalOperations.AggregatedList

    -- ** compute.globalOperations.delete
    , module Network.Google.Resource.Compute.GlobalOperations.Delete

    -- ** compute.globalOperations.get
    , module Network.Google.Resource.Compute.GlobalOperations.Get

    -- ** compute.globalOperations.list
    , module Network.Google.Resource.Compute.GlobalOperations.List

    -- ** compute.healthChecks.delete
    , module Network.Google.Resource.Compute.HealthChecks.Delete

    -- ** compute.healthChecks.get
    , module Network.Google.Resource.Compute.HealthChecks.Get

    -- ** compute.healthChecks.insert
    , module Network.Google.Resource.Compute.HealthChecks.Insert

    -- ** compute.healthChecks.list
    , module Network.Google.Resource.Compute.HealthChecks.List

    -- ** compute.healthChecks.patch
    , module Network.Google.Resource.Compute.HealthChecks.Patch

    -- ** compute.healthChecks.update
    , module Network.Google.Resource.Compute.HealthChecks.Update

    -- ** compute.httpHealthChecks.delete
    , module Network.Google.Resource.Compute.HTTPHealthChecks.Delete

    -- ** compute.httpHealthChecks.get
    , module Network.Google.Resource.Compute.HTTPHealthChecks.Get

    -- ** compute.httpHealthChecks.insert
    , module Network.Google.Resource.Compute.HTTPHealthChecks.Insert

    -- ** compute.httpHealthChecks.list
    , module Network.Google.Resource.Compute.HTTPHealthChecks.List

    -- ** compute.httpHealthChecks.patch
    , module Network.Google.Resource.Compute.HTTPHealthChecks.Patch

    -- ** compute.httpHealthChecks.update
    , module Network.Google.Resource.Compute.HTTPHealthChecks.Update

    -- ** compute.httpsHealthChecks.delete
    , module Network.Google.Resource.Compute.HTTPSHealthChecks.Delete

    -- ** compute.httpsHealthChecks.get
    , module Network.Google.Resource.Compute.HTTPSHealthChecks.Get

    -- ** compute.httpsHealthChecks.insert
    , module Network.Google.Resource.Compute.HTTPSHealthChecks.Insert

    -- ** compute.httpsHealthChecks.list
    , module Network.Google.Resource.Compute.HTTPSHealthChecks.List

    -- ** compute.httpsHealthChecks.patch
    , module Network.Google.Resource.Compute.HTTPSHealthChecks.Patch

    -- ** compute.httpsHealthChecks.update
    , module Network.Google.Resource.Compute.HTTPSHealthChecks.Update

    -- ** compute.images.delete
    , module Network.Google.Resource.Compute.Images.Delete

    -- ** compute.images.deprecate
    , module Network.Google.Resource.Compute.Images.Deprecate

    -- ** compute.images.get
    , module Network.Google.Resource.Compute.Images.Get

    -- ** compute.images.getFromFamily
    , module Network.Google.Resource.Compute.Images.GetFromFamily

    -- ** compute.images.insert
    , module Network.Google.Resource.Compute.Images.Insert

    -- ** compute.images.list
    , module Network.Google.Resource.Compute.Images.List

    -- ** compute.images.setLabels
    , module Network.Google.Resource.Compute.Images.SetLabels

    -- ** compute.instanceGroupManagers.abandonInstances
    , module Network.Google.Resource.Compute.InstanceGroupManagers.AbandonInstances

    -- ** compute.instanceGroupManagers.aggregatedList
    , module Network.Google.Resource.Compute.InstanceGroupManagers.AggregatedList

    -- ** compute.instanceGroupManagers.delete
    , module Network.Google.Resource.Compute.InstanceGroupManagers.Delete

    -- ** compute.instanceGroupManagers.deleteInstances
    , module Network.Google.Resource.Compute.InstanceGroupManagers.DeleteInstances

    -- ** compute.instanceGroupManagers.get
    , module Network.Google.Resource.Compute.InstanceGroupManagers.Get

    -- ** compute.instanceGroupManagers.insert
    , module Network.Google.Resource.Compute.InstanceGroupManagers.Insert

    -- ** compute.instanceGroupManagers.list
    , module Network.Google.Resource.Compute.InstanceGroupManagers.List

    -- ** compute.instanceGroupManagers.listManagedInstances
    , module Network.Google.Resource.Compute.InstanceGroupManagers.ListManagedInstances

    -- ** compute.instanceGroupManagers.recreateInstances
    , module Network.Google.Resource.Compute.InstanceGroupManagers.RecreateInstances

    -- ** compute.instanceGroupManagers.resize
    , module Network.Google.Resource.Compute.InstanceGroupManagers.Resize

    -- ** compute.instanceGroupManagers.setInstanceTemplate
    , module Network.Google.Resource.Compute.InstanceGroupManagers.SetInstanceTemplate

    -- ** compute.instanceGroupManagers.setTargetPools
    , module Network.Google.Resource.Compute.InstanceGroupManagers.SetTargetPools

    -- ** compute.instanceGroups.addInstances
    , module Network.Google.Resource.Compute.InstanceGroups.AddInstances

    -- ** compute.instanceGroups.aggregatedList
    , module Network.Google.Resource.Compute.InstanceGroups.AggregatedList

    -- ** compute.instanceGroups.delete
    , module Network.Google.Resource.Compute.InstanceGroups.Delete

    -- ** compute.instanceGroups.get
    , module Network.Google.Resource.Compute.InstanceGroups.Get

    -- ** compute.instanceGroups.insert
    , module Network.Google.Resource.Compute.InstanceGroups.Insert

    -- ** compute.instanceGroups.list
    , module Network.Google.Resource.Compute.InstanceGroups.List

    -- ** compute.instanceGroups.listInstances
    , module Network.Google.Resource.Compute.InstanceGroups.ListInstances

    -- ** compute.instanceGroups.removeInstances
    , module Network.Google.Resource.Compute.InstanceGroups.RemoveInstances

    -- ** compute.instanceGroups.setNamedPorts
    , module Network.Google.Resource.Compute.InstanceGroups.SetNamedPorts

    -- ** compute.instanceTemplates.delete
    , module Network.Google.Resource.Compute.InstanceTemplates.Delete

    -- ** compute.instanceTemplates.get
    , module Network.Google.Resource.Compute.InstanceTemplates.Get

    -- ** compute.instanceTemplates.insert
    , module Network.Google.Resource.Compute.InstanceTemplates.Insert

    -- ** compute.instanceTemplates.list
    , module Network.Google.Resource.Compute.InstanceTemplates.List

    -- ** compute.instances.addAccessConfig
    , module Network.Google.Resource.Compute.Instances.AddAccessConfig

    -- ** compute.instances.aggregatedList
    , module Network.Google.Resource.Compute.Instances.AggregatedList

    -- ** compute.instances.attachDisk
    , module Network.Google.Resource.Compute.Instances.AttachDisk

    -- ** compute.instances.delete
    , module Network.Google.Resource.Compute.Instances.Delete

    -- ** compute.instances.deleteAccessConfig
    , module Network.Google.Resource.Compute.Instances.DeleteAccessConfig

    -- ** compute.instances.detachDisk
    , module Network.Google.Resource.Compute.Instances.DetachDisk

    -- ** compute.instances.get
    , module Network.Google.Resource.Compute.Instances.Get

    -- ** compute.instances.getSerialPortOutput
    , module Network.Google.Resource.Compute.Instances.GetSerialPortOutput

    -- ** compute.instances.insert
    , module Network.Google.Resource.Compute.Instances.Insert

    -- ** compute.instances.list
    , module Network.Google.Resource.Compute.Instances.List

    -- ** compute.instances.reset
    , module Network.Google.Resource.Compute.Instances.Reset

    -- ** compute.instances.setDeletionProtection
    , module Network.Google.Resource.Compute.Instances.SetDeletionProtection

    -- ** compute.instances.setDiskAutoDelete
    , module Network.Google.Resource.Compute.Instances.SetDiskAutoDelete

    -- ** compute.instances.setLabels
    , module Network.Google.Resource.Compute.Instances.SetLabels

    -- ** compute.instances.setMachineResources
    , module Network.Google.Resource.Compute.Instances.SetMachineResources

    -- ** compute.instances.setMachineType
    , module Network.Google.Resource.Compute.Instances.SetMachineType

    -- ** compute.instances.setMetadata
    , module Network.Google.Resource.Compute.Instances.SetMetadata

    -- ** compute.instances.setMinCpuPlatform
    , module Network.Google.Resource.Compute.Instances.SetMinCPUPlatform

    -- ** compute.instances.setScheduling
    , module Network.Google.Resource.Compute.Instances.SetScheduling

    -- ** compute.instances.setServiceAccount
    , module Network.Google.Resource.Compute.Instances.SetServiceAccount

    -- ** compute.instances.setTags
    , module Network.Google.Resource.Compute.Instances.SetTags

    -- ** compute.instances.start
    , module Network.Google.Resource.Compute.Instances.Start

    -- ** compute.instances.startWithEncryptionKey
    , module Network.Google.Resource.Compute.Instances.StartWithEncryptionKey

    -- ** compute.instances.stop
    , module Network.Google.Resource.Compute.Instances.Stop

    -- ** compute.interconnectAttachments.aggregatedList
    , module Network.Google.Resource.Compute.InterconnectAttachments.AggregatedList

    -- ** compute.interconnectAttachments.delete
    , module Network.Google.Resource.Compute.InterconnectAttachments.Delete

    -- ** compute.interconnectAttachments.get
    , module Network.Google.Resource.Compute.InterconnectAttachments.Get

    -- ** compute.interconnectAttachments.insert
    , module Network.Google.Resource.Compute.InterconnectAttachments.Insert

    -- ** compute.interconnectAttachments.list
    , module Network.Google.Resource.Compute.InterconnectAttachments.List

    -- ** compute.interconnectLocations.get
    , module Network.Google.Resource.Compute.InterconnectLocations.Get

    -- ** compute.interconnectLocations.list
    , module Network.Google.Resource.Compute.InterconnectLocations.List

    -- ** compute.interconnects.delete
    , module Network.Google.Resource.Compute.Interconnects.Delete

    -- ** compute.interconnects.get
    , module Network.Google.Resource.Compute.Interconnects.Get

    -- ** compute.interconnects.insert
    , module Network.Google.Resource.Compute.Interconnects.Insert

    -- ** compute.interconnects.list
    , module Network.Google.Resource.Compute.Interconnects.List

    -- ** compute.interconnects.patch
    , module Network.Google.Resource.Compute.Interconnects.Patch

    -- ** compute.licenses.get
    , module Network.Google.Resource.Compute.Licenses.Get

    -- ** compute.machineTypes.aggregatedList
    , module Network.Google.Resource.Compute.MachineTypes.AggregatedList

    -- ** compute.machineTypes.get
    , module Network.Google.Resource.Compute.MachineTypes.Get

    -- ** compute.machineTypes.list
    , module Network.Google.Resource.Compute.MachineTypes.List

    -- ** compute.networks.addPeering
    , module Network.Google.Resource.Compute.Networks.AddPeering

    -- ** compute.networks.delete
    , module Network.Google.Resource.Compute.Networks.Delete

    -- ** compute.networks.get
    , module Network.Google.Resource.Compute.Networks.Get

    -- ** compute.networks.insert
    , module Network.Google.Resource.Compute.Networks.Insert

    -- ** compute.networks.list
    , module Network.Google.Resource.Compute.Networks.List

    -- ** compute.networks.patch
    , module Network.Google.Resource.Compute.Networks.Patch

    -- ** compute.networks.removePeering
    , module Network.Google.Resource.Compute.Networks.RemovePeering

    -- ** compute.networks.switchToCustomMode
    , module Network.Google.Resource.Compute.Networks.SwitchToCustomMode

    -- ** compute.projects.disableXpnHost
    , module Network.Google.Resource.Compute.Projects.DisableXpnHost

    -- ** compute.projects.disableXpnResource
    , module Network.Google.Resource.Compute.Projects.DisableXpnResource

    -- ** compute.projects.enableXpnHost
    , module Network.Google.Resource.Compute.Projects.EnableXpnHost

    -- ** compute.projects.enableXpnResource
    , module Network.Google.Resource.Compute.Projects.EnableXpnResource

    -- ** compute.projects.get
    , module Network.Google.Resource.Compute.Projects.Get

    -- ** compute.projects.getXpnHost
    , module Network.Google.Resource.Compute.Projects.GetXpnHost

    -- ** compute.projects.getXpnResources
    , module Network.Google.Resource.Compute.Projects.GetXpnResources

    -- ** compute.projects.listXpnHosts
    , module Network.Google.Resource.Compute.Projects.ListXpnHosts

    -- ** compute.projects.moveDisk
    , module Network.Google.Resource.Compute.Projects.MoveDisk

    -- ** compute.projects.moveInstance
    , module Network.Google.Resource.Compute.Projects.MoveInstance

    -- ** compute.projects.setCommonInstanceMetadata
    , module Network.Google.Resource.Compute.Projects.SetCommonInstanceMetadata

    -- ** compute.projects.setUsageExportBucket
    , module Network.Google.Resource.Compute.Projects.SetUsageExportBucket

    -- ** compute.regionAutoscalers.delete
    , module Network.Google.Resource.Compute.RegionAutoscalers.Delete

    -- ** compute.regionAutoscalers.get
    , module Network.Google.Resource.Compute.RegionAutoscalers.Get

    -- ** compute.regionAutoscalers.insert
    , module Network.Google.Resource.Compute.RegionAutoscalers.Insert

    -- ** compute.regionAutoscalers.list
    , module Network.Google.Resource.Compute.RegionAutoscalers.List

    -- ** compute.regionAutoscalers.patch
    , module Network.Google.Resource.Compute.RegionAutoscalers.Patch

    -- ** compute.regionAutoscalers.update
    , module Network.Google.Resource.Compute.RegionAutoscalers.Update

    -- ** compute.regionBackendServices.delete
    , module Network.Google.Resource.Compute.RegionBackendServices.Delete

    -- ** compute.regionBackendServices.get
    , module Network.Google.Resource.Compute.RegionBackendServices.Get

    -- ** compute.regionBackendServices.getHealth
    , module Network.Google.Resource.Compute.RegionBackendServices.GetHealth

    -- ** compute.regionBackendServices.insert
    , module Network.Google.Resource.Compute.RegionBackendServices.Insert

    -- ** compute.regionBackendServices.list
    , module Network.Google.Resource.Compute.RegionBackendServices.List

    -- ** compute.regionBackendServices.patch
    , module Network.Google.Resource.Compute.RegionBackendServices.Patch

    -- ** compute.regionBackendServices.update
    , module Network.Google.Resource.Compute.RegionBackendServices.Update

    -- ** compute.regionCommitments.aggregatedList
    , module Network.Google.Resource.Compute.RegionCommitments.AggregatedList

    -- ** compute.regionCommitments.get
    , module Network.Google.Resource.Compute.RegionCommitments.Get

    -- ** compute.regionCommitments.insert
    , module Network.Google.Resource.Compute.RegionCommitments.Insert

    -- ** compute.regionCommitments.list
    , module Network.Google.Resource.Compute.RegionCommitments.List

    -- ** compute.regionInstanceGroupManagers.abandonInstances
    , module Network.Google.Resource.Compute.RegionInstanceGroupManagers.AbandonInstances

    -- ** compute.regionInstanceGroupManagers.delete
    , module Network.Google.Resource.Compute.RegionInstanceGroupManagers.Delete

    -- ** compute.regionInstanceGroupManagers.deleteInstances
    , module Network.Google.Resource.Compute.RegionInstanceGroupManagers.DeleteInstances

    -- ** compute.regionInstanceGroupManagers.get
    , module Network.Google.Resource.Compute.RegionInstanceGroupManagers.Get

    -- ** compute.regionInstanceGroupManagers.insert
    , module Network.Google.Resource.Compute.RegionInstanceGroupManagers.Insert

    -- ** compute.regionInstanceGroupManagers.list
    , module Network.Google.Resource.Compute.RegionInstanceGroupManagers.List

    -- ** compute.regionInstanceGroupManagers.listManagedInstances
    , module Network.Google.Resource.Compute.RegionInstanceGroupManagers.ListManagedInstances

    -- ** compute.regionInstanceGroupManagers.recreateInstances
    , module Network.Google.Resource.Compute.RegionInstanceGroupManagers.RecreateInstances

    -- ** compute.regionInstanceGroupManagers.resize
    , module Network.Google.Resource.Compute.RegionInstanceGroupManagers.Resize

    -- ** compute.regionInstanceGroupManagers.setInstanceTemplate
    , module Network.Google.Resource.Compute.RegionInstanceGroupManagers.SetInstanceTemplate

    -- ** compute.regionInstanceGroupManagers.setTargetPools
    , module Network.Google.Resource.Compute.RegionInstanceGroupManagers.SetTargetPools

    -- ** compute.regionInstanceGroups.get
    , module Network.Google.Resource.Compute.RegionInstanceGroups.Get

    -- ** compute.regionInstanceGroups.list
    , module Network.Google.Resource.Compute.RegionInstanceGroups.List

    -- ** compute.regionInstanceGroups.listInstances
    , module Network.Google.Resource.Compute.RegionInstanceGroups.ListInstances

    -- ** compute.regionInstanceGroups.setNamedPorts
    , module Network.Google.Resource.Compute.RegionInstanceGroups.SetNamedPorts

    -- ** compute.regionOperations.delete
    , module Network.Google.Resource.Compute.RegionOperations.Delete

    -- ** compute.regionOperations.get
    , module Network.Google.Resource.Compute.RegionOperations.Get

    -- ** compute.regionOperations.list
    , module Network.Google.Resource.Compute.RegionOperations.List

    -- ** compute.regions.get
    , module Network.Google.Resource.Compute.Regions.Get

    -- ** compute.regions.list
    , module Network.Google.Resource.Compute.Regions.List

    -- ** compute.routers.aggregatedList
    , module Network.Google.Resource.Compute.Routers.AggregatedList

    -- ** compute.routers.delete
    , module Network.Google.Resource.Compute.Routers.Delete

    -- ** compute.routers.get
    , module Network.Google.Resource.Compute.Routers.Get

    -- ** compute.routers.getRouterStatus
    , module Network.Google.Resource.Compute.Routers.GetRouterStatus

    -- ** compute.routers.insert
    , module Network.Google.Resource.Compute.Routers.Insert

    -- ** compute.routers.list
    , module Network.Google.Resource.Compute.Routers.List

    -- ** compute.routers.patch
    , module Network.Google.Resource.Compute.Routers.Patch

    -- ** compute.routers.preview
    , module Network.Google.Resource.Compute.Routers.Preview

    -- ** compute.routers.update
    , module Network.Google.Resource.Compute.Routers.Update

    -- ** compute.routes.delete
    , module Network.Google.Resource.Compute.Routes.Delete

    -- ** compute.routes.get
    , module Network.Google.Resource.Compute.Routes.Get

    -- ** compute.routes.insert
    , module Network.Google.Resource.Compute.Routes.Insert

    -- ** compute.routes.list
    , module Network.Google.Resource.Compute.Routes.List

    -- ** compute.snapshots.delete
    , module Network.Google.Resource.Compute.Snapshots.Delete

    -- ** compute.snapshots.get
    , module Network.Google.Resource.Compute.Snapshots.Get

    -- ** compute.snapshots.list
    , module Network.Google.Resource.Compute.Snapshots.List

    -- ** compute.snapshots.setLabels
    , module Network.Google.Resource.Compute.Snapshots.SetLabels

    -- ** compute.sslCertificates.delete
    , module Network.Google.Resource.Compute.SSLCertificates.Delete

    -- ** compute.sslCertificates.get
    , module Network.Google.Resource.Compute.SSLCertificates.Get

    -- ** compute.sslCertificates.insert
    , module Network.Google.Resource.Compute.SSLCertificates.Insert

    -- ** compute.sslCertificates.list
    , module Network.Google.Resource.Compute.SSLCertificates.List

    -- ** compute.subnetworks.aggregatedList
    , module Network.Google.Resource.Compute.Subnetworks.AggregatedList

    -- ** compute.subnetworks.delete
    , module Network.Google.Resource.Compute.Subnetworks.Delete

    -- ** compute.subnetworks.expandIpCidrRange
    , module Network.Google.Resource.Compute.Subnetworks.ExpandIPCIdRRange

    -- ** compute.subnetworks.get
    , module Network.Google.Resource.Compute.Subnetworks.Get

    -- ** compute.subnetworks.insert
    , module Network.Google.Resource.Compute.Subnetworks.Insert

    -- ** compute.subnetworks.list
    , module Network.Google.Resource.Compute.Subnetworks.List

    -- ** compute.subnetworks.setPrivateIpGoogleAccess
    , module Network.Google.Resource.Compute.Subnetworks.SetPrivateIPGoogleAccess

    -- ** compute.targetHttpProxies.delete
    , module Network.Google.Resource.Compute.TargetHTTPProxies.Delete

    -- ** compute.targetHttpProxies.get
    , module Network.Google.Resource.Compute.TargetHTTPProxies.Get

    -- ** compute.targetHttpProxies.insert
    , module Network.Google.Resource.Compute.TargetHTTPProxies.Insert

    -- ** compute.targetHttpProxies.list
    , module Network.Google.Resource.Compute.TargetHTTPProxies.List

    -- ** compute.targetHttpProxies.setUrlMap
    , module Network.Google.Resource.Compute.TargetHTTPProxies.SetURLMap

    -- ** compute.targetHttpsProxies.delete
    , module Network.Google.Resource.Compute.TargetHTTPSProxies.Delete

    -- ** compute.targetHttpsProxies.get
    , module Network.Google.Resource.Compute.TargetHTTPSProxies.Get

    -- ** compute.targetHttpsProxies.insert
    , module Network.Google.Resource.Compute.TargetHTTPSProxies.Insert

    -- ** compute.targetHttpsProxies.list
    , module Network.Google.Resource.Compute.TargetHTTPSProxies.List

    -- ** compute.targetHttpsProxies.setSslCertificates
    , module Network.Google.Resource.Compute.TargetHTTPSProxies.SetSSLCertificates

    -- ** compute.targetHttpsProxies.setUrlMap
    , module Network.Google.Resource.Compute.TargetHTTPSProxies.SetURLMap

    -- ** compute.targetInstances.aggregatedList
    , module Network.Google.Resource.Compute.TargetInstances.AggregatedList

    -- ** compute.targetInstances.delete
    , module Network.Google.Resource.Compute.TargetInstances.Delete

    -- ** compute.targetInstances.get
    , module Network.Google.Resource.Compute.TargetInstances.Get

    -- ** compute.targetInstances.insert
    , module Network.Google.Resource.Compute.TargetInstances.Insert

    -- ** compute.targetInstances.list
    , module Network.Google.Resource.Compute.TargetInstances.List

    -- ** compute.targetPools.addHealthCheck
    , module Network.Google.Resource.Compute.TargetPools.AddHealthCheck

    -- ** compute.targetPools.addInstance
    , module Network.Google.Resource.Compute.TargetPools.AddInstance

    -- ** compute.targetPools.aggregatedList
    , module Network.Google.Resource.Compute.TargetPools.AggregatedList

    -- ** compute.targetPools.delete
    , module Network.Google.Resource.Compute.TargetPools.Delete

    -- ** compute.targetPools.get
    , module Network.Google.Resource.Compute.TargetPools.Get

    -- ** compute.targetPools.getHealth
    , module Network.Google.Resource.Compute.TargetPools.GetHealth

    -- ** compute.targetPools.insert
    , module Network.Google.Resource.Compute.TargetPools.Insert

    -- ** compute.targetPools.list
    , module Network.Google.Resource.Compute.TargetPools.List

    -- ** compute.targetPools.removeHealthCheck
    , module Network.Google.Resource.Compute.TargetPools.RemoveHealthCheck

    -- ** compute.targetPools.removeInstance
    , module Network.Google.Resource.Compute.TargetPools.RemoveInstance

    -- ** compute.targetPools.setBackup
    , module Network.Google.Resource.Compute.TargetPools.SetBackup

    -- ** compute.targetSslProxies.delete
    , module Network.Google.Resource.Compute.TargetSSLProxies.Delete

    -- ** compute.targetSslProxies.get
    , module Network.Google.Resource.Compute.TargetSSLProxies.Get

    -- ** compute.targetSslProxies.insert
    , module Network.Google.Resource.Compute.TargetSSLProxies.Insert

    -- ** compute.targetSslProxies.list
    , module Network.Google.Resource.Compute.TargetSSLProxies.List

    -- ** compute.targetSslProxies.setBackendService
    , module Network.Google.Resource.Compute.TargetSSLProxies.SetBackendService

    -- ** compute.targetSslProxies.setProxyHeader
    , module Network.Google.Resource.Compute.TargetSSLProxies.SetProxyHeader

    -- ** compute.targetSslProxies.setSslCertificates
    , module Network.Google.Resource.Compute.TargetSSLProxies.SetSSLCertificates

    -- ** compute.targetTcpProxies.delete
    , module Network.Google.Resource.Compute.TargetTCPProxies.Delete

    -- ** compute.targetTcpProxies.get
    , module Network.Google.Resource.Compute.TargetTCPProxies.Get

    -- ** compute.targetTcpProxies.insert
    , module Network.Google.Resource.Compute.TargetTCPProxies.Insert

    -- ** compute.targetTcpProxies.list
    , module Network.Google.Resource.Compute.TargetTCPProxies.List

    -- ** compute.targetTcpProxies.setBackendService
    , module Network.Google.Resource.Compute.TargetTCPProxies.SetBackendService

    -- ** compute.targetTcpProxies.setProxyHeader
    , module Network.Google.Resource.Compute.TargetTCPProxies.SetProxyHeader

    -- ** compute.targetVpnGateways.aggregatedList
    , module Network.Google.Resource.Compute.TargetVPNGateways.AggregatedList

    -- ** compute.targetVpnGateways.delete
    , module Network.Google.Resource.Compute.TargetVPNGateways.Delete

    -- ** compute.targetVpnGateways.get
    , module Network.Google.Resource.Compute.TargetVPNGateways.Get

    -- ** compute.targetVpnGateways.insert
    , module Network.Google.Resource.Compute.TargetVPNGateways.Insert

    -- ** compute.targetVpnGateways.list
    , module Network.Google.Resource.Compute.TargetVPNGateways.List

    -- ** compute.urlMaps.delete
    , module Network.Google.Resource.Compute.URLMaps.Delete

    -- ** compute.urlMaps.get
    , module Network.Google.Resource.Compute.URLMaps.Get

    -- ** compute.urlMaps.insert
    , module Network.Google.Resource.Compute.URLMaps.Insert

    -- ** compute.urlMaps.invalidateCache
    , module Network.Google.Resource.Compute.URLMaps.InvalidateCache

    -- ** compute.urlMaps.list
    , module Network.Google.Resource.Compute.URLMaps.List

    -- ** compute.urlMaps.patch
    , module Network.Google.Resource.Compute.URLMaps.Patch

    -- ** compute.urlMaps.update
    , module Network.Google.Resource.Compute.URLMaps.Update

    -- ** compute.urlMaps.validate
    , module Network.Google.Resource.Compute.URLMaps.Validate

    -- ** compute.vpnTunnels.aggregatedList
    , module Network.Google.Resource.Compute.VPNTunnels.AggregatedList

    -- ** compute.vpnTunnels.delete
    , module Network.Google.Resource.Compute.VPNTunnels.Delete

    -- ** compute.vpnTunnels.get
    , module Network.Google.Resource.Compute.VPNTunnels.Get

    -- ** compute.vpnTunnels.insert
    , module Network.Google.Resource.Compute.VPNTunnels.Insert

    -- ** compute.vpnTunnels.list
    , module Network.Google.Resource.Compute.VPNTunnels.List

    -- ** compute.zoneOperations.delete
    , module Network.Google.Resource.Compute.ZoneOperations.Delete

    -- ** compute.zoneOperations.get
    , module Network.Google.Resource.Compute.ZoneOperations.Get

    -- ** compute.zoneOperations.list
    , module Network.Google.Resource.Compute.ZoneOperations.List

    -- ** compute.zones.get
    , module Network.Google.Resource.Compute.Zones.Get

    -- ** compute.zones.list
    , module Network.Google.Resource.Compute.Zones.List

    -- * Types

    -- ** HTTPSHealthCheckListWarningCode
    , HTTPSHealthCheckListWarningCode (..)

    -- ** InstanceAggregatedListWarning
    , InstanceAggregatedListWarning
    , instanceAggregatedListWarning
    , ialwData
    , ialwCode
    , ialwMessage

    -- ** TargetHTTPSProxyList
    , TargetHTTPSProxyList
    , targetHTTPSProxyList
    , thplNextPageToken
    , thplKind
    , thplItems
    , thplSelfLink
    , thplWarning
    , thplId

    -- ** RoutersScopedList
    , RoutersScopedList
    , routersScopedList
    , rslRouters
    , rslWarning

    -- ** RouterStatusResponse
    , RouterStatusResponse
    , routerStatusResponse
    , rsrKind
    , rsrResult

    -- ** RegionInstanceGroupManagersDeleteInstancesRequest
    , RegionInstanceGroupManagersDeleteInstancesRequest
    , regionInstanceGroupManagersDeleteInstancesRequest
    , rigmdirInstances

    -- ** AddressesScopedList
    , AddressesScopedList
    , addressesScopedList
    , aslAddresses
    , aslWarning

    -- ** OperationWarningsItemDataItem
    , OperationWarningsItemDataItem
    , operationWarningsItemDataItem
    , owidiValue
    , owidiKey

    -- ** SchedulingOnHostMaintenance
    , SchedulingOnHostMaintenance (..)

    -- ** RegionInstanceGroupsListInstancesRequest
    , RegionInstanceGroupsListInstancesRequest
    , regionInstanceGroupsListInstancesRequest
    , riglirInstanceState
    , riglirPortName

    -- ** BackendServiceAggregatedListWarning
    , BackendServiceAggregatedListWarning
    , backendServiceAggregatedListWarning
    , bsalwData
    , bsalwCode
    , bsalwMessage

    -- ** AutoscalingPolicyCustomMetricUtilizationUtilizationTargetType
    , AutoscalingPolicyCustomMetricUtilizationUtilizationTargetType (..)

    -- ** BackendServiceListWarningDataItem
    , BackendServiceListWarningDataItem
    , backendServiceListWarningDataItem
    , bslwdiValue
    , bslwdiKey

    -- ** FirewallDeniedItem
    , FirewallDeniedItem
    , firewallDeniedItem
    , fdiIPProtocol
    , fdiPorts

    -- ** InstanceGroupManagersAbandonInstancesRequest
    , InstanceGroupManagersAbandonInstancesRequest
    , instanceGroupManagersAbandonInstancesRequest
    , igmairInstances

    -- ** MachineTypeAggregatedListItems
    , MachineTypeAggregatedListItems
    , machineTypeAggregatedListItems
    , mtaliAddtional

    -- ** BackendServiceListWarningCode
    , BackendServiceListWarningCode (..)

    -- ** DiskTypeAggregatedListItems
    , DiskTypeAggregatedListItems
    , diskTypeAggregatedListItems
    , dtaliAddtional

    -- ** InstancesSetLabelsRequest
    , InstancesSetLabelsRequest
    , instancesSetLabelsRequest
    , islrLabels
    , islrLabelFingerprint

    -- ** RouterAggregatedList
    , RouterAggregatedList
    , routerAggregatedList
    , ralNextPageToken
    , ralKind
    , ralItems
    , ralSelfLink
    , ralWarning
    , ralId

    -- ** FirewallList
    , FirewallList
    , firewallList
    , flNextPageToken
    , flKind
    , flItems
    , flSelfLink
    , flWarning
    , flId

    -- ** InstancesScopedListWarning
    , InstancesScopedListWarning
    , instancesScopedListWarning
    , islwData
    , islwCode
    , islwMessage

    -- ** RegionInstanceGroupManagersRecreateRequest
    , RegionInstanceGroupManagersRecreateRequest
    , regionInstanceGroupManagersRecreateRequest
    , rigmrrInstances

    -- ** InstanceLabels
    , InstanceLabels
    , instanceLabels
    , ilAddtional

    -- ** InstanceListWarningCode
    , InstanceListWarningCode (..)

    -- ** BackendServicesScopedListWarning
    , BackendServicesScopedListWarning
    , backendServicesScopedListWarning
    , bsslwData
    , bsslwCode
    , bsslwMessage

    -- ** InstanceGroupList
    , InstanceGroupList
    , instanceGroupList
    , iglNextPageToken
    , iglKind
    , iglItems
    , iglSelfLink
    , iglWarning
    , iglId

    -- ** InstancesSetMachineTypeRequest
    , InstancesSetMachineTypeRequest
    , instancesSetMachineTypeRequest
    , ismtrMachineType

    -- ** CustomerEncryptionKey
    , CustomerEncryptionKey
    , customerEncryptionKey
    , cekSha256
    , cekRawKey

    -- ** AutoscalerAggregatedListItems
    , AutoscalerAggregatedListItems
    , autoscalerAggregatedListItems
    , aaliAddtional

    -- ** InstanceListWarningDataItem
    , InstanceListWarningDataItem
    , instanceListWarningDataItem
    , ilwdiValue
    , ilwdiKey

    -- ** InstanceGroupManagersSetInstanceTemplateRequest
    , InstanceGroupManagersSetInstanceTemplateRequest
    , instanceGroupManagersSetInstanceTemplateRequest
    , igmsitrInstanceTemplate

    -- ** DeprecationStatus
    , DeprecationStatus
    , deprecationStatus
    , dsState
    , dsDeleted
    , dsReplacement
    , dsObsolete
    , dsDeprecated

    -- ** HTTPSHealthCheckListWarningDataItem
    , HTTPSHealthCheckListWarningDataItem
    , httpsHealthCheckListWarningDataItem
    , hhclwdiValue
    , hhclwdiKey

    -- ** OperationWarningsItemCode
    , OperationWarningsItemCode (..)

    -- ** Snapshot
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

    -- ** RouterStatus
    , RouterStatus
    , routerStatus
    , rsBestRoutesForRouter
    , rsBGPPeerStatus
    , rsNetwork
    , rsBestRoutes

    -- ** AutoscalingPolicyCustomMetricUtilization
    , AutoscalingPolicyCustomMetricUtilization
    , autoscalingPolicyCustomMetricUtilization
    , apcmuUtilizationTarget
    , apcmuMetric
    , apcmuUtilizationTargetType

    -- ** ForwardingRuleList
    , ForwardingRuleList
    , forwardingRuleList
    , frlNextPageToken
    , frlKind
    , frlItems
    , frlSelfLink
    , frlWarning
    , frlId

    -- ** VPNTunnelsScopedList
    , VPNTunnelsScopedList
    , vpnTunnelsScopedList
    , vtslVPNTunnels
    , vtslWarning

    -- ** SubnetworkSecondaryRange
    , SubnetworkSecondaryRange
    , subnetworkSecondaryRange
    , ssrRangeName
    , ssrIPCIdRRange

    -- ** BackendServiceProtocol
    , BackendServiceProtocol (..)

    -- ** RegionInstanceGroupsListInstancesWarning
    , RegionInstanceGroupsListInstancesWarning
    , regionInstanceGroupsListInstancesWarning
    , rigliwData
    , rigliwCode
    , rigliwMessage

    -- ** BackendBucketListWarningCode
    , BackendBucketListWarningCode (..)

    -- ** AcceleratorTypeAggregatedListWarningDataItem
    , AcceleratorTypeAggregatedListWarningDataItem
    , acceleratorTypeAggregatedListWarningDataItem
    , atalwdiValue
    , atalwdiKey

    -- ** InstanceGroupsSetNamedPortsRequest
    , InstanceGroupsSetNamedPortsRequest
    , instanceGroupsSetNamedPortsRequest
    , igsnprFingerprint
    , igsnprNamedPorts

    -- ** AcceleratorTypesScopedListWarningCode
    , AcceleratorTypesScopedListWarningCode (..)

    -- ** OperationList
    , OperationList
    , operationList
    , olNextPageToken
    , olKind
    , olItems
    , olSelfLink
    , olWarning
    , olId

    -- ** DiskList
    , DiskList
    , diskList
    , dlNextPageToken
    , dlKind
    , dlItems
    , dlSelfLink
    , dlWarning
    , dlId

    -- ** TargetPoolsAddInstanceRequest
    , TargetPoolsAddInstanceRequest
    , targetPoolsAddInstanceRequest
    , tpairInstances

    -- ** RegionAutoscalerList
    , RegionAutoscalerList
    , regionAutoscalerList
    , rNextPageToken
    , rKind
    , rItems
    , rSelfLink
    , rWarning
    , rId

    -- ** HealthCheckListWarningDataItem
    , HealthCheckListWarningDataItem
    , healthCheckListWarningDataItem
    , hclwdiValue
    , hclwdiKey

    -- ** InstanceGroupsAddInstancesRequest
    , InstanceGroupsAddInstancesRequest
    , instanceGroupsAddInstancesRequest
    , igairInstances

    -- ** TargetTCPProxiesSetProxyHeaderRequestProxyHeader
    , TargetTCPProxiesSetProxyHeaderRequestProxyHeader (..)

    -- ** InstanceGroupManagerList
    , InstanceGroupManagerList
    , instanceGroupManagerList
    , igmlNextPageToken
    , igmlKind
    , igmlItems
    , igmlSelfLink
    , igmlWarning
    , igmlId

    -- ** SubnetworksScopedListWarning
    , SubnetworksScopedListWarning
    , subnetworksScopedListWarning
    , sslwData
    , sslwCode
    , sslwMessage

    -- ** AcceleratorTypeListWarning
    , AcceleratorTypeListWarning
    , acceleratorTypeListWarning
    , atlwData
    , atlwCode
    , atlwMessage

    -- ** AttachedDiskType
    , AttachedDiskType (..)

    -- ** Image
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

    -- ** AcceleratorTypeAggregatedListWarningCode
    , AcceleratorTypeAggregatedListWarningCode (..)

    -- ** NetworksAddPeeringRequest
    , NetworksAddPeeringRequest
    , networksAddPeeringRequest
    , naprPeerNetwork
    , naprName
    , naprAutoCreateRoutes

    -- ** URLMapListWarning
    , URLMapListWarning
    , urlMapListWarning
    , umlwData
    , umlwCode
    , umlwMessage

    -- ** URLMap
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

    -- ** ImageListWarning
    , ImageListWarning
    , imageListWarning
    , ilwData
    , ilwCode
    , ilwMessage

    -- ** HealthCheckListWarningCode
    , HealthCheckListWarningCode (..)

    -- ** InstanceGroupAggregatedListItems
    , InstanceGroupAggregatedListItems
    , instanceGroupAggregatedListItems
    , igaliAddtional

    -- ** TargetPoolList
    , TargetPoolList
    , targetPoolList
    , tplNextPageToken
    , tplKind
    , tplItems
    , tplSelfLink
    , tplWarning
    , tplId

    -- ** AcceleratorType
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

    -- ** BackendBucketListWarningDataItem
    , BackendBucketListWarningDataItem
    , backendBucketListWarningDataItem
    , bblwdiValue
    , bblwdiKey

    -- ** AcceleratorTypesScopedListWarningDataItem
    , AcceleratorTypesScopedListWarningDataItem
    , acceleratorTypesScopedListWarningDataItem
    , atslwdiValue
    , atslwdiKey

    -- ** InterconnectOutageNotificationSource
    , InterconnectOutageNotificationSource (..)

    -- ** TargetInstanceAggregatedList
    , TargetInstanceAggregatedList
    , targetInstanceAggregatedList
    , tialNextPageToken
    , tialKind
    , tialItems
    , tialSelfLink
    , tialWarning
    , tialId

    -- ** InterconnectLinkType
    , InterconnectLinkType (..)

    -- ** DisksScopedList
    , DisksScopedList
    , disksScopedList
    , dslWarning
    , dslDisks

    -- ** InterconnectLocationListWarningDataItem
    , InterconnectLocationListWarningDataItem
    , interconnectLocationListWarningDataItem
    , illwdiValue
    , illwdiKey

    -- ** InstanceGroupManagersScopedList
    , InstanceGroupManagersScopedList
    , instanceGroupManagersScopedList
    , igmslWarning
    , igmslInstanceGroupManagers

    -- ** SubnetworkListWarning
    , SubnetworkListWarning
    , subnetworkListWarning
    , slwData
    , slwCode
    , slwMessage

    -- ** HealthCheck
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

    -- ** TargetSSLProxyProxyHeader
    , TargetSSLProxyProxyHeader (..)

    -- ** HTTPHealthCheckListWarning
    , HTTPHealthCheckListWarning
    , hTTPHealthCheckListWarning
    , httphclwData
    , httphclwCode
    , httphclwMessage

    -- ** CommitmentAggregatedListItems
    , CommitmentAggregatedListItems
    , commitmentAggregatedListItems
    , caliAddtional

    -- ** DiskAggregatedList
    , DiskAggregatedList
    , diskAggregatedList
    , dalNextPageToken
    , dalKind
    , dalItems
    , dalSelfLink
    , dalWarning
    , dalId

    -- ** InstanceGroupsListInstancesWarningDataItem
    , InstanceGroupsListInstancesWarningDataItem
    , instanceGroupsListInstancesWarningDataItem
    , igliwdiValue
    , igliwdiKey

    -- ** InstanceWithNamedPorts
    , InstanceWithNamedPorts
    , instanceWithNamedPorts
    , iwnpStatus
    , iwnpNamedPorts
    , iwnpInstance

    -- ** InterconnectListWarning
    , InterconnectListWarning
    , interconnectListWarning
    , iData
    , iCode
    , iMessage

    -- ** ForwardingRulesScopedList
    , ForwardingRulesScopedList
    , forwardingRulesScopedList
    , frslWarning
    , frslForwardingRules

    -- ** InstanceReference
    , InstanceReference
    , instanceReference
    , iInstance

    -- ** OperationAggregatedList
    , OperationAggregatedList
    , operationAggregatedList
    , oalNextPageToken
    , oalKind
    , oalItems
    , oalSelfLink
    , oalWarning
    , oalId

    -- ** OperationsScopedList
    , OperationsScopedList
    , operationsScopedList
    , oslWarning
    , oslOperations

    -- ** NamedPort
    , NamedPort
    , namedPort
    , npName
    , npPort

    -- ** InterconnectLocationListWarningCode
    , InterconnectLocationListWarningCode (..)

    -- ** InstanceGroupsListInstancesWarningCode
    , InstanceGroupsListInstancesWarningCode (..)

    -- ** RegionInstanceGroupsListInstancesRequestInstanceState
    , RegionInstanceGroupsListInstancesRequestInstanceState (..)

    -- ** SubnetworkAggregatedListWarningDataItem
    , SubnetworkAggregatedListWarningDataItem
    , subnetworkAggregatedListWarningDataItem
    , salwdiValue
    , salwdiKey

    -- ** ProjectXpnProjectStatus
    , ProjectXpnProjectStatus (..)

    -- ** TargetInstanceList
    , TargetInstanceList
    , targetInstanceList
    , tilNextPageToken
    , tilKind
    , tilItems
    , tilSelfLink
    , tilWarning
    , tilId

    -- ** TargetTCPProxyListWarning
    , TargetTCPProxyListWarning
    , targetTCPProxyListWarning
    , ttplwData
    , ttplwCode
    , ttplwMessage

    -- ** InstanceGroupManagerAggregatedList
    , InstanceGroupManagerAggregatedList
    , instanceGroupManagerAggregatedList
    , igmalNextPageToken
    , igmalKind
    , igmalItems
    , igmalSelfLink
    , igmalWarning
    , igmalId

    -- ** BackendBucket
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

    -- ** ProjectsEnableXpnResourceRequest
    , ProjectsEnableXpnResourceRequest
    , projectsEnableXpnResourceRequest
    , pexrrXpnResource

    -- ** ImageSourceType
    , ImageSourceType (..)

    -- ** TargetPoolsScopedList
    , TargetPoolsScopedList
    , targetPoolsScopedList
    , tpslWarning
    , tpslTargetPools

    -- ** InterconnectAttachmentAggregatedListItems
    , InterconnectAttachmentAggregatedListItems
    , interconnectAttachmentAggregatedListItems
    , iaaliAddtional

    -- ** SubnetworkAggregatedListWarningCode
    , SubnetworkAggregatedListWarningCode (..)

    -- ** InstancesSetMinCPUPlatformRequest
    , InstancesSetMinCPUPlatformRequest
    , instancesSetMinCPUPlatformRequest
    , ismcprMinCPUPlatform

    -- ** ForwardingRuleAggregatedList
    , ForwardingRuleAggregatedList
    , forwardingRuleAggregatedList
    , fralNextPageToken
    , fralKind
    , fralItems
    , fralSelfLink
    , fralWarning
    , fralId

    -- ** TargetReference
    , TargetReference
    , targetReference
    , trTarget

    -- ** TargetPoolAggregatedList
    , TargetPoolAggregatedList
    , targetPoolAggregatedList
    , tpalNextPageToken
    , tpalKind
    , tpalItems
    , tpalSelfLink
    , tpalWarning
    , tpalId

    -- ** OperationsScopedListWarningDataItem
    , OperationsScopedListWarningDataItem
    , operationsScopedListWarningDataItem
    , oslwdiValue
    , oslwdiKey

    -- ** BackendServiceSessionAffinity
    , BackendServiceSessionAffinity (..)

    -- ** GlobalSetLabelsRequest
    , GlobalSetLabelsRequest
    , globalSetLabelsRequest
    , gslrLabels
    , gslrLabelFingerprint

    -- ** TargetPool
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

    -- ** ImageList
    , ImageList
    , imageList
    , ilNextPageToken
    , ilKind
    , ilItems
    , ilSelfLink
    , ilWarning
    , ilId

    -- ** OperationAggregatedListWarningDataItem
    , OperationAggregatedListWarningDataItem
    , operationAggregatedListWarningDataItem
    , oalwdiValue
    , oalwdiKey

    -- ** ForwardingRuleListWarning
    , ForwardingRuleListWarning
    , forwardingRuleListWarning
    , frlwData
    , frlwCode
    , frlwMessage

    -- ** VPNTunnelsScopedListWarning
    , VPNTunnelsScopedListWarning
    , vpnTunnelsScopedListWarning
    , vtslwData
    , vtslwCode
    , vtslwMessage

    -- ** ForwardingRulesScopedListWarningCode
    , ForwardingRulesScopedListWarningCode (..)

    -- ** OperationsScopedListWarningCode
    , OperationsScopedListWarningCode (..)

    -- ** OperationAggregatedListWarningCode
    , OperationAggregatedListWarningCode (..)

    -- ** TargetSSLProxiesSetBackendServiceRequest
    , TargetSSLProxiesSetBackendServiceRequest
    , targetSSLProxiesSetBackendServiceRequest
    , tspsbsrService

    -- ** ForwardingRule
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

    -- ** URLMapList
    , URLMapList
    , urlMapList
    , umlNextPageToken
    , umlKind
    , umlItems
    , umlSelfLink
    , umlWarning
    , umlId

    -- ** ForwardingRulesScopedListWarningDataItem
    , ForwardingRulesScopedListWarningDataItem
    , forwardingRulesScopedListWarningDataItem
    , frslwdiValue
    , frslwdiKey

    -- ** TargetInstanceAggregatedListWarning
    , TargetInstanceAggregatedListWarning
    , targetInstanceAggregatedListWarning
    , tialwData
    , tialwCode
    , tialwMessage

    -- ** InstanceGroupManagersScopedListWarningDataItem
    , InstanceGroupManagersScopedListWarningDataItem
    , instanceGroupManagersScopedListWarningDataItem
    , igmslwdiValue
    , igmslwdiKey

    -- ** SubnetworksScopedList
    , SubnetworksScopedList
    , subnetworksScopedList
    , sslSubnetworks
    , sslWarning

    -- ** DiskAggregatedListWarningCode
    , DiskAggregatedListWarningCode (..)

    -- ** AcceleratorTypeList
    , AcceleratorTypeList
    , acceleratorTypeList
    , atlNextPageToken
    , atlKind
    , atlItems
    , atlSelfLink
    , atlWarning
    , atlId

    -- ** DiskAggregatedListWarningDataItem
    , DiskAggregatedListWarningDataItem
    , diskAggregatedListWarningDataItem
    , dalwdiValue
    , dalwdiKey

    -- ** TargetPoolListWarning
    , TargetPoolListWarning
    , targetPoolListWarning
    , tplwData
    , tplwCode
    , tplwMessage

    -- ** TargetPoolAggregatedListWarningDataItem
    , TargetPoolAggregatedListWarningDataItem
    , targetPoolAggregatedListWarningDataItem
    , tpalwdiValue
    , tpalwdiKey

    -- ** DisksScopedListWarningCode
    , DisksScopedListWarningCode (..)

    -- ** Project
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

    -- ** RegionInstanceGroupManagersListInstancesResponse
    , RegionInstanceGroupManagersListInstancesResponse
    , regionInstanceGroupManagersListInstancesResponse
    , rigmlirManagedInstances

    -- ** Operation
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

    -- ** DisksScopedListWarningDataItem
    , DisksScopedListWarningDataItem
    , disksScopedListWarningDataItem
    , dslwdiValue
    , dslwdiKey

    -- ** InstanceGroupManagersScopedListWarningCode
    , InstanceGroupManagersScopedListWarningCode (..)

    -- ** Disk
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

    -- ** DiskMoveRequest
    , DiskMoveRequest
    , diskMoveRequest
    , dmrTargetDisk
    , dmrDestinationZone

    -- ** ForwardingRuleAggregatedListWarningCode
    , ForwardingRuleAggregatedListWarningCode (..)

    -- ** AutoscalingPolicyLoadBalancingUtilization
    , AutoscalingPolicyLoadBalancingUtilization
    , autoscalingPolicyLoadBalancingUtilization
    , aplbuUtilizationTarget

    -- ** TargetPoolAggregatedListWarningCode
    , TargetPoolAggregatedListWarningCode (..)

    -- ** ForwardingRuleAggregatedListWarningDataItem
    , ForwardingRuleAggregatedListWarningDataItem
    , forwardingRuleAggregatedListWarningDataItem
    , fralwdiValue
    , fralwdiKey

    -- ** TargetPoolsScopedListWarningDataItem
    , TargetPoolsScopedListWarningDataItem
    , targetPoolsScopedListWarningDataItem
    , tpslwdiValue
    , tpslwdiKey

    -- ** InstanceGroupManager
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

    -- ** InstanceGroupManagerListWarning
    , InstanceGroupManagerListWarning
    , instanceGroupManagerListWarning
    , igmlwData
    , igmlwCode
    , igmlwMessage

    -- ** RegionInstanceGroupsListInstances
    , RegionInstanceGroupsListInstances
    , regionInstanceGroupsListInstances
    , rigliNextPageToken
    , rigliKind
    , rigliItems
    , rigliSelfLink
    , rigliWarning
    , rigliId

    -- ** DiskListWarning
    , DiskListWarning
    , diskListWarning
    , dlwData
    , dlwCode
    , dlwMessage

    -- ** TCPHealthCheck
    , TCPHealthCheck
    , tcpHealthCheck
    , thcResponse
    , thcProxyHeader
    , thcPortName
    , thcPort
    , thcRequest

    -- ** InstanceGroupManagerAggregatedListWarningDataItem
    , InstanceGroupManagerAggregatedListWarningDataItem
    , instanceGroupManagerAggregatedListWarningDataItem
    , igmalwdiValue
    , igmalwdiKey

    -- ** TargetPoolsScopedListWarningCode
    , TargetPoolsScopedListWarningCode (..)

    -- ** RegionAutoscalerListWarning
    , RegionAutoscalerListWarning
    , regionAutoscalerListWarning
    , ralwData
    , ralwCode
    , ralwMessage

    -- ** TargetInstanceListWarningCode
    , TargetInstanceListWarningCode (..)

    -- ** InterconnectLocationRegionInfoLocationPresence
    , InterconnectLocationRegionInfoLocationPresence (..)

    -- ** SSLHealthCheckProxyHeader
    , SSLHealthCheckProxyHeader (..)

    -- ** TargetVPNGatewayStatus
    , TargetVPNGatewayStatus (..)

    -- ** TargetInstanceListWarningDataItem
    , TargetInstanceListWarningDataItem
    , targetInstanceListWarningDataItem
    , tilwdiValue
    , tilwdiKey

    -- ** InstanceGroupManagerAggregatedListWarningCode
    , InstanceGroupManagerAggregatedListWarningCode (..)

    -- ** OperationListWarning
    , OperationListWarning
    , operationListWarning
    , olwData
    , olwCode
    , olwMessage

    -- ** InstanceGroupsRemoveInstancesRequest
    , InstanceGroupsRemoveInstancesRequest
    , instanceGroupsRemoveInstancesRequest
    , igrirInstances

    -- ** SnapshotStatus
    , SnapshotStatus (..)

    -- ** MachineTypeListWarningDataItem
    , MachineTypeListWarningDataItem
    , machineTypeListWarningDataItem
    , mtlwdiValue
    , mtlwdiKey

    -- ** NetworksRemovePeeringRequest
    , NetworksRemovePeeringRequest
    , networksRemovePeeringRequest
    , nrprName

    -- ** TargetHTTPProxyListWarningDataItem
    , TargetHTTPProxyListWarningDataItem
    , targetHTTPProxyListWarningDataItem
    , thttpplwdiValue
    , thttpplwdiKey

    -- ** MachineTypeAggregatedListWarning
    , MachineTypeAggregatedListWarning
    , machineTypeAggregatedListWarning
    , mtalwData
    , mtalwCode
    , mtalwMessage

    -- ** AutoscalerListWarningCode
    , AutoscalerListWarningCode (..)

    -- ** BackendBucketList
    , BackendBucketList
    , backendBucketList
    , bblNextPageToken
    , bblKind
    , bblItems
    , bblSelfLink
    , bblWarning
    , bblId

    -- ** InterconnectAttachmentPrivateInfo
    , InterconnectAttachmentPrivateInfo
    , interconnectAttachmentPrivateInfo
    , iapiTag8021q

    -- ** AcceleratorTypesScopedList
    , AcceleratorTypesScopedList
    , acceleratorTypesScopedList
    , atslAcceleratorTypes
    , atslWarning

    -- ** BackendServiceCdnPolicy
    , BackendServiceCdnPolicy
    , backendServiceCdnPolicy
    , bscpCacheKeyPolicy

    -- ** DiskTypeListWarningCode
    , DiskTypeListWarningCode (..)

    -- ** RouteWarningsItemDataItem
    , RouteWarningsItemDataItem
    , routeWarningsItemDataItem
    , rwidiValue
    , rwidiKey

    -- ** TargetInstancesScopedListWarningCode
    , TargetInstancesScopedListWarningCode (..)

    -- ** VPNTunnelListWarning
    , VPNTunnelListWarning
    , vpnTunnelListWarning
    , vtlwData
    , vtlwCode
    , vtlwMessage

    -- ** BackendServiceAggregatedListItems
    , BackendServiceAggregatedListItems
    , backendServiceAggregatedListItems
    , bsaliAddtional

    -- ** VPNTunnelAggregatedListWarningCode
    , VPNTunnelAggregatedListWarningCode (..)

    -- ** InstanceAggregatedListItems
    , InstanceAggregatedListItems
    , instanceAggregatedListItems
    , ialiAddtional

    -- ** AutoscalersScopedListWarning
    , AutoscalersScopedListWarning
    , autoscalersScopedListWarning
    , aslwData
    , aslwCode
    , aslwMessage

    -- ** DiskTypeAggregatedListWarning
    , DiskTypeAggregatedListWarning
    , diskTypeAggregatedListWarning
    , dtalwData
    , dtalwCode
    , dtalwMessage

    -- ** DiskTypeListWarningDataItem
    , DiskTypeListWarningDataItem
    , diskTypeListWarningDataItem
    , dtlwdiValue
    , dtlwdiKey

    -- ** VPNTunnelAggregatedListWarningDataItem
    , VPNTunnelAggregatedListWarningDataItem
    , vpnTunnelAggregatedListWarningDataItem
    , vtalwdiValue
    , vtalwdiKey

    -- ** TargetHTTPProxyListWarningCode
    , TargetHTTPProxyListWarningCode (..)

    -- ** AutoscalerAggregatedListWarning
    , AutoscalerAggregatedListWarning
    , autoscalerAggregatedListWarning
    , aalwData
    , aalwCode
    , aalwMessage

    -- ** AutoscalerListWarningDataItem
    , AutoscalerListWarningDataItem
    , autoscalerListWarningDataItem
    , alwdiValue
    , alwdiKey

    -- ** MachineTypeListWarningCode
    , MachineTypeListWarningCode (..)

    -- ** HealthCheckList
    , HealthCheckList
    , healthCheckList
    , hclNextPageToken
    , hclKind
    , hclItems
    , hclSelfLink
    , hclWarning
    , hclId

    -- ** ManagedInstanceLastAttemptErrors
    , ManagedInstanceLastAttemptErrors
    , managedInstanceLastAttemptErrors
    , milaeErrors

    -- ** GuestOSFeatureType
    , GuestOSFeatureType (..)

    -- ** RouteWarningsItemCode
    , RouteWarningsItemCode (..)

    -- ** TargetPoolsRemoveInstanceRequest
    , TargetPoolsRemoveInstanceRequest
    , targetPoolsRemoveInstanceRequest
    , tprirInstances

    -- ** TargetInstancesScopedListWarningDataItem
    , TargetInstancesScopedListWarningDataItem
    , targetInstancesScopedListWarningDataItem
    , tislwdiValue
    , tislwdiKey

    -- ** MachineTypesScopedListWarning
    , MachineTypesScopedListWarning
    , machineTypesScopedListWarning
    , mtslwData
    , mtslwCode
    , mtslwMessage

    -- ** ZoneSetLabelsRequest
    , ZoneSetLabelsRequest
    , zoneSetLabelsRequest
    , zslrLabels
    , zslrLabelFingerprint

    -- ** TargetInstance
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

    -- ** TargetPoolInstanceHealth
    , TargetPoolInstanceHealth
    , targetPoolInstanceHealth
    , tpihKind
    , tpihHealthStatus

    -- ** RegionInstanceGroupManagerListWarningCode
    , RegionInstanceGroupManagerListWarningCode (..)

    -- ** AcceleratorTypeAggregatedList
    , AcceleratorTypeAggregatedList
    , acceleratorTypeAggregatedList
    , atalNextPageToken
    , atalKind
    , atalItems
    , atalSelfLink
    , atalWarning
    , atalId

    -- ** SnapshotStorageBytesStatus
    , SnapshotStorageBytesStatus (..)

    -- ** NetworkRoutingConfig
    , NetworkRoutingConfig
    , networkRoutingConfig
    , nrcRoutingMode

    -- ** InstanceGroupManagersListManagedInstancesResponse
    , InstanceGroupManagersListManagedInstancesResponse
    , instanceGroupManagersListManagedInstancesResponse
    , igmlmirManagedInstances

    -- ** InstanceProperties
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

    -- ** ProjectsListXpnHostsRequest
    , ProjectsListXpnHostsRequest
    , projectsListXpnHostsRequest
    , plxhrOrganization

    -- ** RegionListWarning
    , RegionListWarning
    , regionListWarning
    , rlwData
    , rlwCode
    , rlwMessage

    -- ** DiskTypesScopedListWarning
    , DiskTypesScopedListWarning
    , diskTypesScopedListWarning
    , dtslwData
    , dtslwCode
    , dtslwMessage

    -- ** RegionInstanceGroupManagerListWarningDataItem
    , RegionInstanceGroupManagerListWarningDataItem
    , regionInstanceGroupManagerListWarningDataItem
    , rigmlwdiValue
    , rigmlwdiKey

    -- ** AddressesScopedListWarningCode
    , AddressesScopedListWarningCode (..)

    -- ** AttachedDiskInitializeParams
    , AttachedDiskInitializeParams
    , attachedDiskInitializeParams
    , adipSourceImage
    , adipDiskSizeGb
    , adipDiskName
    , adipSourceImageEncryptionKey
    , adipDiskType

    -- ** AddressesScopedListWarningDataItem
    , AddressesScopedListWarningDataItem
    , addressesScopedListWarningDataItem
    , aslwdiValue
    , aslwdiKey

    -- ** ImageStatus
    , ImageStatus (..)

    -- ** InstanceGroupListWarningCode
    , InstanceGroupListWarningCode (..)

    -- ** InstanceGroupListWarningDataItem
    , InstanceGroupListWarningDataItem
    , instanceGroupListWarningDataItem
    , iglwdiValue
    , iglwdiKey

    -- ** NetworkInterface
    , NetworkInterface
    , networkInterface
    , niKind
    , niAliasIPRanges
    , niNetwork
    , niName
    , niNetworkIP
    , niSubnetwork
    , niAccessConfigs

    -- ** FirewallListWarningCode
    , FirewallListWarningCode (..)

    -- ** RouterListWarning
    , RouterListWarning
    , routerListWarning
    , rData
    , rCode
    , rMessage

    -- ** FirewallListWarningDataItem
    , FirewallListWarningDataItem
    , firewallListWarningDataItem
    , flwdiValue
    , flwdiKey

    -- ** TargetPoolsRemoveHealthCheckRequest
    , TargetPoolsRemoveHealthCheckRequest
    , targetPoolsRemoveHealthCheckRequest
    , tprhcrHealthChecks

    -- ** AutoscalerStatusDetailsType
    , AutoscalerStatusDetailsType (..)

    -- ** FirewallDirection
    , FirewallDirection (..)

    -- ** RegionInstanceGroupManagersSetTargetPoolsRequest
    , RegionInstanceGroupManagersSetTargetPoolsRequest
    , regionInstanceGroupManagersSetTargetPoolsRequest
    , rigmstprFingerprint
    , rigmstprTargetPools

    -- ** TargetSSLProxyList
    , TargetSSLProxyList
    , targetSSLProxyList
    , tsplNextPageToken
    , tsplKind
    , tsplItems
    , tsplSelfLink
    , tsplWarning
    , tsplId

    -- ** CustomerEncryptionKeyProtectedDisk
    , CustomerEncryptionKeyProtectedDisk
    , customerEncryptionKeyProtectedDisk
    , cekpdDiskEncryptionKey
    , cekpdSource

    -- ** HealthStatusHealthState
    , HealthStatusHealthState (..)

    -- ** InstanceTemplateList
    , InstanceTemplateList
    , instanceTemplateList
    , itlNextPageToken
    , itlKind
    , itlItems
    , itlSelfLink
    , itlWarning
    , itlId

    -- ** InstanceGroupAggregatedListWarning
    , InstanceGroupAggregatedListWarning
    , instanceGroupAggregatedListWarning
    , igalwData
    , igalwCode
    , igalwMessage

    -- ** RouteList
    , RouteList
    , routeList
    , rlNextPageToken
    , rlKind
    , rlItems
    , rlSelfLink
    , rlWarning
    , rlId

    -- ** DeprecationStatusState
    , DeprecationStatusState (..)

    -- ** Router
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

    -- ** RouterAggregatedListWarningCode
    , RouterAggregatedListWarningCode (..)

    -- ** RoutersScopedListWarningCode
    , RoutersScopedListWarningCode (..)

    -- ** RouterAggregatedListWarningDataItem
    , RouterAggregatedListWarningDataItem
    , routerAggregatedListWarningDataItem
    , ralwdiValue
    , ralwdiKey

    -- ** TargetHTTPSProxyListWarningCode
    , TargetHTTPSProxyListWarningCode (..)

    -- ** RoutersScopedListWarningDataItem
    , RoutersScopedListWarningDataItem
    , routersScopedListWarningDataItem
    , rslwdiValue
    , rslwdiKey

    -- ** SubnetworksSetPrivateIPGoogleAccessRequest
    , SubnetworksSetPrivateIPGoogleAccessRequest
    , subnetworksSetPrivateIPGoogleAccessRequest
    , sspigarPrivateIPGoogleAccess

    -- ** TargetHTTPSProxyListWarningDataItem
    , TargetHTTPSProxyListWarningDataItem
    , targetHTTPSProxyListWarningDataItem
    , thplwdiValue
    , thplwdiKey

    -- ** ManagedInstanceCurrentAction
    , ManagedInstanceCurrentAction (..)

    -- ** TargetVPNGatewayList
    , TargetVPNGatewayList
    , targetVPNGatewayList
    , tvglNextPageToken
    , tvglKind
    , tvglItems
    , tvglSelfLink
    , tvglWarning
    , tvglId

    -- ** TargetInstanceNATPolicy
    , TargetInstanceNATPolicy (..)

    -- ** SSLCertificateList
    , SSLCertificateList
    , sslCertificateList
    , sclNextPageToken
    , sclKind
    , sclItems
    , sclSelfLink
    , sclWarning
    , sclId

    -- ** FirewallAllowedItem
    , FirewallAllowedItem
    , firewallAllowedItem
    , faiIPProtocol
    , faiPorts

    -- ** RouterListWarningDataItem
    , RouterListWarningDataItem
    , routerListWarningDataItem
    , rlwdiValue
    , rlwdiKey

    -- ** BackendServiceAggregatedList
    , BackendServiceAggregatedList
    , backendServiceAggregatedList
    , bsalNextPageToken
    , bsalKind
    , bsalItems
    , bsalSelfLink
    , bsalWarning
    , bsalId

    -- ** Network
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

    -- ** RoutersScopedListWarning
    , RoutersScopedListWarning
    , routersScopedListWarning
    , rslwData
    , rslwCode
    , rslwMessage

    -- ** AccessConfigType
    , AccessConfigType (..)

    -- ** TargetHTTPSProxyListWarning
    , TargetHTTPSProxyListWarning
    , targetHTTPSProxyListWarning
    , thplwData
    , thplwCode
    , thplwMessage

    -- ** AddressesScopedListWarning
    , AddressesScopedListWarning
    , addressesScopedListWarning
    , aData
    , aCode
    , aMessage

    -- ** InstanceGroupAggregatedListWarningDataItem
    , InstanceGroupAggregatedListWarningDataItem
    , instanceGroupAggregatedListWarningDataItem
    , igalwdiValue
    , igalwdiKey

    -- ** ForwardingRuleIPVersion
    , ForwardingRuleIPVersion (..)

    -- ** RouterListWarningCode
    , RouterListWarningCode (..)

    -- ** ImageRawDisk
    , ImageRawDisk
    , imageRawDisk
    , irdContainerType
    , irdSource
    , irdSha1Checksum

    -- ** InstanceAggregatedList
    , InstanceAggregatedList
    , instanceAggregatedList
    , ialNextPageToken
    , ialKind
    , ialItems
    , ialSelfLink
    , ialWarning
    , ialId

    -- ** TargetTCPProxiesSetBackendServiceRequest
    , TargetTCPProxiesSetBackendServiceRequest
    , targetTCPProxiesSetBackendServiceRequest
    , ttpsbsrService

    -- ** SSLHealthCheck
    , SSLHealthCheck
    , sslHealthCheck
    , shcResponse
    , shcProxyHeader
    , shcPortName
    , shcPort
    , shcRequest

    -- ** Address
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

    -- ** InstanceGroupAggregatedListWarningCode
    , InstanceGroupAggregatedListWarningCode (..)

    -- ** InstanceGroupListWarning
    , InstanceGroupListWarning
    , instanceGroupListWarning
    , iglwData
    , iglwCode
    , iglwMessage

    -- ** Zone
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

    -- ** RouterBGP
    , RouterBGP
    , routerBGP
    , rbASN

    -- ** FirewallListWarning
    , FirewallListWarning
    , firewallListWarning
    , flwData
    , flwCode
    , flwMessage

    -- ** BackendServicesScopedList
    , BackendServicesScopedList
    , backendServicesScopedList
    , bsslWarning
    , bsslBackendServices

    -- ** InstanceGroupManagersRecreateInstancesRequest
    , InstanceGroupManagersRecreateInstancesRequest
    , instanceGroupManagersRecreateInstancesRequest
    , igmrirInstances

    -- ** TargetSSLProxiesSetSSLCertificatesRequest
    , TargetSSLProxiesSetSSLCertificatesRequest
    , targetSSLProxiesSetSSLCertificatesRequest
    , tspsscrSSLCertificates

    -- ** RouterAggregatedListWarning
    , RouterAggregatedListWarning
    , routerAggregatedListWarning
    , rouData
    , rouCode
    , rouMessage

    -- ** InstancesScopedList
    , InstancesScopedList
    , instancesScopedList
    , islWarning
    , islInstances

    -- ** AcceleratorTypeAggregatedListItems
    , AcceleratorTypeAggregatedListItems
    , acceleratorTypeAggregatedListItems
    , ataliAddtional

    -- ** TargetVPNGatewayAggregatedListWarningDataItem
    , TargetVPNGatewayAggregatedListWarningDataItem
    , targetVPNGatewayAggregatedListWarningDataItem
    , tvgalwdiValue
    , tvgalwdiKey

    -- ** BackendServiceLoadBalancingScheme
    , BackendServiceLoadBalancingScheme (..)

    -- ** HealthCheckReference
    , HealthCheckReference
    , healthCheckReference
    , hcrHealthCheck

    -- ** TargetInstanceAggregatedListItems
    , TargetInstanceAggregatedListItems
    , targetInstanceAggregatedListItems
    , tialiAddtional

    -- ** TargetSSLProxyListWarning
    , TargetSSLProxyListWarning
    , targetSSLProxyListWarning
    , tsplwData
    , tsplwCode
    , tsplwMessage

    -- ** InstanceTemplateListWarning
    , InstanceTemplateListWarning
    , instanceTemplateListWarning
    , itlwData
    , itlwCode
    , itlwMessage

    -- ** RouteListWarning
    , RouteListWarning
    , routeListWarning
    , rlwlData
    , rlwlCode
    , rlwlMessage

    -- ** TargetTCPProxyProxyHeader
    , TargetTCPProxyProxyHeader (..)

    -- ** InstanceGroupAggregatedList
    , InstanceGroupAggregatedList
    , instanceGroupAggregatedList
    , igalNextPageToken
    , igalKind
    , igalItems
    , igalSelfLink
    , igalWarning
    , igalId

    -- ** TargetVPNGatewayAggregatedListWarningCode
    , TargetVPNGatewayAggregatedListWarningCode (..)

    -- ** OperationStatus
    , OperationStatus (..)

    -- ** Route
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

    -- ** TargetVPNGatewaysScopedListWarningDataItem
    , TargetVPNGatewaysScopedListWarningDataItem
    , targetVPNGatewaysScopedListWarningDataItem
    , tvgslwdiValue
    , tvgslwdiKey

    -- ** InterconnectLocationRegionInfo
    , InterconnectLocationRegionInfo
    , interconnectLocationRegionInfo
    , ilriLocationPresence
    , ilriExpectedRttMs
    , ilriRegion

    -- ** TargetVPNGatewaysScopedListWarningCode
    , TargetVPNGatewaysScopedListWarningCode (..)

    -- ** TargetHTTPSProxiesSetSSLCertificatesRequest
    , TargetHTTPSProxiesSetSSLCertificatesRequest
    , targetHTTPSProxiesSetSSLCertificatesRequest
    , thpsscrSSLCertificates

    -- ** InstanceTemplate
    , InstanceTemplate
    , instanceTemplate
    , itKind
    , itSelfLink
    , itName
    , itCreationTimestamp
    , itId
    , itDescription
    , itProperties

    -- ** XpnResourceId
    , XpnResourceId
    , xpnResourceId
    , xriId
    , xriType

    -- ** RouterList
    , RouterList
    , routerList
    , rllNextPageToken
    , rllKind
    , rllItems
    , rllSelfLink
    , rllWarning
    , rllId

    -- ** TargetSSLProxy
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

    -- ** SSLCertificateListWarning
    , SSLCertificateListWarning
    , sslCertificateListWarning
    , sclwData
    , sclwCode
    , sclwMessage

    -- ** SnapshotListWarningDataItem
    , SnapshotListWarningDataItem
    , snapshotListWarningDataItem
    , slwdiValue
    , slwdiKey

    -- ** TargetVPNGateway
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

    -- ** DiskStatus
    , DiskStatus (..)

    -- ** ResourceCommitment
    , ResourceCommitment
    , resourceCommitment
    , rcAmount
    , rcType

    -- ** SnapshotListWarningCode
    , SnapshotListWarningCode (..)

    -- ** BackendServiceIAP
    , BackendServiceIAP
    , backendServiceIAP
    , bsiapEnabled
    , bsiapOAuth2ClientSecretSha256
    , bsiapOAuth2ClientSecret
    , bsiapOAuth2ClientId

    -- ** TargetVPNGatewayListWarning
    , TargetVPNGatewayListWarning
    , targetVPNGatewayListWarning
    , tvglwData
    , tvglwCode
    , tvglwMessage

    -- ** ManagedInstanceInstanceStatus
    , ManagedInstanceInstanceStatus (..)

    -- ** HTTPHealthCheckProxyHeader
    , HTTPHealthCheckProxyHeader (..)

    -- ** InterconnectLocationContinent
    , InterconnectLocationContinent (..)

    -- ** URLMapsValidateResponse
    , URLMapsValidateResponse
    , urlMapsValidateResponse
    , umvrResult

    -- ** SSLCertificate
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

    -- ** RouterStatusBGPPeerStatus
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

    -- ** URLMapReference
    , URLMapReference
    , urlMapReference
    , umrURLMap

    -- ** AttachedDiskMode
    , AttachedDiskMode (..)

    -- ** TargetPoolsAddHealthCheckRequest
    , TargetPoolsAddHealthCheckRequest
    , targetPoolsAddHealthCheckRequest
    , tpahcrHealthChecks

    -- ** CommitmentsScopedList
    , CommitmentsScopedList
    , commitmentsScopedList
    , cslWarning
    , cslCommitments

    -- ** TargetVPNGatewayListWarningCode
    , TargetVPNGatewayListWarningCode (..)

    -- ** DiskAggregatedListItems
    , DiskAggregatedListItems
    , diskAggregatedListItems
    , daliAddtional

    -- ** TargetVPNGatewayAggregatedListWarning
    , TargetVPNGatewayAggregatedListWarning
    , targetVPNGatewayAggregatedListWarning
    , tvgalwData
    , tvgalwCode
    , tvgalwMessage

    -- ** UsageExportLocation
    , UsageExportLocation
    , usageExportLocation
    , uelReportNamePrefix
    , uelBucketName

    -- ** InstanceTemplateListWarningCode
    , InstanceTemplateListWarningCode (..)

    -- ** ZoneList
    , ZoneList
    , zoneList
    , zlNextPageToken
    , zlKind
    , zlItems
    , zlSelfLink
    , zlWarning
    , zlId

    -- ** SSLCertificateListWarningDataItem
    , SSLCertificateListWarningDataItem
    , sslCertificateListWarningDataItem
    , sclwdiValue
    , sclwdiKey

    -- ** TargetSSLProxyListWarningCode
    , TargetSSLProxyListWarningCode (..)

    -- ** RegionStatus
    , RegionStatus (..)

    -- ** TargetTCPProxiesSetProxyHeaderRequest
    , TargetTCPProxiesSetProxyHeaderRequest
    , targetTCPProxiesSetProxyHeaderRequest
    , ttpsphrProxyHeader

    -- ** RouterBGPPeer
    , RouterBGPPeer
    , routerBGPPeer
    , rbpIPAddress
    , rbpInterfaceName
    , rbpPeerIPAddress
    , rbpAdvertisedRoutePriority
    , rbpPeerASN
    , rbpName

    -- ** SubnetworksExpandIPCIdRRangeRequest
    , SubnetworksExpandIPCIdRRangeRequest
    , subnetworksExpandIPCIdRRangeRequest
    , seicirrrIPCIdRRange

    -- ** ManagedInstance
    , ManagedInstance
    , managedInstance
    , miLastAttempt
    , miCurrentAction
    , miId
    , miInstanceStatus
    , miInstance

    -- ** InstanceGroupManagerAggregatedListItems
    , InstanceGroupManagerAggregatedListItems
    , instanceGroupManagerAggregatedListItems
    , igmaliAddtional

    -- ** InstanceGroupManagersDeleteInstancesRequest
    , InstanceGroupManagersDeleteInstancesRequest
    , instanceGroupManagersDeleteInstancesRequest
    , igmdirInstances

    -- ** Backend
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

    -- ** TargetVPNGatewaysScopedListWarning
    , TargetVPNGatewaysScopedListWarning
    , targetVPNGatewaysScopedListWarning
    , tvgslwData
    , tvgslwCode
    , tvgslwMessage

    -- ** TargetSSLProxiesSetProxyHeaderRequestProxyHeader
    , TargetSSLProxiesSetProxyHeaderRequestProxyHeader (..)

    -- ** AddressList
    , AddressList
    , addressList
    , alNextPageToken
    , alKind
    , alItems
    , alSelfLink
    , alWarning
    , alId

    -- ** TargetVPNGatewayListWarningDataItem
    , TargetVPNGatewayListWarningDataItem
    , targetVPNGatewayListWarningDataItem
    , tvglwdiValue
    , tvglwdiKey

    -- ** ForwardingRuleAggregatedListItems
    , ForwardingRuleAggregatedListItems
    , forwardingRuleAggregatedListItems
    , fraliAddtional

    -- ** InterconnectAttachmentAggregatedList
    , InterconnectAttachmentAggregatedList
    , interconnectAttachmentAggregatedList
    , iaalNextPageToken
    , iaalKind
    , iaalItems
    , iaalSelfLink
    , iaalWarning
    , iaalId

    -- ** RouteListWarningCode
    , RouteListWarningCode (..)

    -- ** OperationAggregatedListItems
    , OperationAggregatedListItems
    , operationAggregatedListItems
    , oaliAddtional

    -- ** InstanceGroupManagerActionsSummary
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

    -- ** XpnHostList
    , XpnHostList
    , xpnHostList
    , xhlNextPageToken
    , xhlKind
    , xhlItems
    , xhlSelfLink
    , xhlWarning
    , xhlId

    -- ** VPNTunnelStatus
    , VPNTunnelStatus (..)

    -- ** InstanceTemplateListWarningDataItem
    , InstanceTemplateListWarningDataItem
    , instanceTemplateListWarningDataItem
    , itlwdiValue
    , itlwdiKey

    -- ** SSLCertificateListWarningCode
    , SSLCertificateListWarningCode (..)

    -- ** ServiceAccount
    , ServiceAccount
    , serviceAccount
    , saEmail
    , saScopes

    -- ** SnapshotListWarning
    , SnapshotListWarning
    , snapshotListWarning
    , sData
    , sCode
    , sMessage

    -- ** RegionInstanceGroupManagersAbandonInstancesRequest
    , RegionInstanceGroupManagersAbandonInstancesRequest
    , regionInstanceGroupManagersAbandonInstancesRequest
    , rigmairInstances

    -- ** NetworkList
    , NetworkList
    , networkList
    , nlNextPageToken
    , nlKind
    , nlItems
    , nlSelfLink
    , nlWarning
    , nlId

    -- ** NetworkPeering
    , NetworkPeering
    , networkPeering
    , netState
    , netStateDetails
    , netNetwork
    , netName
    , netAutoCreateRoutes

    -- ** TargetSSLProxyListWarningDataItem
    , TargetSSLProxyListWarningDataItem
    , targetSSLProxyListWarningDataItem
    , tsplwdiValue
    , tsplwdiKey

    -- ** InstanceGroupsListInstancesRequest
    , InstanceGroupsListInstancesRequest
    , instanceGroupsListInstancesRequest
    , iglirInstanceState

    -- ** RouteListWarningDataItem
    , RouteListWarningDataItem
    , routeListWarningDataItem
    , rValue
    , rKey

    -- ** InterconnectAttachmentsScopedList
    , InterconnectAttachmentsScopedList
    , interconnectAttachmentsScopedList
    , iaslWarning
    , iaslInterconnectAttachments

    -- ** BackendBalancingMode
    , BackendBalancingMode (..)

    -- ** CommitmentAggregatedList
    , CommitmentAggregatedList
    , commitmentAggregatedList
    , calNextPageToken
    , calKind
    , calItems
    , calSelfLink
    , calWarning
    , calId

    -- ** RegionInstanceGroupList
    , RegionInstanceGroupList
    , regionInstanceGroupList
    , riglNextPageToken
    , riglKind
    , riglItems
    , riglSelfLink
    , riglWarning
    , riglId

    -- ** TargetPoolAggregatedListItems
    , TargetPoolAggregatedListItems
    , targetPoolAggregatedListItems
    , tpaliAddtional

    -- ** TargetInstancesScopedList
    , TargetInstancesScopedList
    , targetInstancesScopedList
    , tislWarning
    , tislTargetInstances

    -- ** NetworkRoutingConfigRoutingMode
    , NetworkRoutingConfigRoutingMode (..)

    -- ** SubnetworkListWarningCode
    , SubnetworkListWarningCode (..)

    -- ** SubnetworkListWarningDataItem
    , SubnetworkListWarningDataItem
    , subnetworkListWarningDataItem
    , sValue
    , sKey

    -- ** ProjectsDisableXpnResourceRequest
    , ProjectsDisableXpnResourceRequest
    , projectsDisableXpnResourceRequest
    , pdxrrXpnResource

    -- ** CommitmentStatus
    , CommitmentStatus (..)

    -- ** AddressAggregatedListItems
    , AddressAggregatedListItems
    , addressAggregatedListItems
    , aAddtional

    -- ** InterconnectListWarningCode
    , InterconnectListWarningCode (..)

    -- ** AutoscalerList
    , AutoscalerList
    , autoscalerList
    , autNextPageToken
    , autKind
    , autItems
    , autSelfLink
    , autWarning
    , autId

    -- ** InterconnectListWarningDataItem
    , InterconnectListWarningDataItem
    , interconnectListWarningDataItem
    , iValue
    , iKey

    -- ** SubnetworkAggregatedListWarning
    , SubnetworkAggregatedListWarning
    , subnetworkAggregatedListWarning
    , salwData
    , salwCode
    , salwMessage

    -- ** InterconnectLocation
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

    -- ** TargetSSLProxiesSetProxyHeaderRequest
    , TargetSSLProxiesSetProxyHeaderRequest
    , targetSSLProxiesSetProxyHeaderRequest
    , tspsphrProxyHeader

    -- ** HTTPHealthCheckListWarningCode
    , HTTPHealthCheckListWarningCode (..)

    -- ** VPNTunnelAggregatedList
    , VPNTunnelAggregatedList
    , vpnTunnelAggregatedList
    , vtalNextPageToken
    , vtalKind
    , vtalItems
    , vtalSelfLink
    , vtalWarning
    , vtalId

    -- ** InterconnectCircuitInfo
    , InterconnectCircuitInfo
    , interconnectCircuitInfo
    , iciGoogleCircuitId
    , iciCustomerDemarcId
    , iciGoogleDemarcId

    -- ** AttachedDisk
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

    -- ** HTTPHealthCheckListWarningDataItem
    , HTTPHealthCheckListWarningDataItem
    , hTTPHealthCheckListWarningDataItem
    , httphclwdiValue
    , httphclwdiKey

    -- ** InterconnectAttachmentOperationalStatus
    , InterconnectAttachmentOperationalStatus (..)

    -- ** DiskTypeList
    , DiskTypeList
    , diskTypeList
    , dtlNextPageToken
    , dtlKind
    , dtlItems
    , dtlSelfLink
    , dtlWarning
    , dtlId

    -- ** TargetTCPProxyListWarningCode
    , TargetTCPProxyListWarningCode (..)

    -- ** RegionInstanceGroupsSetNamedPortsRequest
    , RegionInstanceGroupsSetNamedPortsRequest
    , regionInstanceGroupsSetNamedPortsRequest
    , rigsnprFingerprint
    , rigsnprNamedPorts

    -- ** TargetTCPProxyListWarningDataItem
    , TargetTCPProxyListWarningDataItem
    , targetTCPProxyListWarningDataItem
    , ttplwdiValue
    , ttplwdiKey

    -- ** ProjectsGetXpnResources
    , ProjectsGetXpnResources
    , projectsGetXpnResources
    , pgxrNextPageToken
    , pgxrKind
    , pgxrResources

    -- ** MachineTypeList
    , MachineTypeList
    , machineTypeList
    , mtlNextPageToken
    , mtlKind
    , mtlItems
    , mtlSelfLink
    , mtlWarning
    , mtlId

    -- ** TargetHTTPProxyList
    , TargetHTTPProxyList
    , targetHTTPProxyList
    , thttpplNextPageToken
    , thttpplKind
    , thttpplItems
    , thttpplSelfLink
    , thttpplWarning
    , thttpplId

    -- ** InterconnectLocationListWarning
    , InterconnectLocationListWarning
    , interconnectLocationListWarning
    , illwData
    , illwCode
    , illwMessage

    -- ** InstanceGroupsListInstancesWarning
    , InstanceGroupsListInstancesWarning
    , instanceGroupsListInstancesWarning
    , igliwData
    , igliwCode
    , igliwMessage

    -- ** RegionInstanceGroupManagerList
    , RegionInstanceGroupManagerList
    , regionInstanceGroupManagerList
    , rigmlNextPageToken
    , rigmlKind
    , rigmlItems
    , rigmlSelfLink
    , rigmlWarning
    , rigmlId

    -- ** ForwardingRuleIPProtocol
    , ForwardingRuleIPProtocol (..)

    -- ** DiskTypesScopedList
    , DiskTypesScopedList
    , diskTypesScopedList
    , dtslDiskTypes
    , dtslWarning

    -- ** ImageListWarningCode
    , ImageListWarningCode (..)

    -- ** AddressStatus
    , AddressStatus (..)

    -- ** ImageListWarningDataItem
    , ImageListWarningDataItem
    , imageListWarningDataItem
    , imaValue
    , imaKey

    -- ** AcceleratorTypeListWarningDataItem
    , AcceleratorTypeListWarningDataItem
    , acceleratorTypeListWarningDataItem
    , atlwdiValue
    , atlwdiKey

    -- ** InterconnectOutageNotification
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

    -- ** AcceleratorTypeListWarningCode
    , AcceleratorTypeListWarningCode (..)

    -- ** DiskTypeAggregatedList
    , DiskTypeAggregatedList
    , diskTypeAggregatedList
    , dtalNextPageToken
    , dtalKind
    , dtalItems
    , dtalSelfLink
    , dtalWarning
    , dtalId

    -- ** HTTPHealthCheck
    , HTTPHealthCheck
    , hTTPHealthCheck
    , httphcRequestPath
    , httphcHost
    , httphcProxyHeader
    , httphcPortName
    , httphcPort

    -- ** URLMapListWarningDataItem
    , URLMapListWarningDataItem
    , urlMapListWarningDataItem
    , umlwdiValue
    , umlwdiKey

    -- ** BackendServiceGroupHealth
    , BackendServiceGroupHealth
    , backendServiceGroupHealth
    , bsghKind
    , bsghHealthStatus

    -- ** URLMapListWarningCode
    , URLMapListWarningCode (..)

    -- ** InstanceGroupsListInstancesRequestInstanceState
    , InstanceGroupsListInstancesRequestInstanceState (..)

    -- ** AutoscalersScopedList
    , AutoscalersScopedList
    , autoscalersScopedList
    , aAutoscalers
    , aWarning

    -- ** AutoscalerAggregatedList
    , AutoscalerAggregatedList
    , autoscalerAggregatedList
    , aalNextPageToken
    , aalKind
    , aalItems
    , aalSelfLink
    , aalWarning
    , aalId

    -- ** RouterAggregatedListItems
    , RouterAggregatedListItems
    , routerAggregatedListItems
    , raliAddtional

    -- ** AcceleratorTypesScopedListWarning
    , AcceleratorTypesScopedListWarning
    , acceleratorTypesScopedListWarning
    , atslwData
    , atslwCode
    , atslwMessage

    -- ** TargetTCPProxy
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

    -- ** BackendBucketListWarning
    , BackendBucketListWarning
    , backendBucketListWarning
    , bblwData
    , bblwCode
    , bblwMessage

    -- ** ImageLabels
    , ImageLabels
    , imageLabels
    , iAddtional

    -- ** HTTPSHealthCheckProxyHeader
    , HTTPSHealthCheckProxyHeader (..)

    -- ** AutoscalingPolicy
    , AutoscalingPolicy
    , autoscalingPolicy
    , apCustomMetricUtilizations
    , apMaxNumReplicas
    , apCPUUtilization
    , apLoadBalancingUtilization
    , apMinNumReplicas
    , apCoolDownPeriodSec

    -- ** RegionList
    , RegionList
    , regionList
    , regNextPageToken
    , regKind
    , regItems
    , regSelfLink
    , regWarning
    , regId

    -- ** AttachedDiskInterface
    , AttachedDiskInterface (..)

    -- ** HealthCheckType
    , HealthCheckType (..)

    -- ** RegionInstanceGroupsListInstancesWarningCode
    , RegionInstanceGroupsListInstancesWarningCode (..)

    -- ** ZoneStatus
    , ZoneStatus (..)

    -- ** VPNTunnelList
    , VPNTunnelList
    , vpnTunnelList
    , vtlNextPageToken
    , vtlKind
    , vtlItems
    , vtlSelfLink
    , vtlWarning
    , vtlId

    -- ** AcceleratorTypeAggregatedListWarning
    , AcceleratorTypeAggregatedListWarning
    , acceleratorTypeAggregatedListWarning
    , atalwData
    , atalwCode
    , atalwMessage

    -- ** RegionInstanceGroupsListInstancesWarningDataItem
    , RegionInstanceGroupsListInstancesWarningDataItem
    , regionInstanceGroupsListInstancesWarningDataItem
    , rigliwdiValue
    , rigliwdiKey

    -- ** Interconnect
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

    -- ** MachineTypeScratchDisksItem
    , MachineTypeScratchDisksItem
    , machineTypeScratchDisksItem
    , mtsdiDiskGb

    -- ** SubnetworksScopedListWarningDataItem
    , SubnetworksScopedListWarningDataItem
    , subnetworksScopedListWarningDataItem
    , sslwdiValue
    , sslwdiKey

    -- ** MachineTypesScopedList
    , MachineTypesScopedList
    , machineTypesScopedList
    , mtslMachineTypes
    , mtslWarning

    -- ** SubnetworksScopedListWarningCode
    , SubnetworksScopedListWarningCode (..)

    -- ** Subnetwork
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

    -- ** HealthCheckListWarning
    , HealthCheckListWarning
    , healthCheckListWarning
    , hclwData
    , hclwCode
    , hclwMessage

    -- ** MachineTypeAggregatedList
    , MachineTypeAggregatedList
    , machineTypeAggregatedList
    , mtalNextPageToken
    , mtalKind
    , mtalItems
    , mtalSelfLink
    , mtalWarning
    , mtalId

    -- ** QuotaMetric
    , QuotaMetric (..)

    -- ** DiskType
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

    -- ** AutoscalerAggregatedListWarningDataItem
    , AutoscalerAggregatedListWarningDataItem
    , autoscalerAggregatedListWarningDataItem
    , aalwdiValue
    , aalwdiKey

    -- ** ZoneSetLabelsRequestLabels
    , ZoneSetLabelsRequestLabels
    , zoneSetLabelsRequestLabels
    , zslrlAddtional

    -- ** URLMapValidationResult
    , URLMapValidationResult
    , urlMapValidationResult
    , umvrLoadErrors
    , umvrLoadSucceeded
    , umvrTestPassed
    , umvrTestFailures

    -- ** Metadata
    , Metadata
    , metadata
    , mKind
    , mFingerprint
    , mItems

    -- ** RouteWarningsItem
    , RouteWarningsItem
    , routeWarningsItem
    , rwiData
    , rwiCode
    , rwiMessage

    -- ** InterconnectLocationList
    , InterconnectLocationList
    , interconnectLocationList
    , illNextPageToken
    , illKind
    , illItems
    , illSelfLink
    , illWarning
    , illId

    -- ** InstancePropertiesLabels
    , InstancePropertiesLabels
    , instancePropertiesLabels
    , iplAddtional

    -- ** AutoscalerAggregatedListWarningCode
    , AutoscalerAggregatedListWarningCode (..)

    -- ** AddressIPVersion
    , AddressIPVersion (..)

    -- ** MachineTypesScopedListWarningDataItem
    , MachineTypesScopedListWarningDataItem
    , machineTypesScopedListWarningDataItem
    , mtslwdiValue
    , mtslwdiKey

    -- ** InstanceStatus
    , InstanceStatus (..)

    -- ** DiskTypeListWarning
    , DiskTypeListWarning
    , diskTypeListWarning
    , dtlwData
    , dtlwCode
    , dtlwMessage

    -- ** RegionListWarningDataItem
    , RegionListWarningDataItem
    , regionListWarningDataItem
    , regValue
    , regKey

    -- ** MachineTypesScopedListWarningCode
    , MachineTypesScopedListWarningCode (..)

    -- ** InstancesSetMachineResourcesRequest
    , InstancesSetMachineResourcesRequest
    , instancesSetMachineResourcesRequest
    , ismrrGuestAccelerators

    -- ** InstancesSetServiceAccountRequest
    , InstancesSetServiceAccountRequest
    , instancesSetServiceAccountRequest
    , issarEmail
    , issarScopes

    -- ** DiskTypesScopedListWarningDataItem
    , DiskTypesScopedListWarningDataItem
    , diskTypesScopedListWarningDataItem
    , dtslwdiValue
    , dtslwdiKey

    -- ** VPNTunnelAggregatedListWarning
    , VPNTunnelAggregatedListWarning
    , vpnTunnelAggregatedListWarning
    , vtalwData
    , vtalwCode
    , vtalwMessage

    -- ** TargetHTTPProxy
    , TargetHTTPProxy
    , targetHTTPProxy
    , thttppURLMap
    , thttppKind
    , thttppSelfLink
    , thttppName
    , thttppCreationTimestamp
    , thttppId
    , thttppDescription

    -- ** MachineType
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

    -- ** AcceleratorConfig
    , AcceleratorConfig
    , acceleratorConfig
    , acAcceleratorCount
    , acAcceleratorType

    -- ** AutoscalerListWarning
    , AutoscalerListWarning
    , autoscalerListWarning
    , alwData
    , alwCode
    , alwMessage

    -- ** RegionListWarningCode
    , RegionListWarningCode (..)

    -- ** DiskTypesScopedListWarningCode
    , DiskTypesScopedListWarningCode (..)

    -- ** MachineTypeAggregatedListWarningDataItem
    , MachineTypeAggregatedListWarningDataItem
    , machineTypeAggregatedListWarningDataItem
    , mtalwdiValue
    , mtalwdiKey

    -- ** OperationError
    , OperationError
    , operationError
    , oeErrors

    -- ** TargetInstancesScopedListWarning
    , TargetInstancesScopedListWarning
    , targetInstancesScopedListWarning
    , tislwData
    , tislwCode
    , tislwMessage

    -- ** SubnetworkAggregatedList
    , SubnetworkAggregatedList
    , subnetworkAggregatedList
    , salNextPageToken
    , salKind
    , salItems
    , salSelfLink
    , salWarning
    , salId

    -- ** MachineTypeAggregatedListWarningCode
    , MachineTypeAggregatedListWarningCode (..)

    -- ** VPNTunnelListWarningDataItem
    , VPNTunnelListWarningDataItem
    , vpnTunnelListWarningDataItem
    , vtlwdiValue
    , vtlwdiKey

    -- ** DisksResizeRequest
    , DisksResizeRequest
    , disksResizeRequest
    , drrSizeGb

    -- ** AutoscalersScopedListWarningDataItem
    , AutoscalersScopedListWarningDataItem
    , autoscalersScopedListWarningDataItem
    , aValue
    , aKey

    -- ** InterconnectOutageNotificationState
    , InterconnectOutageNotificationState (..)

    -- ** VPNTunnelListWarningCode
    , VPNTunnelListWarningCode (..)

    -- ** AutoscalersScopedListWarningCode
    , AutoscalersScopedListWarningCode (..)

    -- ** DiskTypeAggregatedListWarningDataItem
    , DiskTypeAggregatedListWarningDataItem
    , diskTypeAggregatedListWarningDataItem
    , dtalwdiValue
    , dtalwdiKey

    -- ** ForwardingRuleLoadBalancingScheme
    , ForwardingRuleLoadBalancingScheme (..)

    -- ** RegionInstanceGroupManagerListWarning
    , RegionInstanceGroupManagerListWarning
    , regionInstanceGroupManagerListWarning
    , rigmlwData
    , rigmlwCode
    , rigmlwMessage

    -- ** RegionInstanceGroupManagersSetTemplateRequest
    , RegionInstanceGroupManagersSetTemplateRequest
    , regionInstanceGroupManagersSetTemplateRequest
    , rigmstrInstanceTemplate

    -- ** InstanceGroupsListInstances
    , InstanceGroupsListInstances
    , instanceGroupsListInstances
    , igliNextPageToken
    , igliKind
    , igliItems
    , igliSelfLink
    , igliWarning
    , igliId

    -- ** DiskTypeAggregatedListWarningCode
    , DiskTypeAggregatedListWarningCode (..)

    -- ** Autoscaler
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

    -- ** MachineTypeListWarning
    , MachineTypeListWarning
    , machineTypeListWarning
    , mtlwData
    , mtlwCode
    , mtlwMessage

    -- ** TargetHTTPProxyListWarning
    , TargetHTTPProxyListWarning
    , targetHTTPProxyListWarning
    , thttpplwData
    , thttpplwCode
    , thttpplwMessage

    -- ** DiskAggregatedListWarning
    , DiskAggregatedListWarning
    , diskAggregatedListWarning
    , dalwData
    , dalwCode
    , dalwMessage

    -- ** TargetPoolAggregatedListWarning
    , TargetPoolAggregatedListWarning
    , targetPoolAggregatedListWarning
    , tpalwData
    , tpalwCode
    , tpalwMessage

    -- ** DisksScopedListWarning
    , DisksScopedListWarning
    , disksScopedListWarning
    , dslwData
    , dslwCode
    , dslwMessage

    -- ** TargetVPNGatewayAggregatedListItems
    , TargetVPNGatewayAggregatedListItems
    , targetVPNGatewayAggregatedListItems
    , tvgaliAddtional

    -- ** DiskLabels
    , DiskLabels
    , diskLabels
    , dlAddtional

    -- ** InstanceGroupManagerListWarningDataItem
    , InstanceGroupManagerListWarningDataItem
    , instanceGroupManagerListWarningDataItem
    , igmlwdiValue
    , igmlwdiKey

    -- ** InterconnectInterconnectType
    , InterconnectInterconnectType (..)

    -- ** ForwardingRuleAggregatedListWarning
    , ForwardingRuleAggregatedListWarning
    , forwardingRuleAggregatedListWarning
    , fralwData
    , fralwCode
    , fralwMessage

    -- ** InstanceGroupManagerListWarningCode
    , InstanceGroupManagerListWarningCode (..)

    -- ** RouterStatusBGPPeerStatusStatus
    , RouterStatusBGPPeerStatusStatus (..)

    -- ** RegionAutoscalerListWarningDataItem
    , RegionAutoscalerListWarningDataItem
    , regionAutoscalerListWarningDataItem
    , ralwdiaValue
    , ralwdiaKey

    -- ** DiskListWarningCode
    , DiskListWarningCode (..)

    -- ** GlobalSetLabelsRequestLabels
    , GlobalSetLabelsRequestLabels
    , globalSetLabelsRequestLabels
    , gslrlAddtional

    -- ** TargetPoolsScopedListWarning
    , TargetPoolsScopedListWarning
    , targetPoolsScopedListWarning
    , tpslwData
    , tpslwCode
    , tpslwMessage

    -- ** HealthStatus
    , HealthStatus
    , healthStatus
    , hsIPAddress
    , hsHealthState
    , hsPort
    , hsInstance

    -- ** TargetTCPProxyList
    , TargetTCPProxyList
    , targetTCPProxyList
    , ttplNextPageToken
    , ttplKind
    , ttplItems
    , ttplSelfLink
    , ttplWarning
    , ttplId

    -- ** Region
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

    -- ** RegionAutoscalerListWarningCode
    , RegionAutoscalerListWarningCode (..)

    -- ** OperationListWarningDataItem
    , OperationListWarningDataItem
    , operationListWarningDataItem
    , olwdiValue
    , olwdiKey

    -- ** GuestOSFeature
    , GuestOSFeature
    , guestOSFeature
    , gofType

    -- ** VPNTunnel
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

    -- ** InstanceGroupManagerAggregatedListWarning
    , InstanceGroupManagerAggregatedListWarning
    , instanceGroupManagerAggregatedListWarning
    , igmalwData
    , igmalwCode
    , igmalwMessage

    -- ** DiskListWarningDataItem
    , DiskListWarningDataItem
    , diskListWarningDataItem
    , dlwdiValue
    , dlwdiKey

    -- ** CommitmentPlan
    , CommitmentPlan (..)

    -- ** OperationListWarningCode
    , OperationListWarningCode (..)

    -- ** AliasIPRange
    , AliasIPRange
    , aliasIPRange
    , airIPCIdRRange
    , airSubnetworkRangeName

    -- ** ForwardingRuleListWarningCode
    , ForwardingRuleListWarningCode (..)

    -- ** VPNTunnelsScopedListWarningCode
    , VPNTunnelsScopedListWarningCode (..)

    -- ** TargetInstanceListWarning
    , TargetInstanceListWarning
    , targetInstanceListWarning
    , tilwData
    , tilwCode
    , tilwMessage

    -- ** OperationAggregatedListWarning
    , OperationAggregatedListWarning
    , operationAggregatedListWarning
    , oalwData
    , oalwCode
    , oalwMessage

    -- ** TargetInstanceAggregatedListWarningDataItem
    , TargetInstanceAggregatedListWarningDataItem
    , targetInstanceAggregatedListWarningDataItem
    , tialwdiValue
    , tialwdiKey

    -- ** OperationsScopedListWarning
    , OperationsScopedListWarning
    , operationsScopedListWarning
    , oslwData
    , oslwCode
    , oslwMessage

    -- ** ForwardingRuleListWarningDataItem
    , ForwardingRuleListWarningDataItem
    , forwardingRuleListWarningDataItem
    , frlwdiValue
    , frlwdiKey

    -- ** Scheduling
    , Scheduling
    , scheduling
    , sAutomaticRestart
    , sOnHostMaintenance
    , sPreemptible

    -- ** TargetInstanceAggregatedListWarningCode
    , TargetInstanceAggregatedListWarningCode (..)

    -- ** VPNTunnelsScopedListWarningDataItem
    , VPNTunnelsScopedListWarningDataItem
    , vpnTunnelsScopedListWarningDataItem
    , vtslwdiValue
    , vtslwdiKey

    -- ** InterconnectList
    , InterconnectList
    , interconnectList
    , intnNextPageToken
    , intnKind
    , intnItems
    , intnSelfLink
    , intnWarning
    , intnId

    -- ** TargetPoolListWarningDataItem
    , TargetPoolListWarningDataItem
    , targetPoolListWarningDataItem
    , tplwdiValue
    , tplwdiKey

    -- ** SubnetworkList
    , SubnetworkList
    , subnetworkList
    , slNextPageToken
    , slKind
    , slItems
    , slSelfLink
    , slWarning
    , slId

    -- ** AddressAddressType
    , AddressAddressType (..)

    -- ** TargetPoolListWarningCode
    , TargetPoolListWarningCode (..)

    -- ** ForwardingRulesScopedListWarning
    , ForwardingRulesScopedListWarning
    , forwardingRulesScopedListWarning
    , frslwData
    , frslwCode
    , frslwMessage

    -- ** HTTPHealthCheckList
    , HTTPHealthCheckList
    , hTTPHealthCheckList
    , httphclNextPageToken
    , httphclKind
    , httphclItems
    , httphclSelfLink
    , httphclWarning
    , httphclId

    -- ** InstanceGroupManagersScopedListWarning
    , InstanceGroupManagersScopedListWarning
    , instanceGroupManagersScopedListWarning
    , igmslwData
    , igmslwCode
    , igmslwMessage

    -- ** URLMapsValidateRequest
    , URLMapsValidateRequest
    , urlMapsValidateRequest
    , umvrResource

    -- ** InstanceGroupManagersSetTargetPoolsRequest
    , InstanceGroupManagersSetTargetPoolsRequest
    , instanceGroupManagersSetTargetPoolsRequest
    , igmstprFingerprint
    , igmstprTargetPools

    -- ** NetworkListWarningCode
    , NetworkListWarningCode (..)

    -- ** Commitment
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

    -- ** HTTPSHealthCheckList
    , HTTPSHealthCheckList
    , httpsHealthCheckList
    , hhclNextPageToken
    , hhclKind
    , hhclItems
    , hhclSelfLink
    , hhclWarning
    , hhclId

    -- ** AddressListWarningDataItem
    , AddressListWarningDataItem
    , addressListWarningDataItem
    , addValue
    , addKey

    -- ** OperationErrorErrorsItem
    , OperationErrorErrorsItem
    , operationErrorErrorsItem
    , oeeiLocation
    , oeeiCode
    , oeeiMessage

    -- ** CommitmentListWarning
    , CommitmentListWarning
    , commitmentListWarning
    , clwData
    , clwCode
    , clwMessage

    -- ** License
    , License
    , license
    , lChargesUseFee
    , lKind
    , lSelfLink
    , lName

    -- ** PathRule
    , PathRule
    , pathRule
    , prService
    , prPaths

    -- ** XpnHostListWarningCode
    , XpnHostListWarningCode (..)

    -- ** InterconnectAttachmentsScopedListWarningDataItem
    , InterconnectAttachmentsScopedListWarningDataItem
    , interconnectAttachmentsScopedListWarningDataItem
    , iaslwdiValue
    , iaslwdiKey

    -- ** InterconnectAttachmentAggregatedListWarningCode
    , InterconnectAttachmentAggregatedListWarningCode (..)

    -- ** CommitmentsScopedListWarningDataItem
    , CommitmentsScopedListWarningDataItem
    , commitmentsScopedListWarningDataItem
    , cslwdiValue
    , cslwdiKey

    -- ** InterconnectAttachment
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

    -- ** InstanceList
    , InstanceList
    , instanceList
    , insNextPageToken
    , insKind
    , insItems
    , insSelfLink
    , insWarning
    , insId

    -- ** NetworkListWarningDataItem
    , NetworkListWarningDataItem
    , networkListWarningDataItem
    , nlwdiValue
    , nlwdiKey

    -- ** AddressListWarningCode
    , AddressListWarningCode (..)

    -- ** NetworkPeeringState
    , NetworkPeeringState (..)

    -- ** CacheKeyPolicy
    , CacheKeyPolicy
    , cacheKeyPolicy
    , ckpQueryStringWhiteList
    , ckpIncludeHost
    , ckpIncludeProtocol
    , ckpQueryStringBlackList
    , ckpIncludeQueryString

    -- ** ZoneListWarningCode
    , ZoneListWarningCode (..)

    -- ** CommitmentAggregatedListWarningDataItem
    , CommitmentAggregatedListWarningDataItem
    , commitmentAggregatedListWarningDataItem
    , calwdiValue
    , calwdiKey

    -- ** RegionInstanceGroupListWarningDataItem
    , RegionInstanceGroupListWarningDataItem
    , regionInstanceGroupListWarningDataItem
    , riglwdiValue
    , riglwdiKey

    -- ** SubnetworkAggregatedListItems
    , SubnetworkAggregatedListItems
    , subnetworkAggregatedListItems
    , saliAddtional

    -- ** InterconnectAttachmentAggregatedListWarningDataItem
    , InterconnectAttachmentAggregatedListWarningDataItem
    , interconnectAttachmentAggregatedListWarningDataItem
    , iaalwdiValue
    , iaalwdiKey

    -- ** InterconnectAttachmentListWarning
    , InterconnectAttachmentListWarning
    , interconnectAttachmentListWarning
    , intData
    , intCode
    , intMessage

    -- ** InterconnectOutageNotificationIssueType
    , InterconnectOutageNotificationIssueType (..)

    -- ** CommitmentsScopedListWarningCode
    , CommitmentsScopedListWarningCode (..)

    -- ** AddressAggregatedListWarning
    , AddressAggregatedListWarning
    , addressAggregatedListWarning
    , addData
    , addCode
    , addMessage

    -- ** ManagedInstanceLastAttempt
    , ManagedInstanceLastAttempt
    , managedInstanceLastAttempt
    , milaErrors

    -- ** AutoscalerStatusDetails
    , AutoscalerStatusDetails
    , autoscalerStatusDetails
    , asdType
    , asdMessage

    -- ** BackendServiceList
    , BackendServiceList
    , backendServiceList
    , bslNextPageToken
    , bslKind
    , bslItems
    , bslSelfLink
    , bslWarning
    , bslId

    -- ** XpnHostListWarningDataItem
    , XpnHostListWarningDataItem
    , xpnHostListWarningDataItem
    , xhlwdiValue
    , xhlwdiKey

    -- ** InterconnectAttachmentsScopedListWarningCode
    , InterconnectAttachmentsScopedListWarningCode (..)

    -- ** ZoneListWarningDataItem
    , ZoneListWarningDataItem
    , zoneListWarningDataItem
    , zlwdiValue
    , zlwdiKey

    -- ** CommitmentAggregatedListWarningCode
    , CommitmentAggregatedListWarningCode (..)

    -- ** RegionInstanceGroupListWarningCode
    , RegionInstanceGroupListWarningCode (..)

    -- ** InstanceGroupsScopedListWarning
    , InstanceGroupsScopedListWarning
    , instanceGroupsScopedListWarning
    , igslwData
    , igslwCode
    , igslwMessage

    -- ** AutoscalingPolicyCPUUtilization
    , AutoscalingPolicyCPUUtilization
    , autoscalingPolicyCPUUtilization
    , apcuUtilizationTarget

    -- ** InstanceGroupsScopedListWarningCode
    , InstanceGroupsScopedListWarningCode (..)

    -- ** InterconnectOperationalStatus
    , InterconnectOperationalStatus (..)

    -- ** InstanceGroupsScopedListWarningDataItem
    , InstanceGroupsScopedListWarningDataItem
    , instanceGroupsScopedListWarningDataItem
    , igslwdiValue
    , igslwdiKey

    -- ** XpnHostListWarning
    , XpnHostListWarning
    , xpnHostListWarning
    , xhlwData
    , xhlwCode
    , xhlwMessage

    -- ** ResourceGroupReference
    , ResourceGroupReference
    , resourceGroupReference
    , rgrGroup

    -- ** Firewall
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

    -- ** InterconnectAttachmentsScopedListWarning
    , InterconnectAttachmentsScopedListWarning
    , interconnectAttachmentsScopedListWarning
    , iaslwData
    , iaslwCode
    , iaslwMessage

    -- ** HostRule
    , HostRule
    , hostRule
    , hrHosts
    , hrDescription
    , hrPathMatcher

    -- ** Quota
    , Quota
    , quota
    , qMetric
    , qLimit
    , qUsage

    -- ** InstanceGroup
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

    -- ** AddressListWarning
    , AddressListWarning
    , addressListWarning
    , alwlData
    , alwlCode
    , alwlMessage

    -- ** RouterInterface
    , RouterInterface
    , routerInterface
    , riLinkedInterconnectAttachment
    , riName
    , riIPRange
    , riLinkedVPNTunnel

    -- ** InstanceWithNamedPortsStatus
    , InstanceWithNamedPortsStatus (..)

    -- ** NetworkListWarning
    , NetworkListWarning
    , networkListWarning
    , nlwData
    , nlwCode
    , nlwMessage

    -- ** TCPHealthCheckProxyHeader
    , TCPHealthCheckProxyHeader (..)

    -- ** SnapshotList
    , SnapshotList
    , snapshotList
    , snaNextPageToken
    , snaKind
    , snaItems
    , snaSelfLink
    , snaWarning
    , snaId

    -- ** CommitmentListWarningCode
    , CommitmentListWarningCode (..)

    -- ** TestFailure
    , TestFailure
    , testFailure
    , tfPath
    , tfExpectedService
    , tfHost
    , tfActualService

    -- ** CommitmentListWarningDataItem
    , CommitmentListWarningDataItem
    , commitmentListWarningDataItem
    , clwdiValue
    , clwdiKey

    -- ** CommitmentsScopedListWarning
    , CommitmentsScopedListWarning
    , commitmentsScopedListWarning
    , cslwData
    , cslwCode
    , cslwMessage

    -- ** SerialPortOutput
    , SerialPortOutput
    , serialPortOutput
    , spoNext
    , spoContents
    , spoKind
    , spoStart
    , spoSelfLink

    -- ** TargetVPNGatewayAggregatedList
    , TargetVPNGatewayAggregatedList
    , targetVPNGatewayAggregatedList
    , tvgalNextPageToken
    , tvgalKind
    , tvgalItems
    , tvgalSelfLink
    , tvgalWarning
    , tvgalId

    -- ** InterconnectAttachmentAggregatedListWarning
    , InterconnectAttachmentAggregatedListWarning
    , interconnectAttachmentAggregatedListWarning
    , iaalwData
    , iaalwCode
    , iaalwMessage

    -- ** MetadataItemsItem
    , MetadataItemsItem
    , metadataItemsItem
    , miiValue
    , miiKey

    -- ** TargetHTTPSProxy
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

    -- ** ConnectionDraining
    , ConnectionDraining
    , connectionDraining
    , cdDrainingTimeoutSec

    -- ** InterconnectAttachmentListWarningDataItem
    , InterconnectAttachmentListWarningDataItem
    , interconnectAttachmentListWarningDataItem
    , ialwdiValue
    , ialwdiKey

    -- ** AddressAggregatedListWarningCode
    , AddressAggregatedListWarningCode (..)

    -- ** CacheInvalidationRule
    , CacheInvalidationRule
    , cacheInvalidationRule
    , cirPath
    , cirHost

    -- ** AddressAggregatedListWarningDataItem
    , AddressAggregatedListWarningDataItem
    , addressAggregatedListWarningDataItem
    , aalwdiaValue
    , aalwdiaKey

    -- ** InterconnectAttachmentListWarningCode
    , InterconnectAttachmentListWarningCode (..)

    -- ** TargetVPNGatewaysScopedList
    , TargetVPNGatewaysScopedList
    , targetVPNGatewaysScopedList
    , tvgslTargetVPNGateways
    , tvgslWarning

    -- ** CommitmentAggregatedListWarning
    , CommitmentAggregatedListWarning
    , commitmentAggregatedListWarning
    , calwData
    , calwCode
    , calwMessage

    -- ** RegionInstanceGroupListWarning
    , RegionInstanceGroupListWarning
    , regionInstanceGroupListWarning
    , riglwData
    , riglwCode
    , riglwMessage

    -- ** AccessConfig
    , AccessConfig
    , accessConfig
    , acKind
    , acName
    , acNATIP
    , acType

    -- ** ZoneListWarning
    , ZoneListWarning
    , zoneListWarning
    , zlwData
    , zlwCode
    , zlwMessage

    -- ** ManagedInstanceLastAttemptErrorsErrorsItem
    , ManagedInstanceLastAttemptErrorsErrorsItem
    , managedInstanceLastAttemptErrorsErrorsItem
    , milaeeiLocation
    , milaeeiCode
    , milaeeiMessage

    -- ** InstancesScopedListWarningCode
    , InstancesScopedListWarningCode (..)

    -- ** InstancesScopedListWarningDataItem
    , InstancesScopedListWarningDataItem
    , instancesScopedListWarningDataItem
    , islwdiValue
    , islwdiKey

    -- ** SnapshotLabels
    , SnapshotLabels
    , snapshotLabels
    , slAddtional

    -- ** BackendServicesScopedListWarningDataItem
    , BackendServicesScopedListWarningDataItem
    , backendServicesScopedListWarningDataItem
    , bsslwdiValue
    , bsslwdiKey

    -- ** BackendService
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

    -- ** InstanceListWarning
    , InstanceListWarning
    , instanceListWarning
    , insData
    , insCode
    , insMessage

    -- ** InstanceMoveRequest
    , InstanceMoveRequest
    , instanceMoveRequest
    , imrTargetInstance
    , imrDestinationZone

    -- ** XpnResourceIdType
    , XpnResourceIdType (..)

    -- ** BackendServicesScopedListWarningCode
    , BackendServicesScopedListWarningCode (..)

    -- ** CommitmentList
    , CommitmentList
    , commitmentList
    , clNextPageToken
    , clKind
    , clItems
    , clSelfLink
    , clWarning
    , clId

    -- ** TargetPoolSessionAffinity
    , TargetPoolSessionAffinity (..)

    -- ** InstancesSetLabelsRequestLabels
    , InstancesSetLabelsRequestLabels
    , instancesSetLabelsRequestLabels
    , islrlAddtional

    -- ** InstanceGroupsScopedList
    , InstanceGroupsScopedList
    , instanceGroupsScopedList
    , igslWarning
    , igslInstanceGroups

    -- ** InstancesStartWithEncryptionKeyRequest
    , InstancesStartWithEncryptionKeyRequest
    , instancesStartWithEncryptionKeyRequest
    , iswekrDisks

    -- ** HTTPSHealthCheck
    , HTTPSHealthCheck
    , httpsHealthCheck
    , hhcRequestPath
    , hhcHost
    , hhcProxyHeader
    , hhcPortName
    , hhcPort

    -- ** AutoscalerStatus
    , AutoscalerStatus (..)

    -- ** ImageRawDiskContainerType
    , ImageRawDiskContainerType (..)

    -- ** InstanceAggregatedListWarningCode
    , InstanceAggregatedListWarningCode (..)

    -- ** VPNTunnelAggregatedListItems
    , VPNTunnelAggregatedListItems
    , vpnTunnelAggregatedListItems
    , vtaliAddtional

    -- ** ResourceCommitmentType
    , ResourceCommitmentType (..)

    -- ** InstanceAggregatedListWarningDataItem
    , InstanceAggregatedListWarningDataItem
    , instanceAggregatedListWarningDataItem
    , insValue
    , insKey

    -- ** Tags
    , Tags
    , tags
    , tFingerprint
    , tItems

    -- ** AddressAggregatedList
    , AddressAggregatedList
    , addressAggregatedList
    , addNextPageToken
    , addKind
    , addItems
    , addSelfLink
    , addWarning
    , addId

    -- ** InterconnectAttachmentList
    , InterconnectAttachmentList
    , interconnectAttachmentList
    , ialaNextPageToken
    , ialaKind
    , ialaItems
    , ialaSelfLink
    , ialaWarning
    , ialaId

    -- ** OperationWarningsItem
    , OperationWarningsItem
    , operationWarningsItem
    , owiData
    , owiCode
    , owiMessage

    -- ** URLMapTest
    , URLMapTest
    , urlMapTest
    , umtPath
    , umtService
    , umtHost
    , umtDescription

    -- ** HTTPSHealthCheckListWarning
    , HTTPSHealthCheckListWarning
    , httpsHealthCheckListWarning
    , hhclwData
    , hhclwCode
    , hhclwMessage

    -- ** RoutersPreviewResponse
    , RoutersPreviewResponse
    , routersPreviewResponse
    , rprResource

    -- ** BackendServiceAggregatedListWarningDataItem
    , BackendServiceAggregatedListWarningDataItem
    , backendServiceAggregatedListWarningDataItem
    , bsalwdiValue
    , bsalwdiKey

    -- ** Instance
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

    -- ** PathMatcher
    , PathMatcher
    , pathMatcher
    , pmDefaultService
    , pmName
    , pmPathRules
    , pmDescription

    -- ** BackendServiceListWarning
    , BackendServiceListWarning
    , backendServiceListWarning
    , bslwData
    , bslwCode
    , bslwMessage

    -- ** BackendServiceAggregatedListWarningCode
    , BackendServiceAggregatedListWarningCode (..)
    ) where

import Network.Google.Prelude
import Network.Google.Compute.Types
import Network.Google.Resource.Compute.AcceleratorTypes.AggregatedList
import Network.Google.Resource.Compute.AcceleratorTypes.Get
import Network.Google.Resource.Compute.AcceleratorTypes.List
import Network.Google.Resource.Compute.Addresses.AggregatedList
import Network.Google.Resource.Compute.Addresses.Delete
import Network.Google.Resource.Compute.Addresses.Get
import Network.Google.Resource.Compute.Addresses.Insert
import Network.Google.Resource.Compute.Addresses.List
import Network.Google.Resource.Compute.Autoscalers.AggregatedList
import Network.Google.Resource.Compute.Autoscalers.Delete
import Network.Google.Resource.Compute.Autoscalers.Get
import Network.Google.Resource.Compute.Autoscalers.Insert
import Network.Google.Resource.Compute.Autoscalers.List
import Network.Google.Resource.Compute.Autoscalers.Patch
import Network.Google.Resource.Compute.Autoscalers.Update
import Network.Google.Resource.Compute.BackendBuckets.Delete
import Network.Google.Resource.Compute.BackendBuckets.Get
import Network.Google.Resource.Compute.BackendBuckets.Insert
import Network.Google.Resource.Compute.BackendBuckets.List
import Network.Google.Resource.Compute.BackendBuckets.Patch
import Network.Google.Resource.Compute.BackendBuckets.Update
import Network.Google.Resource.Compute.BackendServices.AggregatedList
import Network.Google.Resource.Compute.BackendServices.Delete
import Network.Google.Resource.Compute.BackendServices.Get
import Network.Google.Resource.Compute.BackendServices.GetHealth
import Network.Google.Resource.Compute.BackendServices.Insert
import Network.Google.Resource.Compute.BackendServices.List
import Network.Google.Resource.Compute.BackendServices.Patch
import Network.Google.Resource.Compute.BackendServices.Update
import Network.Google.Resource.Compute.DiskTypes.AggregatedList
import Network.Google.Resource.Compute.DiskTypes.Get
import Network.Google.Resource.Compute.DiskTypes.List
import Network.Google.Resource.Compute.Disks.AggregatedList
import Network.Google.Resource.Compute.Disks.CreateSnapshot
import Network.Google.Resource.Compute.Disks.Delete
import Network.Google.Resource.Compute.Disks.Get
import Network.Google.Resource.Compute.Disks.Insert
import Network.Google.Resource.Compute.Disks.List
import Network.Google.Resource.Compute.Disks.Resize
import Network.Google.Resource.Compute.Disks.SetLabels
import Network.Google.Resource.Compute.Firewalls.Delete
import Network.Google.Resource.Compute.Firewalls.Get
import Network.Google.Resource.Compute.Firewalls.Insert
import Network.Google.Resource.Compute.Firewalls.List
import Network.Google.Resource.Compute.Firewalls.Patch
import Network.Google.Resource.Compute.Firewalls.Update
import Network.Google.Resource.Compute.ForwardingRules.AggregatedList
import Network.Google.Resource.Compute.ForwardingRules.Delete
import Network.Google.Resource.Compute.ForwardingRules.Get
import Network.Google.Resource.Compute.ForwardingRules.Insert
import Network.Google.Resource.Compute.ForwardingRules.List
import Network.Google.Resource.Compute.ForwardingRules.SetTarget
import Network.Google.Resource.Compute.GlobalAddresses.Delete
import Network.Google.Resource.Compute.GlobalAddresses.Get
import Network.Google.Resource.Compute.GlobalAddresses.Insert
import Network.Google.Resource.Compute.GlobalAddresses.List
import Network.Google.Resource.Compute.GlobalForwardingRules.Delete
import Network.Google.Resource.Compute.GlobalForwardingRules.Get
import Network.Google.Resource.Compute.GlobalForwardingRules.Insert
import Network.Google.Resource.Compute.GlobalForwardingRules.List
import Network.Google.Resource.Compute.GlobalForwardingRules.SetTarget
import Network.Google.Resource.Compute.GlobalOperations.AggregatedList
import Network.Google.Resource.Compute.GlobalOperations.Delete
import Network.Google.Resource.Compute.GlobalOperations.Get
import Network.Google.Resource.Compute.GlobalOperations.List
import Network.Google.Resource.Compute.HTTPHealthChecks.Delete
import Network.Google.Resource.Compute.HTTPHealthChecks.Get
import Network.Google.Resource.Compute.HTTPHealthChecks.Insert
import Network.Google.Resource.Compute.HTTPHealthChecks.List
import Network.Google.Resource.Compute.HTTPHealthChecks.Patch
import Network.Google.Resource.Compute.HTTPHealthChecks.Update
import Network.Google.Resource.Compute.HTTPSHealthChecks.Delete
import Network.Google.Resource.Compute.HTTPSHealthChecks.Get
import Network.Google.Resource.Compute.HTTPSHealthChecks.Insert
import Network.Google.Resource.Compute.HTTPSHealthChecks.List
import Network.Google.Resource.Compute.HTTPSHealthChecks.Patch
import Network.Google.Resource.Compute.HTTPSHealthChecks.Update
import Network.Google.Resource.Compute.HealthChecks.Delete
import Network.Google.Resource.Compute.HealthChecks.Get
import Network.Google.Resource.Compute.HealthChecks.Insert
import Network.Google.Resource.Compute.HealthChecks.List
import Network.Google.Resource.Compute.HealthChecks.Patch
import Network.Google.Resource.Compute.HealthChecks.Update
import Network.Google.Resource.Compute.Images.Delete
import Network.Google.Resource.Compute.Images.Deprecate
import Network.Google.Resource.Compute.Images.Get
import Network.Google.Resource.Compute.Images.GetFromFamily
import Network.Google.Resource.Compute.Images.Insert
import Network.Google.Resource.Compute.Images.List
import Network.Google.Resource.Compute.Images.SetLabels
import Network.Google.Resource.Compute.InstanceGroupManagers.AbandonInstances
import Network.Google.Resource.Compute.InstanceGroupManagers.AggregatedList
import Network.Google.Resource.Compute.InstanceGroupManagers.Delete
import Network.Google.Resource.Compute.InstanceGroupManagers.DeleteInstances
import Network.Google.Resource.Compute.InstanceGroupManagers.Get
import Network.Google.Resource.Compute.InstanceGroupManagers.Insert
import Network.Google.Resource.Compute.InstanceGroupManagers.List
import Network.Google.Resource.Compute.InstanceGroupManagers.ListManagedInstances
import Network.Google.Resource.Compute.InstanceGroupManagers.RecreateInstances
import Network.Google.Resource.Compute.InstanceGroupManagers.Resize
import Network.Google.Resource.Compute.InstanceGroupManagers.SetInstanceTemplate
import Network.Google.Resource.Compute.InstanceGroupManagers.SetTargetPools
import Network.Google.Resource.Compute.InstanceGroups.AddInstances
import Network.Google.Resource.Compute.InstanceGroups.AggregatedList
import Network.Google.Resource.Compute.InstanceGroups.Delete
import Network.Google.Resource.Compute.InstanceGroups.Get
import Network.Google.Resource.Compute.InstanceGroups.Insert
import Network.Google.Resource.Compute.InstanceGroups.List
import Network.Google.Resource.Compute.InstanceGroups.ListInstances
import Network.Google.Resource.Compute.InstanceGroups.RemoveInstances
import Network.Google.Resource.Compute.InstanceGroups.SetNamedPorts
import Network.Google.Resource.Compute.InstanceTemplates.Delete
import Network.Google.Resource.Compute.InstanceTemplates.Get
import Network.Google.Resource.Compute.InstanceTemplates.Insert
import Network.Google.Resource.Compute.InstanceTemplates.List
import Network.Google.Resource.Compute.Instances.AddAccessConfig
import Network.Google.Resource.Compute.Instances.AggregatedList
import Network.Google.Resource.Compute.Instances.AttachDisk
import Network.Google.Resource.Compute.Instances.Delete
import Network.Google.Resource.Compute.Instances.DeleteAccessConfig
import Network.Google.Resource.Compute.Instances.DetachDisk
import Network.Google.Resource.Compute.Instances.Get
import Network.Google.Resource.Compute.Instances.GetSerialPortOutput
import Network.Google.Resource.Compute.Instances.Insert
import Network.Google.Resource.Compute.Instances.List
import Network.Google.Resource.Compute.Instances.Reset
import Network.Google.Resource.Compute.Instances.SetDeletionProtection
import Network.Google.Resource.Compute.Instances.SetDiskAutoDelete
import Network.Google.Resource.Compute.Instances.SetLabels
import Network.Google.Resource.Compute.Instances.SetMachineResources
import Network.Google.Resource.Compute.Instances.SetMachineType
import Network.Google.Resource.Compute.Instances.SetMetadata
import Network.Google.Resource.Compute.Instances.SetMinCPUPlatform
import Network.Google.Resource.Compute.Instances.SetScheduling
import Network.Google.Resource.Compute.Instances.SetServiceAccount
import Network.Google.Resource.Compute.Instances.SetTags
import Network.Google.Resource.Compute.Instances.Start
import Network.Google.Resource.Compute.Instances.StartWithEncryptionKey
import Network.Google.Resource.Compute.Instances.Stop
import Network.Google.Resource.Compute.InterconnectAttachments.AggregatedList
import Network.Google.Resource.Compute.InterconnectAttachments.Delete
import Network.Google.Resource.Compute.InterconnectAttachments.Get
import Network.Google.Resource.Compute.InterconnectAttachments.Insert
import Network.Google.Resource.Compute.InterconnectAttachments.List
import Network.Google.Resource.Compute.InterconnectLocations.Get
import Network.Google.Resource.Compute.InterconnectLocations.List
import Network.Google.Resource.Compute.Interconnects.Delete
import Network.Google.Resource.Compute.Interconnects.Get
import Network.Google.Resource.Compute.Interconnects.Insert
import Network.Google.Resource.Compute.Interconnects.List
import Network.Google.Resource.Compute.Interconnects.Patch
import Network.Google.Resource.Compute.Licenses.Get
import Network.Google.Resource.Compute.MachineTypes.AggregatedList
import Network.Google.Resource.Compute.MachineTypes.Get
import Network.Google.Resource.Compute.MachineTypes.List
import Network.Google.Resource.Compute.Networks.AddPeering
import Network.Google.Resource.Compute.Networks.Delete
import Network.Google.Resource.Compute.Networks.Get
import Network.Google.Resource.Compute.Networks.Insert
import Network.Google.Resource.Compute.Networks.List
import Network.Google.Resource.Compute.Networks.Patch
import Network.Google.Resource.Compute.Networks.RemovePeering
import Network.Google.Resource.Compute.Networks.SwitchToCustomMode
import Network.Google.Resource.Compute.Projects.DisableXpnHost
import Network.Google.Resource.Compute.Projects.DisableXpnResource
import Network.Google.Resource.Compute.Projects.EnableXpnHost
import Network.Google.Resource.Compute.Projects.EnableXpnResource
import Network.Google.Resource.Compute.Projects.Get
import Network.Google.Resource.Compute.Projects.GetXpnHost
import Network.Google.Resource.Compute.Projects.GetXpnResources
import Network.Google.Resource.Compute.Projects.ListXpnHosts
import Network.Google.Resource.Compute.Projects.MoveDisk
import Network.Google.Resource.Compute.Projects.MoveInstance
import Network.Google.Resource.Compute.Projects.SetCommonInstanceMetadata
import Network.Google.Resource.Compute.Projects.SetUsageExportBucket
import Network.Google.Resource.Compute.RegionAutoscalers.Delete
import Network.Google.Resource.Compute.RegionAutoscalers.Get
import Network.Google.Resource.Compute.RegionAutoscalers.Insert
import Network.Google.Resource.Compute.RegionAutoscalers.List
import Network.Google.Resource.Compute.RegionAutoscalers.Patch
import Network.Google.Resource.Compute.RegionAutoscalers.Update
import Network.Google.Resource.Compute.RegionBackendServices.Delete
import Network.Google.Resource.Compute.RegionBackendServices.Get
import Network.Google.Resource.Compute.RegionBackendServices.GetHealth
import Network.Google.Resource.Compute.RegionBackendServices.Insert
import Network.Google.Resource.Compute.RegionBackendServices.List
import Network.Google.Resource.Compute.RegionBackendServices.Patch
import Network.Google.Resource.Compute.RegionBackendServices.Update
import Network.Google.Resource.Compute.RegionCommitments.AggregatedList
import Network.Google.Resource.Compute.RegionCommitments.Get
import Network.Google.Resource.Compute.RegionCommitments.Insert
import Network.Google.Resource.Compute.RegionCommitments.List
import Network.Google.Resource.Compute.RegionInstanceGroupManagers.AbandonInstances
import Network.Google.Resource.Compute.RegionInstanceGroupManagers.Delete
import Network.Google.Resource.Compute.RegionInstanceGroupManagers.DeleteInstances
import Network.Google.Resource.Compute.RegionInstanceGroupManagers.Get
import Network.Google.Resource.Compute.RegionInstanceGroupManagers.Insert
import Network.Google.Resource.Compute.RegionInstanceGroupManagers.List
import Network.Google.Resource.Compute.RegionInstanceGroupManagers.ListManagedInstances
import Network.Google.Resource.Compute.RegionInstanceGroupManagers.RecreateInstances
import Network.Google.Resource.Compute.RegionInstanceGroupManagers.Resize
import Network.Google.Resource.Compute.RegionInstanceGroupManagers.SetInstanceTemplate
import Network.Google.Resource.Compute.RegionInstanceGroupManagers.SetTargetPools
import Network.Google.Resource.Compute.RegionInstanceGroups.Get
import Network.Google.Resource.Compute.RegionInstanceGroups.List
import Network.Google.Resource.Compute.RegionInstanceGroups.ListInstances
import Network.Google.Resource.Compute.RegionInstanceGroups.SetNamedPorts
import Network.Google.Resource.Compute.RegionOperations.Delete
import Network.Google.Resource.Compute.RegionOperations.Get
import Network.Google.Resource.Compute.RegionOperations.List
import Network.Google.Resource.Compute.Regions.Get
import Network.Google.Resource.Compute.Regions.List
import Network.Google.Resource.Compute.Routers.AggregatedList
import Network.Google.Resource.Compute.Routers.Delete
import Network.Google.Resource.Compute.Routers.Get
import Network.Google.Resource.Compute.Routers.GetRouterStatus
import Network.Google.Resource.Compute.Routers.Insert
import Network.Google.Resource.Compute.Routers.List
import Network.Google.Resource.Compute.Routers.Patch
import Network.Google.Resource.Compute.Routers.Preview
import Network.Google.Resource.Compute.Routers.Update
import Network.Google.Resource.Compute.Routes.Delete
import Network.Google.Resource.Compute.Routes.Get
import Network.Google.Resource.Compute.Routes.Insert
import Network.Google.Resource.Compute.Routes.List
import Network.Google.Resource.Compute.SSLCertificates.Delete
import Network.Google.Resource.Compute.SSLCertificates.Get
import Network.Google.Resource.Compute.SSLCertificates.Insert
import Network.Google.Resource.Compute.SSLCertificates.List
import Network.Google.Resource.Compute.Snapshots.Delete
import Network.Google.Resource.Compute.Snapshots.Get
import Network.Google.Resource.Compute.Snapshots.List
import Network.Google.Resource.Compute.Snapshots.SetLabels
import Network.Google.Resource.Compute.Subnetworks.AggregatedList
import Network.Google.Resource.Compute.Subnetworks.Delete
import Network.Google.Resource.Compute.Subnetworks.ExpandIPCIdRRange
import Network.Google.Resource.Compute.Subnetworks.Get
import Network.Google.Resource.Compute.Subnetworks.Insert
import Network.Google.Resource.Compute.Subnetworks.List
import Network.Google.Resource.Compute.Subnetworks.SetPrivateIPGoogleAccess
import Network.Google.Resource.Compute.TargetHTTPProxies.Delete
import Network.Google.Resource.Compute.TargetHTTPProxies.Get
import Network.Google.Resource.Compute.TargetHTTPProxies.Insert
import Network.Google.Resource.Compute.TargetHTTPProxies.List
import Network.Google.Resource.Compute.TargetHTTPProxies.SetURLMap
import Network.Google.Resource.Compute.TargetHTTPSProxies.Delete
import Network.Google.Resource.Compute.TargetHTTPSProxies.Get
import Network.Google.Resource.Compute.TargetHTTPSProxies.Insert
import Network.Google.Resource.Compute.TargetHTTPSProxies.List
import Network.Google.Resource.Compute.TargetHTTPSProxies.SetSSLCertificates
import Network.Google.Resource.Compute.TargetHTTPSProxies.SetURLMap
import Network.Google.Resource.Compute.TargetInstances.AggregatedList
import Network.Google.Resource.Compute.TargetInstances.Delete
import Network.Google.Resource.Compute.TargetInstances.Get
import Network.Google.Resource.Compute.TargetInstances.Insert
import Network.Google.Resource.Compute.TargetInstances.List
import Network.Google.Resource.Compute.TargetPools.AddHealthCheck
import Network.Google.Resource.Compute.TargetPools.AddInstance
import Network.Google.Resource.Compute.TargetPools.AggregatedList
import Network.Google.Resource.Compute.TargetPools.Delete
import Network.Google.Resource.Compute.TargetPools.Get
import Network.Google.Resource.Compute.TargetPools.GetHealth
import Network.Google.Resource.Compute.TargetPools.Insert
import Network.Google.Resource.Compute.TargetPools.List
import Network.Google.Resource.Compute.TargetPools.RemoveHealthCheck
import Network.Google.Resource.Compute.TargetPools.RemoveInstance
import Network.Google.Resource.Compute.TargetPools.SetBackup
import Network.Google.Resource.Compute.TargetSSLProxies.Delete
import Network.Google.Resource.Compute.TargetSSLProxies.Get
import Network.Google.Resource.Compute.TargetSSLProxies.Insert
import Network.Google.Resource.Compute.TargetSSLProxies.List
import Network.Google.Resource.Compute.TargetSSLProxies.SetBackendService
import Network.Google.Resource.Compute.TargetSSLProxies.SetProxyHeader
import Network.Google.Resource.Compute.TargetSSLProxies.SetSSLCertificates
import Network.Google.Resource.Compute.TargetTCPProxies.Delete
import Network.Google.Resource.Compute.TargetTCPProxies.Get
import Network.Google.Resource.Compute.TargetTCPProxies.Insert
import Network.Google.Resource.Compute.TargetTCPProxies.List
import Network.Google.Resource.Compute.TargetTCPProxies.SetBackendService
import Network.Google.Resource.Compute.TargetTCPProxies.SetProxyHeader
import Network.Google.Resource.Compute.TargetVPNGateways.AggregatedList
import Network.Google.Resource.Compute.TargetVPNGateways.Delete
import Network.Google.Resource.Compute.TargetVPNGateways.Get
import Network.Google.Resource.Compute.TargetVPNGateways.Insert
import Network.Google.Resource.Compute.TargetVPNGateways.List
import Network.Google.Resource.Compute.URLMaps.Delete
import Network.Google.Resource.Compute.URLMaps.Get
import Network.Google.Resource.Compute.URLMaps.Insert
import Network.Google.Resource.Compute.URLMaps.InvalidateCache
import Network.Google.Resource.Compute.URLMaps.List
import Network.Google.Resource.Compute.URLMaps.Patch
import Network.Google.Resource.Compute.URLMaps.Update
import Network.Google.Resource.Compute.URLMaps.Validate
import Network.Google.Resource.Compute.VPNTunnels.AggregatedList
import Network.Google.Resource.Compute.VPNTunnels.Delete
import Network.Google.Resource.Compute.VPNTunnels.Get
import Network.Google.Resource.Compute.VPNTunnels.Insert
import Network.Google.Resource.Compute.VPNTunnels.List
import Network.Google.Resource.Compute.ZoneOperations.Delete
import Network.Google.Resource.Compute.ZoneOperations.Get
import Network.Google.Resource.Compute.ZoneOperations.List
import Network.Google.Resource.Compute.Zones.Get
import Network.Google.Resource.Compute.Zones.List

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Compute Engine API service.
type ComputeAPI =
     ImagesInsertResource :<|> ImagesListResource :<|>
       ImagesGetResource
       :<|> ImagesGetFromFamilyResource
       :<|> ImagesSetLabelsResource
       :<|> ImagesDeprecateResource
       :<|> ImagesDeleteResource
       :<|> AcceleratorTypesAggregatedListResource
       :<|> AcceleratorTypesListResource
       :<|> AcceleratorTypesGetResource
       :<|> URLMapsInsertResource
       :<|> URLMapsListResource
       :<|> URLMapsPatchResource
       :<|> URLMapsGetResource
       :<|> URLMapsInvalidateCacheResource
       :<|> URLMapsValidateResource
       :<|> URLMapsDeleteResource
       :<|> URLMapsUpdateResource
       :<|> TargetTCPProxiesInsertResource
       :<|> TargetTCPProxiesListResource
       :<|> TargetTCPProxiesGetResource
       :<|> TargetTCPProxiesSetBackendServiceResource
       :<|> TargetTCPProxiesDeleteResource
       :<|> TargetTCPProxiesSetProxyHeaderResource
       :<|> RoutesInsertResource
       :<|> RoutesListResource
       :<|> RoutesGetResource
       :<|> RoutesDeleteResource
       :<|> RegionBackendServicesInsertResource
       :<|> RegionBackendServicesListResource
       :<|> RegionBackendServicesGetHealthResource
       :<|> RegionBackendServicesPatchResource
       :<|> RegionBackendServicesGetResource
       :<|> RegionBackendServicesDeleteResource
       :<|> RegionBackendServicesUpdateResource
       :<|> InstanceTemplatesInsertResource
       :<|> InstanceTemplatesListResource
       :<|> InstanceTemplatesGetResource
       :<|> InstanceTemplatesDeleteResource
       :<|> TargetVPNGatewaysAggregatedListResource
       :<|> TargetVPNGatewaysInsertResource
       :<|> TargetVPNGatewaysListResource
       :<|> TargetVPNGatewaysGetResource
       :<|> TargetVPNGatewaysDeleteResource
       :<|> SSLCertificatesInsertResource
       :<|> SSLCertificatesListResource
       :<|> SSLCertificatesGetResource
       :<|> SSLCertificatesDeleteResource
       :<|> ZonesListResource
       :<|> ZonesGetResource
       :<|> RegionInstanceGroupsListResource
       :<|> RegionInstanceGroupsGetResource
       :<|> RegionInstanceGroupsListInstancesResource
       :<|> RegionInstanceGroupsSetNamedPortsResource
       :<|> GlobalForwardingRulesInsertResource
       :<|> GlobalForwardingRulesListResource
       :<|> GlobalForwardingRulesGetResource
       :<|> GlobalForwardingRulesSetTargetResource
       :<|> GlobalForwardingRulesDeleteResource
       :<|> NetworksInsertResource
       :<|> NetworksListResource
       :<|> NetworksPatchResource
       :<|> NetworksGetResource
       :<|> NetworksSwitchToCustomModeResource
       :<|> NetworksRemovePeeringResource
       :<|> NetworksAddPeeringResource
       :<|> NetworksDeleteResource
       :<|> RoutersAggregatedListResource
       :<|> RoutersInsertResource
       :<|> RoutersListResource
       :<|> RoutersPatchResource
       :<|> RoutersGetResource
       :<|> RoutersPreviewResource
       :<|> RoutersDeleteResource
       :<|> RoutersUpdateResource
       :<|> RoutersGetRouterStatusResource
       :<|> AddressesAggregatedListResource
       :<|> AddressesInsertResource
       :<|> AddressesListResource
       :<|> AddressesGetResource
       :<|> AddressesDeleteResource
       :<|> RegionCommitmentsAggregatedListResource
       :<|> RegionCommitmentsInsertResource
       :<|> RegionCommitmentsListResource
       :<|> RegionCommitmentsGetResource
       :<|> GlobalAddressesInsertResource
       :<|> GlobalAddressesListResource
       :<|> GlobalAddressesGetResource
       :<|> GlobalAddressesDeleteResource
       :<|> RegionsListResource
       :<|> RegionsGetResource
       :<|> TargetHTTPProxiesInsertResource
       :<|> TargetHTTPProxiesListResource
       :<|> TargetHTTPProxiesSetURLMapResource
       :<|> TargetHTTPProxiesGetResource
       :<|> TargetHTTPProxiesDeleteResource
       :<|> VPNTunnelsAggregatedListResource
       :<|> VPNTunnelsInsertResource
       :<|> VPNTunnelsListResource
       :<|> VPNTunnelsGetResource
       :<|> VPNTunnelsDeleteResource
       :<|> AutoscalersAggregatedListResource
       :<|> AutoscalersInsertResource
       :<|> AutoscalersListResource
       :<|> AutoscalersPatchResource
       :<|> AutoscalersGetResource
       :<|> AutoscalersDeleteResource
       :<|> AutoscalersUpdateResource
       :<|>
       RegionInstanceGroupManagersSetTargetPoolsResource
       :<|> RegionInstanceGroupManagersInsertResource
       :<|> RegionInstanceGroupManagersResizeResource
       :<|> RegionInstanceGroupManagersListResource
       :<|>
       RegionInstanceGroupManagersListManagedInstancesResource
       :<|>
       RegionInstanceGroupManagersAbandonInstancesResource
       :<|>
       RegionInstanceGroupManagersSetInstanceTemplateResource
       :<|> RegionInstanceGroupManagersGetResource
       :<|>
       RegionInstanceGroupManagersDeleteInstancesResource
       :<|> RegionInstanceGroupManagersDeleteResource
       :<|>
       RegionInstanceGroupManagersRecreateInstancesResource
       :<|> DiskTypesAggregatedListResource
       :<|> DiskTypesListResource
       :<|> DiskTypesGetResource
       :<|> RegionOperationsListResource
       :<|> RegionOperationsGetResource
       :<|> RegionOperationsDeleteResource
       :<|> MachineTypesAggregatedListResource
       :<|> MachineTypesListResource
       :<|> MachineTypesGetResource
       :<|> HTTPHealthChecksInsertResource
       :<|> HTTPHealthChecksListResource
       :<|> HTTPHealthChecksPatchResource
       :<|> HTTPHealthChecksGetResource
       :<|> HTTPHealthChecksDeleteResource
       :<|> HTTPHealthChecksUpdateResource
       :<|> SubnetworksExpandIPCIdRRangeResource
       :<|> SubnetworksAggregatedListResource
       :<|> SubnetworksInsertResource
       :<|> SubnetworksListResource
       :<|> SubnetworksGetResource
       :<|> SubnetworksSetPrivateIPGoogleAccessResource
       :<|> SubnetworksDeleteResource
       :<|> InterconnectsInsertResource
       :<|> InterconnectsListResource
       :<|> InterconnectsPatchResource
       :<|> InterconnectsGetResource
       :<|> InterconnectsDeleteResource
       :<|> InterconnectLocationsListResource
       :<|> InterconnectLocationsGetResource
       :<|> SnapshotsListResource
       :<|> SnapshotsGetResource
       :<|> SnapshotsSetLabelsResource
       :<|> SnapshotsDeleteResource
       :<|> TargetSSLProxiesInsertResource
       :<|> TargetSSLProxiesListResource
       :<|> TargetSSLProxiesSetSSLCertificatesResource
       :<|> TargetSSLProxiesGetResource
       :<|> TargetSSLProxiesSetBackendServiceResource
       :<|> TargetSSLProxiesDeleteResource
       :<|> TargetSSLProxiesSetProxyHeaderResource
       :<|> HTTPSHealthChecksInsertResource
       :<|> HTTPSHealthChecksListResource
       :<|> HTTPSHealthChecksPatchResource
       :<|> HTTPSHealthChecksGetResource
       :<|> HTTPSHealthChecksDeleteResource
       :<|> HTTPSHealthChecksUpdateResource
       :<|> InstancesAddAccessConfigResource
       :<|> InstancesAggregatedListResource
       :<|> InstancesInsertResource
       :<|> InstancesDetachDiskResource
       :<|> InstancesListResource
       :<|> InstancesSetDeletionProtectionResource
       :<|> InstancesStartResource
       :<|> InstancesSetServiceAccountResource
       :<|> InstancesGetResource
       :<|> InstancesSetMachineResourcesResource
       :<|> InstancesSetLabelsResource
       :<|> InstancesSetMachineTypeResource
       :<|> InstancesDeleteAccessConfigResource
       :<|> InstancesSetMetadataResource
       :<|> InstancesSetSchedulingResource
       :<|> InstancesStartWithEncryptionKeyResource
       :<|> InstancesResetResource
       :<|> InstancesStopResource
       :<|> InstancesGetSerialPortOutputResource
       :<|> InstancesSetTagsResource
       :<|> InstancesAttachDiskResource
       :<|> InstancesDeleteResource
       :<|> InstancesSetDiskAutoDeleteResource
       :<|> InstancesSetMinCPUPlatformResource
       :<|> BackendServicesAggregatedListResource
       :<|> BackendServicesInsertResource
       :<|> BackendServicesListResource
       :<|> BackendServicesGetHealthResource
       :<|> BackendServicesPatchResource
       :<|> BackendServicesGetResource
       :<|> BackendServicesDeleteResource
       :<|> BackendServicesUpdateResource
       :<|> InstanceGroupsRemoveInstancesResource
       :<|> InstanceGroupsAggregatedListResource
       :<|> InstanceGroupsInsertResource
       :<|> InstanceGroupsListResource
       :<|> InstanceGroupsAddInstancesResource
       :<|> InstanceGroupsGetResource
       :<|> InstanceGroupsListInstancesResource
       :<|> InstanceGroupsSetNamedPortsResource
       :<|> InstanceGroupsDeleteResource
       :<|> FirewallsInsertResource
       :<|> FirewallsListResource
       :<|> FirewallsPatchResource
       :<|> FirewallsGetResource
       :<|> FirewallsDeleteResource
       :<|> FirewallsUpdateResource
       :<|> GlobalOperationsAggregatedListResource
       :<|> GlobalOperationsListResource
       :<|> GlobalOperationsGetResource
       :<|> GlobalOperationsDeleteResource
       :<|> LicensesGetResource
       :<|> ZoneOperationsListResource
       :<|> ZoneOperationsGetResource
       :<|> ZoneOperationsDeleteResource
       :<|> InterconnectAttachmentsAggregatedListResource
       :<|> InterconnectAttachmentsInsertResource
       :<|> InterconnectAttachmentsListResource
       :<|> InterconnectAttachmentsGetResource
       :<|> InterconnectAttachmentsDeleteResource
       :<|> TargetHTTPSProxiesInsertResource
       :<|> TargetHTTPSProxiesListResource
       :<|> TargetHTTPSProxiesSetURLMapResource
       :<|> TargetHTTPSProxiesSetSSLCertificatesResource
       :<|> TargetHTTPSProxiesGetResource
       :<|> TargetHTTPSProxiesDeleteResource
       :<|> TargetInstancesAggregatedListResource
       :<|> TargetInstancesInsertResource
       :<|> TargetInstancesListResource
       :<|> TargetInstancesGetResource
       :<|> TargetInstancesDeleteResource
       :<|> DisksAggregatedListResource
       :<|> DisksInsertResource
       :<|> DisksResizeResource
       :<|> DisksListResource
       :<|> DisksGetResource
       :<|> DisksSetLabelsResource
       :<|> DisksCreateSnapshotResource
       :<|> DisksDeleteResource
       :<|> ForwardingRulesAggregatedListResource
       :<|> ForwardingRulesInsertResource
       :<|> ForwardingRulesListResource
       :<|> ForwardingRulesGetResource
       :<|> ForwardingRulesSetTargetResource
       :<|> ForwardingRulesDeleteResource
       :<|> InstanceGroupManagersSetTargetPoolsResource
       :<|> InstanceGroupManagersAggregatedListResource
       :<|> InstanceGroupManagersInsertResource
       :<|> InstanceGroupManagersResizeResource
       :<|> InstanceGroupManagersListResource
       :<|>
       InstanceGroupManagersListManagedInstancesResource
       :<|> InstanceGroupManagersAbandonInstancesResource
       :<|> InstanceGroupManagersSetInstanceTemplateResource
       :<|> InstanceGroupManagersGetResource
       :<|> InstanceGroupManagersDeleteInstancesResource
       :<|> InstanceGroupManagersDeleteResource
       :<|> InstanceGroupManagersRecreateInstancesResource
       :<|> RegionAutoscalersInsertResource
       :<|> RegionAutoscalersListResource
       :<|> RegionAutoscalersPatchResource
       :<|> RegionAutoscalersGetResource
       :<|> RegionAutoscalersDeleteResource
       :<|> RegionAutoscalersUpdateResource
       :<|> ProjectsEnableXpnHostResource
       :<|> ProjectsListXpnHostsResource
       :<|> ProjectsDisableXpnHostResource
       :<|> ProjectsEnableXpnResourceResource
       :<|> ProjectsSetUsageExportBucketResource
       :<|> ProjectsMoveInstanceResource
       :<|> ProjectsDisableXpnResourceResource
       :<|> ProjectsGetXpnHostResource
       :<|> ProjectsGetResource
       :<|> ProjectsMoveDiskResource
       :<|> ProjectsSetCommonInstanceMetadataResource
       :<|> ProjectsGetXpnResourcesResource
       :<|> TargetPoolsAggregatedListResource
       :<|> TargetPoolsRemoveInstanceResource
       :<|> TargetPoolsInsertResource
       :<|> TargetPoolsListResource
       :<|> TargetPoolsGetHealthResource
       :<|> TargetPoolsGetResource
       :<|> TargetPoolsRemoveHealthCheckResource
       :<|> TargetPoolsSetBackupResource
       :<|> TargetPoolsAddInstanceResource
       :<|> TargetPoolsAddHealthCheckResource
       :<|> TargetPoolsDeleteResource
       :<|> HealthChecksInsertResource
       :<|> HealthChecksListResource
       :<|> HealthChecksPatchResource
       :<|> HealthChecksGetResource
       :<|> HealthChecksDeleteResource
       :<|> HealthChecksUpdateResource
       :<|> BackendBucketsInsertResource
       :<|> BackendBucketsListResource
       :<|> BackendBucketsPatchResource
       :<|> BackendBucketsGetResource
       :<|> BackendBucketsDeleteResource
       :<|> BackendBucketsUpdateResource
