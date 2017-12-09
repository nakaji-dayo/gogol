{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Monitoring
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Manages your Stackdriver Monitoring data and configurations. Most
-- projects must be associated with a Stackdriver account, with a few
-- exceptions as noted on the individual method pages.
--
-- /See:/ <https://cloud.google.com/monitoring/api/ Stackdriver Monitoring API Reference>
module Network.Google.Monitoring
    (
    -- * Service Configuration
      monitoringService

    -- * OAuth Scopes
    , monitoringReadScope
    , cloudPlatformScope
    , monitoringScope
    , monitoringWriteScope

    -- * API Declaration
    , MonitoringAPI

    -- * Resources

    -- ** monitoring.projects.collectdTimeSeries.create
    , module Network.Google.Resource.Monitoring.Projects.CollectdTimeSeries.Create

    -- ** monitoring.projects.groups.create
    , module Network.Google.Resource.Monitoring.Projects.Groups.Create

    -- ** monitoring.projects.groups.delete
    , module Network.Google.Resource.Monitoring.Projects.Groups.Delete

    -- ** monitoring.projects.groups.get
    , module Network.Google.Resource.Monitoring.Projects.Groups.Get

    -- ** monitoring.projects.groups.list
    , module Network.Google.Resource.Monitoring.Projects.Groups.List

    -- ** monitoring.projects.groups.members.list
    , module Network.Google.Resource.Monitoring.Projects.Groups.Members.List

    -- ** monitoring.projects.groups.update
    , module Network.Google.Resource.Monitoring.Projects.Groups.Update

    -- ** monitoring.projects.metricDescriptors.create
    , module Network.Google.Resource.Monitoring.Projects.MetricDescriptors.Create

    -- ** monitoring.projects.metricDescriptors.delete
    , module Network.Google.Resource.Monitoring.Projects.MetricDescriptors.Delete

    -- ** monitoring.projects.metricDescriptors.get
    , module Network.Google.Resource.Monitoring.Projects.MetricDescriptors.Get

    -- ** monitoring.projects.metricDescriptors.list
    , module Network.Google.Resource.Monitoring.Projects.MetricDescriptors.List

    -- ** monitoring.projects.monitoredResourceDescriptors.get
    , module Network.Google.Resource.Monitoring.Projects.MonitoredResourceDescriptors.Get

    -- ** monitoring.projects.monitoredResourceDescriptors.list
    , module Network.Google.Resource.Monitoring.Projects.MonitoredResourceDescriptors.List

    -- ** monitoring.projects.timeSeries.create
    , module Network.Google.Resource.Monitoring.Projects.TimeSeries.Create

    -- ** monitoring.projects.timeSeries.list
    , module Network.Google.Resource.Monitoring.Projects.TimeSeries.List

    -- ** monitoring.projects.uptimeCheckConfigs.create
    , module Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Create

    -- ** monitoring.projects.uptimeCheckConfigs.delete
    , module Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Delete

    -- ** monitoring.projects.uptimeCheckConfigs.get
    , module Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Get

    -- ** monitoring.projects.uptimeCheckConfigs.list
    , module Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.List

    -- ** monitoring.projects.uptimeCheckConfigs.patch
    , module Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Patch

    -- ** monitoring.uptimeCheckIps.list
    , module Network.Google.Resource.Monitoring.UptimeCheckIPs.List

    -- * Types

    -- ** MetricDescriptorValueType
    , MetricDescriptorValueType (..)

    -- ** MonitoredResourceDescriptor
    , MonitoredResourceDescriptor
    , monitoredResourceDescriptor
    , mrdName
    , mrdDisplayName
    , mrdLabels
    , mrdType
    , mrdDescription

    -- ** CollectdValueDataSourceType
    , CollectdValueDataSourceType (..)

    -- ** Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- ** ListTimeSeriesResponse
    , ListTimeSeriesResponse
    , listTimeSeriesResponse
    , ltsrNextPageToken
    , ltsrTimeSeries

    -- ** UptimeCheckIPRegion
    , UptimeCheckIPRegion (..)

    -- ** MetricDescriptor
    , MetricDescriptor
    , metricDescriptor
    , mdMetricKind
    , mdName
    , mdDisplayName
    , mdLabels
    , mdType
    , mdValueType
    , mdDescription
    , mdUnit

    -- ** Group
    , Group
    , group'
    , gName
    , gDisplayName
    , gFilter
    , gIsCluster
    , gParentName

    -- ** TypedValue
    , TypedValue
    , typedValue
    , tvBoolValue
    , tvDoubleValue
    , tvStringValue
    , tvDistributionValue
    , tvInt64Value

    -- ** MonitoredResourceLabels
    , MonitoredResourceLabels
    , monitoredResourceLabels
    , mrlAddtional

    -- ** SourceContext
    , SourceContext
    , sourceContext
    , scFileName

    -- ** BasicAuthentication
    , BasicAuthentication
    , basicAuthentication
    , baUsername
    , baPassword

    -- ** Distribution
    , Distribution
    , distribution
    , dSumOfSquaredDeviation
    , dMean
    , dCount
    , dBucketCounts
    , dRange
    , dBucketOptions

    -- ** Field
    , Field
    , field
    , fKind
    , fOneofIndex
    , fName
    , fJSONName
    , fCardinality
    , fOptions
    , fPacked
    , fDefaultValue
    , fNumber
    , fTypeURL

    -- ** FieldKind
    , FieldKind (..)

    -- ** Empty
    , Empty
    , empty

    -- ** ListGroupsResponse
    , ListGroupsResponse
    , listGroupsResponse
    , lgrNextPageToken
    , lgrGroup

    -- ** ListMetricDescriptorsResponse
    , ListMetricDescriptorsResponse
    , listMetricDescriptorsResponse
    , lmdrMetricDescriptors
    , lmdrNextPageToken

    -- ** OptionValue
    , OptionValue
    , optionValue
    , ovAddtional

    -- ** CreateTimeSeriesRequest
    , CreateTimeSeriesRequest
    , createTimeSeriesRequest
    , ctsrTimeSeries

    -- ** StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- ** InternalChecker
    , InternalChecker
    , internalChecker
    , icNetwork
    , icCheckerId
    , icGcpZone
    , icDisplayName
    , icProjectId

    -- ** ListMonitoredResourceDescriptorsResponse
    , ListMonitoredResourceDescriptorsResponse
    , listMonitoredResourceDescriptorsResponse
    , lmrdrNextPageToken
    , lmrdrResourceDescriptors

    -- ** LabelDescriptorValueType
    , LabelDescriptorValueType (..)

    -- ** Explicit
    , Explicit
    , explicit
    , eBounds

    -- ** MetricLabels
    , MetricLabels
    , metricLabels
    , mlAddtional

    -- ** CollectdPayloadMetadata
    , CollectdPayloadMetadata
    , collectdPayloadMetadata
    , cpmAddtional

    -- ** CollectdValue
    , CollectdValue
    , collectdValue
    , cvDataSourceName
    , cvDataSourceType
    , cvValue

    -- ** CreateCollectdTimeSeriesRequest
    , CreateCollectdTimeSeriesRequest
    , createCollectdTimeSeriesRequest
    , cctsrCollectdPayloads
    , cctsrResource
    , cctsrCollectdVersion

    -- ** TypeSyntax
    , TypeSyntax (..)

    -- ** UptimeCheckConfig
    , UptimeCheckConfig
    , uptimeCheckConfig
    , uccInternalCheckers
    , uccPeriod
    , uccContentMatchers
    , uccName
    , uccMonitoredResource
    , uccSelectedRegions
    , uccIsInternal
    , uccDisplayName
    , uccResourceGroup
    , uccTimeout
    , uccHTTPCheck
    , uccTCPCheck

    -- ** Point
    , Point
    , point
    , pValue
    , pInterval

    -- ** CollectdPayload
    , CollectdPayload
    , collectdPayload
    , cpStartTime
    , cpPluginInstance
    , cpValues
    , cpTypeInstance
    , cpEndTime
    , cpMetadata
    , cpType
    , cpPlugin

    -- ** Metric
    , Metric
    , metric
    , mLabels
    , mType

    -- ** CollectdPayloadError
    , CollectdPayloadError
    , collectdPayloadError
    , cpeError
    , cpeValueErrors
    , cpeIndex

    -- ** Exponential
    , Exponential
    , exponential
    , eGrowthFactor
    , eScale
    , eNumFiniteBuckets

    -- ** ResourceGroupResourceType
    , ResourceGroupResourceType (..)

    -- ** Range
    , Range
    , range
    , rMax
    , rMin

    -- ** MonitoredResource
    , MonitoredResource
    , monitoredResource
    , mrLabels
    , mrType

    -- ** UptimeCheckIP
    , UptimeCheckIP
    , uptimeCheckIP
    , uciIPAddress
    , uciLocation
    , uciRegion

    -- ** Xgafv
    , Xgafv (..)

    -- ** TimeInterval
    , TimeInterval
    , timeInterval
    , tiStartTime
    , tiEndTime

    -- ** HTTPCheckHeaders
    , HTTPCheckHeaders
    , hTTPCheckHeaders
    , httpchAddtional

    -- ** TimeSeriesMetricKind
    , TimeSeriesMetricKind (..)

    -- ** ContentMatcher
    , ContentMatcher
    , contentMatcher
    , cmContent

    -- ** ListGroupMembersResponse
    , ListGroupMembersResponse
    , listGroupMembersResponse
    , lgmrNextPageToken
    , lgmrMembers
    , lgmrTotalSize

    -- ** LabelDescriptor
    , LabelDescriptor
    , labelDescriptor
    , ldKey
    , ldValueType
    , ldDescription

    -- ** TimeSeriesValueType
    , TimeSeriesValueType (..)

    -- ** Linear
    , Linear
    , linear
    , lOffSet
    , lWidth
    , lNumFiniteBuckets

    -- ** ListUptimeCheckIPsResponse
    , ListUptimeCheckIPsResponse
    , listUptimeCheckIPsResponse
    , lucirNextPageToken
    , lucirUptimeCheckIPs

    -- ** ResourceGroup
    , ResourceGroup
    , resourceGroup
    , rgResourceType
    , rgGroupId

    -- ** FieldCardinality
    , FieldCardinality (..)

    -- ** Type
    , Type
    , type'
    , tSourceContext
    , tOneofs
    , tName
    , tOptions
    , tFields
    , tSyntax

    -- ** MetricDescriptorMetricKind
    , MetricDescriptorMetricKind (..)

    -- ** CreateCollectdTimeSeriesResponse
    , CreateCollectdTimeSeriesResponse
    , createCollectdTimeSeriesResponse
    , cctsrPayloadErrors

    -- ** Option
    , Option
    , option
    , oValue
    , oName

    -- ** BucketOptions
    , BucketOptions
    , bucketOptions
    , boExponentialBuckets
    , boLinearBuckets
    , boExplicitBuckets

    -- ** ListUptimeCheckConfigsResponse
    , ListUptimeCheckConfigsResponse
    , listUptimeCheckConfigsResponse
    , luccrUptimeCheckConfigs
    , luccrNextPageToken

    -- ** HTTPCheck
    , HTTPCheck
    , hTTPCheck
    , httpcUseSSL
    , httpcPath
    , httpcMaskHeaders
    , httpcHeaders
    , httpcAuthInfo
    , httpcPort

    -- ** TimeSeries
    , TimeSeries
    , timeSeries
    , tsPoints
    , tsMetricKind
    , tsMetric
    , tsResource
    , tsValueType

    -- ** TCPCheck
    , TCPCheck
    , tcpCheck
    , tcPort

    -- ** CollectdValueError
    , CollectdValueError
    , collectdValueError
    , cveError
    , cveIndex
    ) where

import Network.Google.Prelude
import Network.Google.Monitoring.Types
import Network.Google.Resource.Monitoring.Projects.CollectdTimeSeries.Create
import Network.Google.Resource.Monitoring.Projects.Groups.Create
import Network.Google.Resource.Monitoring.Projects.Groups.Delete
import Network.Google.Resource.Monitoring.Projects.Groups.Get
import Network.Google.Resource.Monitoring.Projects.Groups.List
import Network.Google.Resource.Monitoring.Projects.Groups.Members.List
import Network.Google.Resource.Monitoring.Projects.Groups.Update
import Network.Google.Resource.Monitoring.Projects.MetricDescriptors.Create
import Network.Google.Resource.Monitoring.Projects.MetricDescriptors.Delete
import Network.Google.Resource.Monitoring.Projects.MetricDescriptors.Get
import Network.Google.Resource.Monitoring.Projects.MetricDescriptors.List
import Network.Google.Resource.Monitoring.Projects.MonitoredResourceDescriptors.Get
import Network.Google.Resource.Monitoring.Projects.MonitoredResourceDescriptors.List
import Network.Google.Resource.Monitoring.Projects.TimeSeries.Create
import Network.Google.Resource.Monitoring.Projects.TimeSeries.List
import Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Create
import Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Delete
import Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Get
import Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.List
import Network.Google.Resource.Monitoring.Projects.UptimeCheckConfigs.Patch
import Network.Google.Resource.Monitoring.UptimeCheckIPs.List

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Stackdriver Monitoring API service.
type MonitoringAPI =
     UptimeCheckIPsListResource :<|>
       ProjectsMetricDescriptorsListResource
       :<|> ProjectsMetricDescriptorsGetResource
       :<|> ProjectsMetricDescriptorsCreateResource
       :<|> ProjectsMetricDescriptorsDeleteResource
       :<|> ProjectsGroupsMembersListResource
       :<|> ProjectsGroupsListResource
       :<|> ProjectsGroupsGetResource
       :<|> ProjectsGroupsCreateResource
       :<|> ProjectsGroupsDeleteResource
       :<|> ProjectsGroupsUpdateResource
       :<|> ProjectsCollectdTimeSeriesCreateResource
       :<|> ProjectsUptimeCheckConfigsListResource
       :<|> ProjectsUptimeCheckConfigsPatchResource
       :<|> ProjectsUptimeCheckConfigsGetResource
       :<|> ProjectsUptimeCheckConfigsCreateResource
       :<|> ProjectsUptimeCheckConfigsDeleteResource
       :<|> ProjectsMonitoredResourceDescriptorsListResource
       :<|> ProjectsMonitoredResourceDescriptorsGetResource
       :<|> ProjectsTimeSeriesListResource
       :<|> ProjectsTimeSeriesCreateResource
