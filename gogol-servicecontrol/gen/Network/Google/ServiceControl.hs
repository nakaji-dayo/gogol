{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.ServiceControl
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Google Service Control provides control plane functionality to managed
-- services, such as logging, monitoring, and status checks.
--
-- /See:/ <https://cloud.google.com/service-control/ Google Service Control API Reference>
module Network.Google.ServiceControl
    (
    -- * Service Configuration
      serviceControlService

    -- * OAuth Scopes
    , serviceControlScope
    , cloudPlatformScope

    -- * API Declaration
    , ServiceControlAPI

    -- * Resources

    -- ** servicecontrol.services.allocateQuota
    , module Network.Google.Resource.ServiceControl.Services.AllocateQuota

    -- ** servicecontrol.services.check
    , module Network.Google.Resource.ServiceControl.Services.Check

    -- ** servicecontrol.services.endReconciliation
    , module Network.Google.Resource.ServiceControl.Services.EndReconciliation

    -- ** servicecontrol.services.releaseQuota
    , module Network.Google.Resource.ServiceControl.Services.ReleaseQuota

    -- ** servicecontrol.services.report
    , module Network.Google.Resource.ServiceControl.Services.Report

    -- ** servicecontrol.services.startReconciliation
    , module Network.Google.Resource.ServiceControl.Services.StartReconciliation

    -- * Types

    -- ** StartReconciliationResponse
    , StartReconciliationResponse
    , startReconciliationResponse
    , srrQuotaMetrics
    , srrReconciliationErrors
    , srrServiceConfigId
    , srrOperationId

    -- ** AuditLogServiceData
    , AuditLogServiceData
    , auditLogServiceData
    , alsdAddtional

    -- ** AuditLogMetadata
    , AuditLogMetadata
    , auditLogMetadata
    , almAddtional

    -- ** Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- ** RequestMetadata
    , RequestMetadata
    , requestMetadata
    , rmCallerSuppliedUserAgent
    , rmCallerIP
    , rmCallerNetwork

    -- ** ReportRequest
    , ReportRequest
    , reportRequest
    , rrServiceConfigId
    , rrOperations

    -- ** CheckRequest
    , CheckRequest
    , checkRequest
    , crOperation
    , crRequestProjectSettings
    , crServiceConfigId
    , crSkipActivationCheck

    -- ** MetricValueLabels
    , MetricValueLabels
    , metricValueLabels
    , mvlAddtional

    -- ** Distribution
    , Distribution
    , distribution
    , dSumOfSquaredDeviation
    , dMean
    , dExponentialBuckets
    , dMaximum
    , dLinearBuckets
    , dCount
    , dMinimum
    , dBucketCounts
    , dExplicitBuckets

    -- ** QuotaPropertiesQuotaMode
    , QuotaPropertiesQuotaMode (..)

    -- ** ExponentialBuckets
    , ExponentialBuckets
    , exponentialBuckets
    , ebGrowthFactor
    , ebScale
    , ebNumFiniteBuckets

    -- ** AuthorizationInfo
    , AuthorizationInfo
    , authorizationInfo
    , aiGranted
    , aiResource
    , aiPermission

    -- ** Operation
    , Operation
    , operation
    , oImportance
    , oResourceContainer
    , oStartTime
    , oResources
    , oUserLabels
    , oQuotaProperties
    , oEndTime
    , oOperationName
    , oLabels
    , oOperationId
    , oConsumerId
    , oMetricValueSets
    , oLogEntries

    -- ** MetricValueSet
    , MetricValueSet
    , metricValueSet
    , mvsMetricName
    , mvsMetricValues

    -- ** LogEntryLabels
    , LogEntryLabels
    , logEntryLabels
    , lelAddtional

    -- ** LogEntryProtoPayload
    , LogEntryProtoPayload
    , logEntryProtoPayload
    , leppAddtional

    -- ** LinearBuckets
    , LinearBuckets
    , linearBuckets
    , lbOffSet
    , lbWidth
    , lbNumFiniteBuckets

    -- ** QuotaInfo
    , QuotaInfo
    , quotaInfo
    , qiLimitExceeded
    , qiQuotaMetrics
    , qiQuotaConsumed

    -- ** Money
    , Money
    , money
    , mCurrencyCode
    , mNanos
    , mUnits

    -- ** ConsumerInfo
    , ConsumerInfo
    , consumerInfo
    , ciProjectNumber

    -- ** AllocateInfo
    , AllocateInfo
    , allocateInfo
    , aiUnusedArguments

    -- ** CheckErrorCode
    , CheckErrorCode (..)

    -- ** QuotaOperation
    , QuotaOperation
    , quotaOperation
    , qoQuotaMode
    , qoMethodName
    , qoQuotaMetrics
    , qoLabels
    , qoOperationId
    , qoConsumerId

    -- ** StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- ** CheckError
    , CheckError
    , checkError
    , ceCode
    , ceDetail

    -- ** QuotaOperationQuotaMode
    , QuotaOperationQuotaMode (..)

    -- ** QuotaOperationLabels
    , QuotaOperationLabels
    , quotaOperationLabels
    , qolAddtional

    -- ** ReportError
    , ReportError
    , reportError
    , reStatus
    , reOperationId

    -- ** ReleaseQuotaRequest
    , ReleaseQuotaRequest
    , releaseQuotaRequest
    , rqrReleaseOperation
    , rqrServiceConfigId

    -- ** OperationImportance
    , OperationImportance (..)

    -- ** EndReconciliationRequest
    , EndReconciliationRequest
    , endReconciliationRequest
    , errServiceConfigId
    , errReconciliationOperation

    -- ** AuthenticationInfoThirdPartyPrincipal
    , AuthenticationInfoThirdPartyPrincipal
    , authenticationInfoThirdPartyPrincipal
    , aitppAddtional

    -- ** ReleaseQuotaResponse
    , ReleaseQuotaResponse
    , releaseQuotaResponse
    , rQuotaMetrics
    , rServiceConfigId
    , rReleaseErrors
    , rOperationId

    -- ** ResourceInfo
    , ResourceInfo
    , resourceInfo
    , riResourceName
    , riResourceContainer

    -- ** MetricValue
    , MetricValue
    , metricValue
    , mvBoolValue
    , mvStartTime
    , mvMoneyValue
    , mvDoubleValue
    , mvStringValue
    , mvDistributionValue
    , mvEndTime
    , mvInt64Value
    , mvLabels

    -- ** QuotaProperties
    , QuotaProperties
    , quotaProperties
    , qpQuotaMode

    -- ** AllocateQuotaRequest
    , AllocateQuotaRequest
    , allocateQuotaRequest
    , aqrServiceConfigId
    , aqrAllocateOperation

    -- ** Xgafv
    , Xgafv (..)

    -- ** AuditLogResponse
    , AuditLogResponse
    , auditLogResponse
    , alrAddtional

    -- ** CheckResponse
    , CheckResponse
    , checkResponse
    , cCheckErrors
    , cQuotaInfo
    , cServiceConfigId
    , cCheckInfo
    , cOperationId

    -- ** ReportResponse
    , ReportResponse
    , reportResponse
    , repReportErrors
    , repReportInfos
    , repServiceConfigId

    -- ** ReportInfo
    , ReportInfo
    , reportInfo
    , riQuotaInfo
    , riOperationId

    -- ** LogEntry
    , LogEntry
    , logEntry
    , leSeverity
    , leTextPayload
    , leStructPayload
    , leName
    , leInsertId
    , leLabels
    , leProtoPayload
    , leTimestamp

    -- ** AuditLogRequest
    , AuditLogRequest
    , auditLogRequest
    , aAddtional

    -- ** OperationLabels
    , OperationLabels
    , operationLabels
    , olAddtional

    -- ** CheckInfo
    , CheckInfo
    , checkInfo
    , ciConsumerInfo
    , ciUnusedArguments

    -- ** AllocateQuotaResponse
    , AllocateQuotaResponse
    , allocateQuotaResponse
    , aAllocateInfo
    , aAllocateErrors
    , aQuotaMetrics
    , aServiceConfigId
    , aOperationId

    -- ** EndReconciliationResponse
    , EndReconciliationResponse
    , endReconciliationResponse
    , eQuotaMetrics
    , eReconciliationErrors
    , eServiceConfigId
    , eOperationId

    -- ** LogEntrySeverity
    , LogEntrySeverity (..)

    -- ** ExplicitBuckets
    , ExplicitBuckets
    , explicitBuckets
    , ebBounds

    -- ** QuotaErrorCode
    , QuotaErrorCode (..)

    -- ** QuotaError
    , QuotaError
    , quotaError
    , qeSubject
    , qeCode
    , qeDescription

    -- ** StartReconciliationRequest
    , StartReconciliationRequest
    , startReconciliationRequest
    , sServiceConfigId
    , sReconciliationOperation

    -- ** AuthenticationInfo
    , AuthenticationInfo
    , authenticationInfo
    , aiThirdPartyPrincipal
    , aiPrincipalEmail
    , aiAuthoritySelector

    -- ** QuotaInfoQuotaConsumed
    , QuotaInfoQuotaConsumed
    , quotaInfoQuotaConsumed
    , qiqcAddtional

    -- ** AuditLog
    , AuditLog
    , auditLog
    , alRequestMetadata
    , alStatus
    , alResourceName
    , alAuthorizationInfo
    , alServiceData
    , alMethodName
    , alResponse
    , alServiceName
    , alMetadata
    , alNumResponseItems
    , alAuthenticationInfo
    , alRequest

    -- ** OperationUserLabels
    , OperationUserLabels
    , operationUserLabels
    , oulAddtional

    -- ** LogEntryStructPayload
    , LogEntryStructPayload
    , logEntryStructPayload
    , lespAddtional
    ) where

import Network.Google.Prelude
import Network.Google.Resource.ServiceControl.Services.AllocateQuota
import Network.Google.Resource.ServiceControl.Services.Check
import Network.Google.Resource.ServiceControl.Services.EndReconciliation
import Network.Google.Resource.ServiceControl.Services.ReleaseQuota
import Network.Google.Resource.ServiceControl.Services.Report
import Network.Google.Resource.ServiceControl.Services.StartReconciliation
import Network.Google.ServiceControl.Types

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Service Control API service.
type ServiceControlAPI =
     ServicesStartReconciliationResource :<|>
       ServicesReportResource
       :<|> ServicesCheckResource
       :<|> ServicesReleaseQuotaResource
       :<|> ServicesEndReconciliationResource
       :<|> ServicesAllocateQuotaResource
