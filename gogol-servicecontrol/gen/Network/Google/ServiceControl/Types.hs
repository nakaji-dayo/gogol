{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.ServiceControl.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.ServiceControl.Types
    (
    -- * Service Configuration
      serviceControlService

    -- * OAuth Scopes
    , serviceControlScope
    , cloudPlatformScope

    -- * StartReconciliationResponse
    , StartReconciliationResponse
    , startReconciliationResponse
    , srrQuotaMetrics
    , srrReconciliationErrors
    , srrServiceConfigId
    , srrOperationId

    -- * AuditLogServiceData
    , AuditLogServiceData
    , auditLogServiceData
    , alsdAddtional

    -- * AuditLogMetadata
    , AuditLogMetadata
    , auditLogMetadata
    , almAddtional

    -- * Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- * RequestMetadata
    , RequestMetadata
    , requestMetadata
    , rmCallerSuppliedUserAgent
    , rmCallerIP
    , rmCallerNetwork

    -- * ReportRequest
    , ReportRequest
    , reportRequest
    , rrServiceConfigId
    , rrOperations

    -- * CheckRequest
    , CheckRequest
    , checkRequest
    , crOperation
    , crRequestProjectSettings
    , crServiceConfigId
    , crSkipActivationCheck

    -- * MetricValueLabels
    , MetricValueLabels
    , metricValueLabels
    , mvlAddtional

    -- * Distribution
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

    -- * QuotaPropertiesQuotaMode
    , QuotaPropertiesQuotaMode (..)

    -- * ExponentialBuckets
    , ExponentialBuckets
    , exponentialBuckets
    , ebGrowthFactor
    , ebScale
    , ebNumFiniteBuckets

    -- * AuthorizationInfo
    , AuthorizationInfo
    , authorizationInfo
    , aiGranted
    , aiResource
    , aiPermission

    -- * Operation
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

    -- * MetricValueSet
    , MetricValueSet
    , metricValueSet
    , mvsMetricName
    , mvsMetricValues

    -- * LogEntryLabels
    , LogEntryLabels
    , logEntryLabels
    , lelAddtional

    -- * LogEntryProtoPayload
    , LogEntryProtoPayload
    , logEntryProtoPayload
    , leppAddtional

    -- * LinearBuckets
    , LinearBuckets
    , linearBuckets
    , lbOffSet
    , lbWidth
    , lbNumFiniteBuckets

    -- * QuotaInfo
    , QuotaInfo
    , quotaInfo
    , qiLimitExceeded
    , qiQuotaMetrics
    , qiQuotaConsumed

    -- * Money
    , Money
    , money
    , mCurrencyCode
    , mNanos
    , mUnits

    -- * ConsumerInfo
    , ConsumerInfo
    , consumerInfo
    , ciProjectNumber

    -- * AllocateInfo
    , AllocateInfo
    , allocateInfo
    , aiUnusedArguments

    -- * CheckErrorCode
    , CheckErrorCode (..)

    -- * QuotaOperation
    , QuotaOperation
    , quotaOperation
    , qoQuotaMode
    , qoMethodName
    , qoQuotaMetrics
    , qoLabels
    , qoOperationId
    , qoConsumerId

    -- * StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- * CheckError
    , CheckError
    , checkError
    , ceCode
    , ceDetail

    -- * QuotaOperationQuotaMode
    , QuotaOperationQuotaMode (..)

    -- * QuotaOperationLabels
    , QuotaOperationLabels
    , quotaOperationLabels
    , qolAddtional

    -- * ReportError
    , ReportError
    , reportError
    , reStatus
    , reOperationId

    -- * ReleaseQuotaRequest
    , ReleaseQuotaRequest
    , releaseQuotaRequest
    , rqrReleaseOperation
    , rqrServiceConfigId

    -- * OperationImportance
    , OperationImportance (..)

    -- * EndReconciliationRequest
    , EndReconciliationRequest
    , endReconciliationRequest
    , errServiceConfigId
    , errReconciliationOperation

    -- * AuthenticationInfoThirdPartyPrincipal
    , AuthenticationInfoThirdPartyPrincipal
    , authenticationInfoThirdPartyPrincipal
    , aitppAddtional

    -- * ReleaseQuotaResponse
    , ReleaseQuotaResponse
    , releaseQuotaResponse
    , rQuotaMetrics
    , rServiceConfigId
    , rReleaseErrors
    , rOperationId

    -- * ResourceInfo
    , ResourceInfo
    , resourceInfo
    , riResourceName
    , riResourceContainer

    -- * MetricValue
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

    -- * QuotaProperties
    , QuotaProperties
    , quotaProperties
    , qpQuotaMode

    -- * AllocateQuotaRequest
    , AllocateQuotaRequest
    , allocateQuotaRequest
    , aqrServiceConfigId
    , aqrAllocateOperation

    -- * Xgafv
    , Xgafv (..)

    -- * AuditLogResponse
    , AuditLogResponse
    , auditLogResponse
    , alrAddtional

    -- * CheckResponse
    , CheckResponse
    , checkResponse
    , cCheckErrors
    , cQuotaInfo
    , cServiceConfigId
    , cCheckInfo
    , cOperationId

    -- * ReportResponse
    , ReportResponse
    , reportResponse
    , repReportErrors
    , repReportInfos
    , repServiceConfigId

    -- * ReportInfo
    , ReportInfo
    , reportInfo
    , riQuotaInfo
    , riOperationId

    -- * LogEntry
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

    -- * AuditLogRequest
    , AuditLogRequest
    , auditLogRequest
    , aAddtional

    -- * OperationLabels
    , OperationLabels
    , operationLabels
    , olAddtional

    -- * CheckInfo
    , CheckInfo
    , checkInfo
    , ciConsumerInfo
    , ciUnusedArguments

    -- * AllocateQuotaResponse
    , AllocateQuotaResponse
    , allocateQuotaResponse
    , aAllocateInfo
    , aAllocateErrors
    , aQuotaMetrics
    , aServiceConfigId
    , aOperationId

    -- * EndReconciliationResponse
    , EndReconciliationResponse
    , endReconciliationResponse
    , eQuotaMetrics
    , eReconciliationErrors
    , eServiceConfigId
    , eOperationId

    -- * LogEntrySeverity
    , LogEntrySeverity (..)

    -- * ExplicitBuckets
    , ExplicitBuckets
    , explicitBuckets
    , ebBounds

    -- * QuotaErrorCode
    , QuotaErrorCode (..)

    -- * QuotaError
    , QuotaError
    , quotaError
    , qeSubject
    , qeCode
    , qeDescription

    -- * StartReconciliationRequest
    , StartReconciliationRequest
    , startReconciliationRequest
    , sServiceConfigId
    , sReconciliationOperation

    -- * AuthenticationInfo
    , AuthenticationInfo
    , authenticationInfo
    , aiThirdPartyPrincipal
    , aiPrincipalEmail
    , aiAuthoritySelector

    -- * QuotaInfoQuotaConsumed
    , QuotaInfoQuotaConsumed
    , quotaInfoQuotaConsumed
    , qiqcAddtional

    -- * AuditLog
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

    -- * OperationUserLabels
    , OperationUserLabels
    , operationUserLabels
    , oulAddtional

    -- * LogEntryStructPayload
    , LogEntryStructPayload
    , logEntryStructPayload
    , lespAddtional
    ) where

import Network.Google.Prelude
import Network.Google.ServiceControl.Types.Product
import Network.Google.ServiceControl.Types.Sum

-- | Default request referring to version 'v1' of the Google Service Control API. This contains the host and root path used as a starting point for constructing service requests.
serviceControlService :: ServiceConfig
serviceControlService
  = defaultService (ServiceId "servicecontrol:v1")
      "servicecontrol.googleapis.com"

-- | Manage your Google Service Control data
serviceControlScope :: Proxy '["https://www.googleapis.com/auth/servicecontrol"]
serviceControlScope = Proxy;

-- | View and manage your data across Google Cloud Platform services
cloudPlatformScope :: Proxy '["https://www.googleapis.com/auth/cloud-platform"]
cloudPlatformScope = Proxy;
