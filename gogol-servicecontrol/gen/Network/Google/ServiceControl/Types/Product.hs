{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.ServiceControl.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.ServiceControl.Types.Product where

import Network.Google.Prelude
import Network.Google.ServiceControl.Types.Sum

-- | Response message for QuotaController.StartReconciliation.
--
-- /See:/ 'startReconciliationResponse' smart constructor.
data StartReconciliationResponse = StartReconciliationResponse'
    { _srrQuotaMetrics :: !(Maybe [MetricValueSet])
    , _srrReconciliationErrors :: !(Maybe [QuotaError])
    , _srrServiceConfigId :: !(Maybe Text)
    , _srrOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartReconciliationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srrQuotaMetrics'
--
-- * 'srrReconciliationErrors'
--
-- * 'srrServiceConfigId'
--
-- * 'srrOperationId'
startReconciliationResponse
    :: StartReconciliationResponse
startReconciliationResponse = 
    StartReconciliationResponse'
    { _srrQuotaMetrics = Nothing
    , _srrReconciliationErrors = Nothing
    , _srrServiceConfigId = Nothing
    , _srrOperationId = Nothing
    }

-- | Metric values as tracked by One Platform before the start of
-- reconciliation. The following metrics will be included: 1. Per quota
-- metric total usage will be specified using the following gauge metric:
-- \"serviceruntime.googleapis.com\/allocation\/consumer\/quota_used_count\"
-- 2. Value for each quota limit associated with the metrics will be
-- specified using the following gauge metric:
-- \"serviceruntime.googleapis.com\/quota\/limit\"
srrQuotaMetrics :: Lens' StartReconciliationResponse [MetricValueSet]
srrQuotaMetrics
  = lens _srrQuotaMetrics
      (\ s a -> s{_srrQuotaMetrics = a})
      . _Default
      . _Coerce

-- | Indicates the decision of the reconciliation start.
srrReconciliationErrors :: Lens' StartReconciliationResponse [QuotaError]
srrReconciliationErrors
  = lens _srrReconciliationErrors
      (\ s a -> s{_srrReconciliationErrors = a})
      . _Default
      . _Coerce

-- | ID of the actual config used to process the request.
srrServiceConfigId :: Lens' StartReconciliationResponse (Maybe Text)
srrServiceConfigId
  = lens _srrServiceConfigId
      (\ s a -> s{_srrServiceConfigId = a})

-- | The same operation_id value used in the StartReconciliationRequest. Used
-- for logging and diagnostics purposes.
srrOperationId :: Lens' StartReconciliationResponse (Maybe Text)
srrOperationId
  = lens _srrOperationId
      (\ s a -> s{_srrOperationId = a})

instance FromJSON StartReconciliationResponse where
        parseJSON
          = withObject "StartReconciliationResponse"
              (\ o ->
                 StartReconciliationResponse' <$>
                   (o .:? "quotaMetrics" .!= mempty) <*>
                     (o .:? "reconciliationErrors" .!= mempty)
                     <*> (o .:? "serviceConfigId")
                     <*> (o .:? "operationId"))

instance ToJSON StartReconciliationResponse where
        toJSON StartReconciliationResponse'{..}
          = object
              (catMaybes
                 [("quotaMetrics" .=) <$> _srrQuotaMetrics,
                  ("reconciliationErrors" .=) <$>
                    _srrReconciliationErrors,
                  ("serviceConfigId" .=) <$> _srrServiceConfigId,
                  ("operationId" .=) <$> _srrOperationId])

-- | Deprecated, use \`metadata\` field instead. Other service-specific data
-- about the request, response, and other activities.
--
-- /See:/ 'auditLogServiceData' smart constructor.
newtype AuditLogServiceData = AuditLogServiceData'
    { _alsdAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuditLogServiceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alsdAddtional'
auditLogServiceData
    :: HashMap Text JSONValue -- ^ 'alsdAddtional'
    -> AuditLogServiceData
auditLogServiceData pAlsdAddtional_ = 
    AuditLogServiceData'
    { _alsdAddtional = _Coerce # pAlsdAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
alsdAddtional :: Lens' AuditLogServiceData (HashMap Text JSONValue)
alsdAddtional
  = lens _alsdAddtional
      (\ s a -> s{_alsdAddtional = a})
      . _Coerce

instance FromJSON AuditLogServiceData where
        parseJSON
          = withObject "AuditLogServiceData"
              (\ o -> AuditLogServiceData' <$> (parseJSONObject o))

instance ToJSON AuditLogServiceData where
        toJSON = toJSON . _alsdAddtional

-- | Other service-specific data about the request, response, and other
-- information associated with the current audited event.
--
-- /See:/ 'auditLogMetadata' smart constructor.
newtype AuditLogMetadata = AuditLogMetadata'
    { _almAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuditLogMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'almAddtional'
auditLogMetadata
    :: HashMap Text JSONValue -- ^ 'almAddtional'
    -> AuditLogMetadata
auditLogMetadata pAlmAddtional_ = 
    AuditLogMetadata'
    { _almAddtional = _Coerce # pAlmAddtional_
    }

-- | Properties of the object.
almAddtional :: Lens' AuditLogMetadata (HashMap Text JSONValue)
almAddtional
  = lens _almAddtional (\ s a -> s{_almAddtional = a})
      . _Coerce

instance FromJSON AuditLogMetadata where
        parseJSON
          = withObject "AuditLogMetadata"
              (\ o -> AuditLogMetadata' <$> (parseJSONObject o))

instance ToJSON AuditLogMetadata where
        toJSON = toJSON . _almAddtional

-- | The \`Status\` type defines a logical error model that is suitable for
-- different programming environments, including REST APIs and RPC APIs. It
-- is used by [gRPC](https:\/\/github.com\/grpc). The error model is
-- designed to be: - Simple to use and understand for most users - Flexible
-- enough to meet unexpected needs # Overview The \`Status\` message
-- contains three pieces of data: error code, error message, and error
-- details. The error code should be an enum value of google.rpc.Code, but
-- it may accept additional error codes if needed. The error message should
-- be a developer-facing English message that helps developers *understand*
-- and *resolve* the error. If a localized user-facing error message is
-- needed, put the localized message in the error details or localize it in
-- the client. The optional error details may contain arbitrary information
-- about the error. There is a predefined set of error detail types in the
-- package \`google.rpc\` that can be used for common error conditions. #
-- Language mapping The \`Status\` message is the logical representation of
-- the error model, but it is not necessarily the actual wire format. When
-- the \`Status\` message is exposed in different client libraries and
-- different wire protocols, it can be mapped differently. For example, it
-- will likely be mapped to some exceptions in Java, but more likely mapped
-- to some error codes in C. # Other uses The error model and the
-- \`Status\` message can be used in a variety of environments, either with
-- or without APIs, to provide a consistent developer experience across
-- different environments. Example uses of this error model include: -
-- Partial errors. If a service needs to return partial errors to the
-- client, it may embed the \`Status\` in the normal response to indicate
-- the partial errors. - Workflow errors. A typical workflow has multiple
-- steps. Each step may have a \`Status\` message for error reporting. -
-- Batch operations. If a client uses batch request and batch response, the
-- \`Status\` message should be used directly inside batch response, one
-- for each error sub-response. - Asynchronous operations. If an API call
-- embeds asynchronous operation results in its response, the status of
-- those operations should be represented directly using the \`Status\`
-- message. - Logging. If some API errors are stored in logs, the message
-- \`Status\` could be used directly after any stripping needed for
-- security\/privacy reasons.
--
-- /See:/ 'status' smart constructor.
data Status = Status'
    { _sDetails :: !(Maybe [StatusDetailsItem])
    , _sCode :: !(Maybe (Textual Int32))
    , _sMessage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Status' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDetails'
--
-- * 'sCode'
--
-- * 'sMessage'
status
    :: Status
status = 
    Status'
    { _sDetails = Nothing
    , _sCode = Nothing
    , _sMessage = Nothing
    }

-- | A list of messages that carry the error details. There is a common set
-- of message types for APIs to use.
sDetails :: Lens' Status [StatusDetailsItem]
sDetails
  = lens _sDetails (\ s a -> s{_sDetails = a}) .
      _Default
      . _Coerce

-- | The status code, which should be an enum value of google.rpc.Code.
sCode :: Lens' Status (Maybe Int32)
sCode
  = lens _sCode (\ s a -> s{_sCode = a}) .
      mapping _Coerce

-- | A developer-facing error message, which should be in English. Any
-- user-facing error message should be localized and sent in the
-- google.rpc.Status.details field, or localized by the client.
sMessage :: Lens' Status (Maybe Text)
sMessage = lens _sMessage (\ s a -> s{_sMessage = a})

instance FromJSON Status where
        parseJSON
          = withObject "Status"
              (\ o ->
                 Status' <$>
                   (o .:? "details" .!= mempty) <*> (o .:? "code") <*>
                     (o .:? "message"))

instance ToJSON Status where
        toJSON Status'{..}
          = object
              (catMaybes
                 [("details" .=) <$> _sDetails,
                  ("code" .=) <$> _sCode,
                  ("message" .=) <$> _sMessage])

-- | Metadata about the request.
--
-- /See:/ 'requestMetadata' smart constructor.
data RequestMetadata = RequestMetadata'
    { _rmCallerSuppliedUserAgent :: !(Maybe Text)
    , _rmCallerIP :: !(Maybe Text)
    , _rmCallerNetwork :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RequestMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmCallerSuppliedUserAgent'
--
-- * 'rmCallerIP'
--
-- * 'rmCallerNetwork'
requestMetadata
    :: RequestMetadata
requestMetadata = 
    RequestMetadata'
    { _rmCallerSuppliedUserAgent = Nothing
    , _rmCallerIP = Nothing
    , _rmCallerNetwork = Nothing
    }

-- | The user agent of the caller. This information is not authenticated and
-- should be treated accordingly. For example: +
-- \`google-api-python-client\/1.4.0\`: The request was made by the Google
-- API client for Python. + \`Cloud SDK Command Line Tool
-- apitools-client\/1.0 gcloud\/0.9.62\`: The request was made by the
-- Google Cloud SDK CLI (gcloud). + \`AppEngine-Google;
-- (+http:\/\/code.google.com\/appengine; appid: s~my-project\`: The
-- request was made from the \`my-project\` App Engine app. NOLINT
rmCallerSuppliedUserAgent :: Lens' RequestMetadata (Maybe Text)
rmCallerSuppliedUserAgent
  = lens _rmCallerSuppliedUserAgent
      (\ s a -> s{_rmCallerSuppliedUserAgent = a})

-- | The IP address of the caller. For caller from internet, this will be
-- public IPv4 or IPv6 address. For caller from a Compute Engine VM with
-- external IP address, this will be the VM\'s external IP address. For
-- caller from a Compute Engine VM without external IP address, if the VM
-- is in the same organization (or project) as the accessed resource,
-- \`caller_ip\` will be the VM\'s internal IPv4 address, otherwise the
-- \`caller_ip\` will be redacted to \"gce-internal-ip\". See
-- https:\/\/cloud.google.com\/compute\/docs\/vpc\/ for more information.
rmCallerIP :: Lens' RequestMetadata (Maybe Text)
rmCallerIP
  = lens _rmCallerIP (\ s a -> s{_rmCallerIP = a})

-- | The network of the caller. Set only if the network host project is part
-- of the same GCP organization (or project) as the accessed resource. See
-- https:\/\/cloud.google.com\/compute\/docs\/vpc\/ for more information.
-- This is a scheme-less URI full resource name. For example:
-- \"\/\/compute.googleapis.com\/projects\/PROJECT_ID\/global\/networks\/NETWORK_ID\"
rmCallerNetwork :: Lens' RequestMetadata (Maybe Text)
rmCallerNetwork
  = lens _rmCallerNetwork
      (\ s a -> s{_rmCallerNetwork = a})

instance FromJSON RequestMetadata where
        parseJSON
          = withObject "RequestMetadata"
              (\ o ->
                 RequestMetadata' <$>
                   (o .:? "callerSuppliedUserAgent") <*>
                     (o .:? "callerIp")
                     <*> (o .:? "callerNetwork"))

instance ToJSON RequestMetadata where
        toJSON RequestMetadata'{..}
          = object
              (catMaybes
                 [("callerSuppliedUserAgent" .=) <$>
                    _rmCallerSuppliedUserAgent,
                  ("callerIp" .=) <$> _rmCallerIP,
                  ("callerNetwork" .=) <$> _rmCallerNetwork])

-- | Request message for the Report method.
--
-- /See:/ 'reportRequest' smart constructor.
data ReportRequest = ReportRequest'
    { _rrServiceConfigId :: !(Maybe Text)
    , _rrOperations :: !(Maybe [Operation])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReportRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrServiceConfigId'
--
-- * 'rrOperations'
reportRequest
    :: ReportRequest
reportRequest = 
    ReportRequest'
    { _rrServiceConfigId = Nothing
    , _rrOperations = Nothing
    }

-- | Specifies which version of service config should be used to process the
-- request. If unspecified or no matching version can be found, the latest
-- one will be used.
rrServiceConfigId :: Lens' ReportRequest (Maybe Text)
rrServiceConfigId
  = lens _rrServiceConfigId
      (\ s a -> s{_rrServiceConfigId = a})

-- | Operations to be reported. Typically the service should report one
-- operation per request. Putting multiple operations into a single request
-- is allowed, but should be used only when multiple operations are
-- natually available at the time of the report. If multiple operations are
-- in a single request, the total request size should be no larger than
-- 1MB. See ReportResponse.report_errors for partial failure behavior.
rrOperations :: Lens' ReportRequest [Operation]
rrOperations
  = lens _rrOperations (\ s a -> s{_rrOperations = a})
      . _Default
      . _Coerce

instance FromJSON ReportRequest where
        parseJSON
          = withObject "ReportRequest"
              (\ o ->
                 ReportRequest' <$>
                   (o .:? "serviceConfigId") <*>
                     (o .:? "operations" .!= mempty))

instance ToJSON ReportRequest where
        toJSON ReportRequest'{..}
          = object
              (catMaybes
                 [("serviceConfigId" .=) <$> _rrServiceConfigId,
                  ("operations" .=) <$> _rrOperations])

-- | Request message for the Check method.
--
-- /See:/ 'checkRequest' smart constructor.
data CheckRequest = CheckRequest'
    { _crOperation :: !(Maybe Operation)
    , _crRequestProjectSettings :: !(Maybe Bool)
    , _crServiceConfigId :: !(Maybe Text)
    , _crSkipActivationCheck :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CheckRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crOperation'
--
-- * 'crRequestProjectSettings'
--
-- * 'crServiceConfigId'
--
-- * 'crSkipActivationCheck'
checkRequest
    :: CheckRequest
checkRequest = 
    CheckRequest'
    { _crOperation = Nothing
    , _crRequestProjectSettings = Nothing
    , _crServiceConfigId = Nothing
    , _crSkipActivationCheck = Nothing
    }

-- | The operation to be checked.
crOperation :: Lens' CheckRequest (Maybe Operation)
crOperation
  = lens _crOperation (\ s a -> s{_crOperation = a})

-- | Requests the project settings to be returned as part of the check
-- response.
crRequestProjectSettings :: Lens' CheckRequest (Maybe Bool)
crRequestProjectSettings
  = lens _crRequestProjectSettings
      (\ s a -> s{_crRequestProjectSettings = a})

-- | Specifies which version of service configuration should be used to
-- process the request. If unspecified or no matching version can be found,
-- the latest one will be used.
crServiceConfigId :: Lens' CheckRequest (Maybe Text)
crServiceConfigId
  = lens _crServiceConfigId
      (\ s a -> s{_crServiceConfigId = a})

-- | Indicates if service activation check should be skipped for this
-- request. Default behavior is to perform the check and apply relevant
-- quota.
crSkipActivationCheck :: Lens' CheckRequest (Maybe Bool)
crSkipActivationCheck
  = lens _crSkipActivationCheck
      (\ s a -> s{_crSkipActivationCheck = a})

instance FromJSON CheckRequest where
        parseJSON
          = withObject "CheckRequest"
              (\ o ->
                 CheckRequest' <$>
                   (o .:? "operation") <*>
                     (o .:? "requestProjectSettings")
                     <*> (o .:? "serviceConfigId")
                     <*> (o .:? "skipActivationCheck"))

instance ToJSON CheckRequest where
        toJSON CheckRequest'{..}
          = object
              (catMaybes
                 [("operation" .=) <$> _crOperation,
                  ("requestProjectSettings" .=) <$>
                    _crRequestProjectSettings,
                  ("serviceConfigId" .=) <$> _crServiceConfigId,
                  ("skipActivationCheck" .=) <$>
                    _crSkipActivationCheck])

-- | The labels describing the metric value. See comments on
-- google.api.servicecontrol.v1.Operation.labels for the overriding
-- relationship.
--
-- /See:/ 'metricValueLabels' smart constructor.
newtype MetricValueLabels = MetricValueLabels'
    { _mvlAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricValueLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvlAddtional'
metricValueLabels
    :: HashMap Text Text -- ^ 'mvlAddtional'
    -> MetricValueLabels
metricValueLabels pMvlAddtional_ = 
    MetricValueLabels'
    { _mvlAddtional = _Coerce # pMvlAddtional_
    }

mvlAddtional :: Lens' MetricValueLabels (HashMap Text Text)
mvlAddtional
  = lens _mvlAddtional (\ s a -> s{_mvlAddtional = a})
      . _Coerce

instance FromJSON MetricValueLabels where
        parseJSON
          = withObject "MetricValueLabels"
              (\ o -> MetricValueLabels' <$> (parseJSONObject o))

instance ToJSON MetricValueLabels where
        toJSON = toJSON . _mvlAddtional

-- | Distribution represents a frequency distribution of double-valued sample
-- points. It contains the size of the population of sample points plus
-- additional optional information: - the arithmetic mean of the samples -
-- the minimum and maximum of the samples - the sum-squared-deviation of
-- the samples, used to compute variance - a histogram of the values of the
-- sample points
--
-- /See:/ 'distribution' smart constructor.
data Distribution = Distribution'
    { _dSumOfSquaredDeviation :: !(Maybe (Textual Double))
    , _dMean :: !(Maybe (Textual Double))
    , _dExponentialBuckets :: !(Maybe ExponentialBuckets)
    , _dMaximum :: !(Maybe (Textual Double))
    , _dLinearBuckets :: !(Maybe LinearBuckets)
    , _dCount :: !(Maybe (Textual Int64))
    , _dMinimum :: !(Maybe (Textual Double))
    , _dBucketCounts :: !(Maybe [Textual Int64])
    , _dExplicitBuckets :: !(Maybe ExplicitBuckets)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Distribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSumOfSquaredDeviation'
--
-- * 'dMean'
--
-- * 'dExponentialBuckets'
--
-- * 'dMaximum'
--
-- * 'dLinearBuckets'
--
-- * 'dCount'
--
-- * 'dMinimum'
--
-- * 'dBucketCounts'
--
-- * 'dExplicitBuckets'
distribution
    :: Distribution
distribution = 
    Distribution'
    { _dSumOfSquaredDeviation = Nothing
    , _dMean = Nothing
    , _dExponentialBuckets = Nothing
    , _dMaximum = Nothing
    , _dLinearBuckets = Nothing
    , _dCount = Nothing
    , _dMinimum = Nothing
    , _dBucketCounts = Nothing
    , _dExplicitBuckets = Nothing
    }

-- | The sum of squared deviations from the mean: Sum[i=1..count]((x_i -
-- mean)^2) where each x_i is a sample values. If \`count\` is zero then
-- this field must be zero, otherwise validation of the request fails.
dSumOfSquaredDeviation :: Lens' Distribution (Maybe Double)
dSumOfSquaredDeviation
  = lens _dSumOfSquaredDeviation
      (\ s a -> s{_dSumOfSquaredDeviation = a})
      . mapping _Coerce

-- | The arithmetic mean of the samples in the distribution. If \`count\` is
-- zero then this field must be zero.
dMean :: Lens' Distribution (Maybe Double)
dMean
  = lens _dMean (\ s a -> s{_dMean = a}) .
      mapping _Coerce

-- | Buckets with exponentially growing width.
dExponentialBuckets :: Lens' Distribution (Maybe ExponentialBuckets)
dExponentialBuckets
  = lens _dExponentialBuckets
      (\ s a -> s{_dExponentialBuckets = a})

-- | The maximum of the population of values. Ignored if \`count\` is zero.
dMaximum :: Lens' Distribution (Maybe Double)
dMaximum
  = lens _dMaximum (\ s a -> s{_dMaximum = a}) .
      mapping _Coerce

-- | Buckets with constant width.
dLinearBuckets :: Lens' Distribution (Maybe LinearBuckets)
dLinearBuckets
  = lens _dLinearBuckets
      (\ s a -> s{_dLinearBuckets = a})

-- | The total number of samples in the distribution. Must be >= 0.
dCount :: Lens' Distribution (Maybe Int64)
dCount
  = lens _dCount (\ s a -> s{_dCount = a}) .
      mapping _Coerce

-- | The minimum of the population of values. Ignored if \`count\` is zero.
dMinimum :: Lens' Distribution (Maybe Double)
dMinimum
  = lens _dMinimum (\ s a -> s{_dMinimum = a}) .
      mapping _Coerce

-- | The number of samples in each histogram bucket. \`bucket_counts\` are
-- optional. If present, they must sum to the \`count\` value. The buckets
-- are defined below in \`bucket_option\`. There are N buckets.
-- \`bucket_counts[0]\` is the number of samples in the underflow bucket.
-- \`bucket_counts[1]\` to \`bucket_counts[N-1]\` are the numbers of
-- samples in each of the finite buckets. And \`bucket_counts[N] is the
-- number of samples in the overflow bucket. See the comments of
-- \`bucket_option\` below for more details. Any suffix of trailing zeros
-- may be omitted.
dBucketCounts :: Lens' Distribution [Int64]
dBucketCounts
  = lens _dBucketCounts
      (\ s a -> s{_dBucketCounts = a})
      . _Default
      . _Coerce

-- | Buckets with arbitrary user-provided width.
dExplicitBuckets :: Lens' Distribution (Maybe ExplicitBuckets)
dExplicitBuckets
  = lens _dExplicitBuckets
      (\ s a -> s{_dExplicitBuckets = a})

instance FromJSON Distribution where
        parseJSON
          = withObject "Distribution"
              (\ o ->
                 Distribution' <$>
                   (o .:? "sumOfSquaredDeviation") <*> (o .:? "mean")
                     <*> (o .:? "exponentialBuckets")
                     <*> (o .:? "maximum")
                     <*> (o .:? "linearBuckets")
                     <*> (o .:? "count")
                     <*> (o .:? "minimum")
                     <*> (o .:? "bucketCounts" .!= mempty)
                     <*> (o .:? "explicitBuckets"))

instance ToJSON Distribution where
        toJSON Distribution'{..}
          = object
              (catMaybes
                 [("sumOfSquaredDeviation" .=) <$>
                    _dSumOfSquaredDeviation,
                  ("mean" .=) <$> _dMean,
                  ("exponentialBuckets" .=) <$> _dExponentialBuckets,
                  ("maximum" .=) <$> _dMaximum,
                  ("linearBuckets" .=) <$> _dLinearBuckets,
                  ("count" .=) <$> _dCount,
                  ("minimum" .=) <$> _dMinimum,
                  ("bucketCounts" .=) <$> _dBucketCounts,
                  ("explicitBuckets" .=) <$> _dExplicitBuckets])

-- | Describing buckets with exponentially growing width.
--
-- /See:/ 'exponentialBuckets' smart constructor.
data ExponentialBuckets = ExponentialBuckets'
    { _ebGrowthFactor :: !(Maybe (Textual Double))
    , _ebScale :: !(Maybe (Textual Double))
    , _ebNumFiniteBuckets :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExponentialBuckets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebGrowthFactor'
--
-- * 'ebScale'
--
-- * 'ebNumFiniteBuckets'
exponentialBuckets
    :: ExponentialBuckets
exponentialBuckets = 
    ExponentialBuckets'
    { _ebGrowthFactor = Nothing
    , _ebScale = Nothing
    , _ebNumFiniteBuckets = Nothing
    }

-- | The i\'th exponential bucket covers the interval [scale *
-- growth_factor^(i-1), scale * growth_factor^i) where i ranges from 1 to
-- num_finite_buckets inclusive. Must be larger than 1.0.
ebGrowthFactor :: Lens' ExponentialBuckets (Maybe Double)
ebGrowthFactor
  = lens _ebGrowthFactor
      (\ s a -> s{_ebGrowthFactor = a})
      . mapping _Coerce

-- | The i\'th exponential bucket covers the interval [scale *
-- growth_factor^(i-1), scale * growth_factor^i) where i ranges from 1 to
-- num_finite_buckets inclusive. Must be > 0.
ebScale :: Lens' ExponentialBuckets (Maybe Double)
ebScale
  = lens _ebScale (\ s a -> s{_ebScale = a}) .
      mapping _Coerce

-- | The number of finite buckets. With the underflow and overflow buckets,
-- the total number of buckets is \`num_finite_buckets\` + 2. See comments
-- on \`bucket_options\` for details.
ebNumFiniteBuckets :: Lens' ExponentialBuckets (Maybe Int32)
ebNumFiniteBuckets
  = lens _ebNumFiniteBuckets
      (\ s a -> s{_ebNumFiniteBuckets = a})
      . mapping _Coerce

instance FromJSON ExponentialBuckets where
        parseJSON
          = withObject "ExponentialBuckets"
              (\ o ->
                 ExponentialBuckets' <$>
                   (o .:? "growthFactor") <*> (o .:? "scale") <*>
                     (o .:? "numFiniteBuckets"))

instance ToJSON ExponentialBuckets where
        toJSON ExponentialBuckets'{..}
          = object
              (catMaybes
                 [("growthFactor" .=) <$> _ebGrowthFactor,
                  ("scale" .=) <$> _ebScale,
                  ("numFiniteBuckets" .=) <$> _ebNumFiniteBuckets])

-- | Authorization information for the operation.
--
-- /See:/ 'authorizationInfo' smart constructor.
data AuthorizationInfo = AuthorizationInfo'
    { _aiGranted :: !(Maybe Bool)
    , _aiResource :: !(Maybe Text)
    , _aiPermission :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuthorizationInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiGranted'
--
-- * 'aiResource'
--
-- * 'aiPermission'
authorizationInfo
    :: AuthorizationInfo
authorizationInfo = 
    AuthorizationInfo'
    { _aiGranted = Nothing
    , _aiResource = Nothing
    , _aiPermission = Nothing
    }

-- | Whether or not authorization for \`resource\` and \`permission\` was
-- granted.
aiGranted :: Lens' AuthorizationInfo (Maybe Bool)
aiGranted
  = lens _aiGranted (\ s a -> s{_aiGranted = a})

-- | The resource being accessed, as a REST-style string. For example:
-- bigquery.googleapis.com\/projects\/PROJECTID\/datasets\/DATASETID
aiResource :: Lens' AuthorizationInfo (Maybe Text)
aiResource
  = lens _aiResource (\ s a -> s{_aiResource = a})

-- | The required IAM permission.
aiPermission :: Lens' AuthorizationInfo (Maybe Text)
aiPermission
  = lens _aiPermission (\ s a -> s{_aiPermission = a})

instance FromJSON AuthorizationInfo where
        parseJSON
          = withObject "AuthorizationInfo"
              (\ o ->
                 AuthorizationInfo' <$>
                   (o .:? "granted") <*> (o .:? "resource") <*>
                     (o .:? "permission"))

instance ToJSON AuthorizationInfo where
        toJSON AuthorizationInfo'{..}
          = object
              (catMaybes
                 [("granted" .=) <$> _aiGranted,
                  ("resource" .=) <$> _aiResource,
                  ("permission" .=) <$> _aiPermission])

-- | Represents information regarding an operation.
--
-- /See:/ 'operation' smart constructor.
data Operation = Operation'
    { _oImportance :: !(Maybe OperationImportance)
    , _oResourceContainer :: !(Maybe Text)
    , _oStartTime :: !(Maybe DateTime')
    , _oResources :: !(Maybe [ResourceInfo])
    , _oUserLabels :: !(Maybe OperationUserLabels)
    , _oQuotaProperties :: !(Maybe QuotaProperties)
    , _oEndTime :: !(Maybe DateTime')
    , _oOperationName :: !(Maybe Text)
    , _oLabels :: !(Maybe OperationLabels)
    , _oOperationId :: !(Maybe Text)
    , _oConsumerId :: !(Maybe Text)
    , _oMetricValueSets :: !(Maybe [MetricValueSet])
    , _oLogEntries :: !(Maybe [LogEntry])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oImportance'
--
-- * 'oResourceContainer'
--
-- * 'oStartTime'
--
-- * 'oResources'
--
-- * 'oUserLabels'
--
-- * 'oQuotaProperties'
--
-- * 'oEndTime'
--
-- * 'oOperationName'
--
-- * 'oLabels'
--
-- * 'oOperationId'
--
-- * 'oConsumerId'
--
-- * 'oMetricValueSets'
--
-- * 'oLogEntries'
operation
    :: Operation
operation = 
    Operation'
    { _oImportance = Nothing
    , _oResourceContainer = Nothing
    , _oStartTime = Nothing
    , _oResources = Nothing
    , _oUserLabels = Nothing
    , _oQuotaProperties = Nothing
    , _oEndTime = Nothing
    , _oOperationName = Nothing
    , _oLabels = Nothing
    , _oOperationId = Nothing
    , _oConsumerId = Nothing
    , _oMetricValueSets = Nothing
    , _oLogEntries = Nothing
    }

-- | DO NOT USE. This is an experimental field.
oImportance :: Lens' Operation (Maybe OperationImportance)
oImportance
  = lens _oImportance (\ s a -> s{_oImportance = a})

-- | DO NOT USE. This field is deprecated, use \"resources\" field instead.
-- The resource name of the parent of a resource in the resource hierarchy.
-- This can be in one of the following formats: - “projects\/” -
-- “folders\/” - “organizations\/”
oResourceContainer :: Lens' Operation (Maybe Text)
oResourceContainer
  = lens _oResourceContainer
      (\ s a -> s{_oResourceContainer = a})

-- | Required. Start time of the operation.
oStartTime :: Lens' Operation (Maybe UTCTime)
oStartTime
  = lens _oStartTime (\ s a -> s{_oStartTime = a}) .
      mapping _DateTime

-- | The resources that are involved in the operation.
oResources :: Lens' Operation [ResourceInfo]
oResources
  = lens _oResources (\ s a -> s{_oResources = a}) .
      _Default
      . _Coerce

-- | User defined labels for the resource that this operation is associated
-- with. Only a combination of 1000 user labels per consumer project are
-- allowed.
oUserLabels :: Lens' Operation (Maybe OperationUserLabels)
oUserLabels
  = lens _oUserLabels (\ s a -> s{_oUserLabels = a})

-- | Represents the properties needed for quota check. Applicable only if
-- this operation is for a quota check request. If this is not specified,
-- no quota check will be performed.
oQuotaProperties :: Lens' Operation (Maybe QuotaProperties)
oQuotaProperties
  = lens _oQuotaProperties
      (\ s a -> s{_oQuotaProperties = a})

-- | End time of the operation. Required when the operation is used in
-- ServiceController.Report, but optional when the operation is used in
-- ServiceController.Check.
oEndTime :: Lens' Operation (Maybe UTCTime)
oEndTime
  = lens _oEndTime (\ s a -> s{_oEndTime = a}) .
      mapping _DateTime

-- | Fully qualified name of the operation. Reserved for future use.
oOperationName :: Lens' Operation (Maybe Text)
oOperationName
  = lens _oOperationName
      (\ s a -> s{_oOperationName = a})

-- | Labels describing the operation. Only the following labels are allowed:
-- - Labels describing monitored resources as defined in the service
-- configuration. - Default labels of metric values. When specified, labels
-- defined in the metric value override these default. - The following
-- labels defined by Google Cloud Platform: -
-- \`cloud.googleapis.com\/location\` describing the location where the
-- operation happened, - \`servicecontrol.googleapis.com\/user_agent\`
-- describing the user agent of the API request, -
-- \`servicecontrol.googleapis.com\/service_agent\` describing the service
-- used to handle the API request (e.g. ESP), -
-- \`servicecontrol.googleapis.com\/platform\` describing the platform
-- where the API is served (e.g. GAE, GCE, GKE).
oLabels :: Lens' Operation (Maybe OperationLabels)
oLabels = lens _oLabels (\ s a -> s{_oLabels = a})

-- | Identity of the operation. This must be unique within the scope of the
-- service that generated the operation. If the service calls Check() and
-- Report() on the same operation, the two calls should carry the same id.
-- UUID version 4 is recommended, though not required. In scenarios where
-- an operation is computed from existing information and an idempotent id
-- is desirable for deduplication purpose, UUID version 5 is recommended.
-- See RFC 4122 for details.
oOperationId :: Lens' Operation (Maybe Text)
oOperationId
  = lens _oOperationId (\ s a -> s{_oOperationId = a})

-- | Identity of the consumer who is using the service. This field should be
-- filled in for the operations initiated by a consumer, but not for
-- service-initiated operations that are not related to a specific
-- consumer. This can be in one of the following formats: project:,
-- project_number:, api_key:.
oConsumerId :: Lens' Operation (Maybe Text)
oConsumerId
  = lens _oConsumerId (\ s a -> s{_oConsumerId = a})

-- | Represents information about this operation. Each MetricValueSet
-- corresponds to a metric defined in the service configuration. The data
-- type used in the MetricValueSet must agree with the data type specified
-- in the metric definition. Within a single operation, it is not allowed
-- to have more than one MetricValue instances that have the same metric
-- names and identical label value combinations. If a request has such
-- duplicated MetricValue instances, the entire request is rejected with an
-- invalid argument error.
oMetricValueSets :: Lens' Operation [MetricValueSet]
oMetricValueSets
  = lens _oMetricValueSets
      (\ s a -> s{_oMetricValueSets = a})
      . _Default
      . _Coerce

-- | Represents information to be logged.
oLogEntries :: Lens' Operation [LogEntry]
oLogEntries
  = lens _oLogEntries (\ s a -> s{_oLogEntries = a}) .
      _Default
      . _Coerce

instance FromJSON Operation where
        parseJSON
          = withObject "Operation"
              (\ o ->
                 Operation' <$>
                   (o .:? "importance") <*> (o .:? "resourceContainer")
                     <*> (o .:? "startTime")
                     <*> (o .:? "resources" .!= mempty)
                     <*> (o .:? "userLabels")
                     <*> (o .:? "quotaProperties")
                     <*> (o .:? "endTime")
                     <*> (o .:? "operationName")
                     <*> (o .:? "labels")
                     <*> (o .:? "operationId")
                     <*> (o .:? "consumerId")
                     <*> (o .:? "metricValueSets" .!= mempty)
                     <*> (o .:? "logEntries" .!= mempty))

instance ToJSON Operation where
        toJSON Operation'{..}
          = object
              (catMaybes
                 [("importance" .=) <$> _oImportance,
                  ("resourceContainer" .=) <$> _oResourceContainer,
                  ("startTime" .=) <$> _oStartTime,
                  ("resources" .=) <$> _oResources,
                  ("userLabels" .=) <$> _oUserLabels,
                  ("quotaProperties" .=) <$> _oQuotaProperties,
                  ("endTime" .=) <$> _oEndTime,
                  ("operationName" .=) <$> _oOperationName,
                  ("labels" .=) <$> _oLabels,
                  ("operationId" .=) <$> _oOperationId,
                  ("consumerId" .=) <$> _oConsumerId,
                  ("metricValueSets" .=) <$> _oMetricValueSets,
                  ("logEntries" .=) <$> _oLogEntries])

-- | Represents a set of metric values in the same metric. Each metric value
-- in the set should have a unique combination of start time, end time, and
-- label values.
--
-- /See:/ 'metricValueSet' smart constructor.
data MetricValueSet = MetricValueSet'
    { _mvsMetricName :: !(Maybe Text)
    , _mvsMetricValues :: !(Maybe [MetricValue])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricValueSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvsMetricName'
--
-- * 'mvsMetricValues'
metricValueSet
    :: MetricValueSet
metricValueSet = 
    MetricValueSet'
    { _mvsMetricName = Nothing
    , _mvsMetricValues = Nothing
    }

-- | The metric name defined in the service configuration.
mvsMetricName :: Lens' MetricValueSet (Maybe Text)
mvsMetricName
  = lens _mvsMetricName
      (\ s a -> s{_mvsMetricName = a})

-- | The values in this metric.
mvsMetricValues :: Lens' MetricValueSet [MetricValue]
mvsMetricValues
  = lens _mvsMetricValues
      (\ s a -> s{_mvsMetricValues = a})
      . _Default
      . _Coerce

instance FromJSON MetricValueSet where
        parseJSON
          = withObject "MetricValueSet"
              (\ o ->
                 MetricValueSet' <$>
                   (o .:? "metricName") <*>
                     (o .:? "metricValues" .!= mempty))

instance ToJSON MetricValueSet where
        toJSON MetricValueSet'{..}
          = object
              (catMaybes
                 [("metricName" .=) <$> _mvsMetricName,
                  ("metricValues" .=) <$> _mvsMetricValues])

-- | A set of user-defined (key, value) data that provides additional
-- information about the log entry.
--
-- /See:/ 'logEntryLabels' smart constructor.
newtype LogEntryLabels = LogEntryLabels'
    { _lelAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LogEntryLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lelAddtional'
logEntryLabels
    :: HashMap Text Text -- ^ 'lelAddtional'
    -> LogEntryLabels
logEntryLabels pLelAddtional_ = 
    LogEntryLabels'
    { _lelAddtional = _Coerce # pLelAddtional_
    }

lelAddtional :: Lens' LogEntryLabels (HashMap Text Text)
lelAddtional
  = lens _lelAddtional (\ s a -> s{_lelAddtional = a})
      . _Coerce

instance FromJSON LogEntryLabels where
        parseJSON
          = withObject "LogEntryLabels"
              (\ o -> LogEntryLabels' <$> (parseJSONObject o))

instance ToJSON LogEntryLabels where
        toJSON = toJSON . _lelAddtional

-- | The log entry payload, represented as a protocol buffer that is
-- expressed as a JSON object. The only accepted type currently is
-- AuditLog.
--
-- /See:/ 'logEntryProtoPayload' smart constructor.
newtype LogEntryProtoPayload = LogEntryProtoPayload'
    { _leppAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LogEntryProtoPayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leppAddtional'
logEntryProtoPayload
    :: HashMap Text JSONValue -- ^ 'leppAddtional'
    -> LogEntryProtoPayload
logEntryProtoPayload pLeppAddtional_ = 
    LogEntryProtoPayload'
    { _leppAddtional = _Coerce # pLeppAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
leppAddtional :: Lens' LogEntryProtoPayload (HashMap Text JSONValue)
leppAddtional
  = lens _leppAddtional
      (\ s a -> s{_leppAddtional = a})
      . _Coerce

instance FromJSON LogEntryProtoPayload where
        parseJSON
          = withObject "LogEntryProtoPayload"
              (\ o ->
                 LogEntryProtoPayload' <$> (parseJSONObject o))

instance ToJSON LogEntryProtoPayload where
        toJSON = toJSON . _leppAddtional

-- | Describing buckets with constant width.
--
-- /See:/ 'linearBuckets' smart constructor.
data LinearBuckets = LinearBuckets'
    { _lbOffSet :: !(Maybe (Textual Double))
    , _lbWidth :: !(Maybe (Textual Double))
    , _lbNumFiniteBuckets :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LinearBuckets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbOffSet'
--
-- * 'lbWidth'
--
-- * 'lbNumFiniteBuckets'
linearBuckets
    :: LinearBuckets
linearBuckets = 
    LinearBuckets'
    { _lbOffSet = Nothing
    , _lbWidth = Nothing
    , _lbNumFiniteBuckets = Nothing
    }

-- | The i\'th linear bucket covers the interval [offset + (i-1) * width,
-- offset + i * width) where i ranges from 1 to num_finite_buckets,
-- inclusive.
lbOffSet :: Lens' LinearBuckets (Maybe Double)
lbOffSet
  = lens _lbOffSet (\ s a -> s{_lbOffSet = a}) .
      mapping _Coerce

-- | The i\'th linear bucket covers the interval [offset + (i-1) * width,
-- offset + i * width) where i ranges from 1 to num_finite_buckets,
-- inclusive. Must be strictly positive.
lbWidth :: Lens' LinearBuckets (Maybe Double)
lbWidth
  = lens _lbWidth (\ s a -> s{_lbWidth = a}) .
      mapping _Coerce

-- | The number of finite buckets. With the underflow and overflow buckets,
-- the total number of buckets is \`num_finite_buckets\` + 2. See comments
-- on \`bucket_options\` for details.
lbNumFiniteBuckets :: Lens' LinearBuckets (Maybe Int32)
lbNumFiniteBuckets
  = lens _lbNumFiniteBuckets
      (\ s a -> s{_lbNumFiniteBuckets = a})
      . mapping _Coerce

instance FromJSON LinearBuckets where
        parseJSON
          = withObject "LinearBuckets"
              (\ o ->
                 LinearBuckets' <$>
                   (o .:? "offset") <*> (o .:? "width") <*>
                     (o .:? "numFiniteBuckets"))

instance ToJSON LinearBuckets where
        toJSON LinearBuckets'{..}
          = object
              (catMaybes
                 [("offset" .=) <$> _lbOffSet,
                  ("width" .=) <$> _lbWidth,
                  ("numFiniteBuckets" .=) <$> _lbNumFiniteBuckets])

-- | Contains the quota information for a quota check response.
--
-- /See:/ 'quotaInfo' smart constructor.
data QuotaInfo = QuotaInfo'
    { _qiLimitExceeded :: !(Maybe [Text])
    , _qiQuotaMetrics :: !(Maybe [MetricValueSet])
    , _qiQuotaConsumed :: !(Maybe QuotaInfoQuotaConsumed)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'QuotaInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qiLimitExceeded'
--
-- * 'qiQuotaMetrics'
--
-- * 'qiQuotaConsumed'
quotaInfo
    :: QuotaInfo
quotaInfo = 
    QuotaInfo'
    { _qiLimitExceeded = Nothing
    , _qiQuotaMetrics = Nothing
    , _qiQuotaConsumed = Nothing
    }

-- | Quota Metrics that have exceeded quota limits. For QuotaGroup-based
-- quota, this is QuotaGroup.name For QuotaLimit-based quota, this is
-- QuotaLimit.name See: google.api.Quota Deprecated: Use quota_metrics to
-- get per quota group limit exceeded status.
qiLimitExceeded :: Lens' QuotaInfo [Text]
qiLimitExceeded
  = lens _qiLimitExceeded
      (\ s a -> s{_qiLimitExceeded = a})
      . _Default
      . _Coerce

-- | Quota metrics to indicate the usage. Depending on the check request, one
-- or more of the following metrics will be included: 1. For rate quota,
-- per quota group or per quota metric incremental usage will be specified
-- using the following delta metric:
-- \"serviceruntime.googleapis.com\/api\/consumer\/quota_used_count\" 2.
-- For allocation quota, per quota metric total usage will be specified
-- using the following gauge metric:
-- \"serviceruntime.googleapis.com\/allocation\/consumer\/quota_used_count\"
-- 3. For both rate quota and allocation quota, the quota limit reached
-- condition will be specified using the following boolean metric:
-- \"serviceruntime.googleapis.com\/quota\/exceeded\"
qiQuotaMetrics :: Lens' QuotaInfo [MetricValueSet]
qiQuotaMetrics
  = lens _qiQuotaMetrics
      (\ s a -> s{_qiQuotaMetrics = a})
      . _Default
      . _Coerce

-- | Map of quota group name to the actual number of tokens consumed. If the
-- quota check was not successful, then this will not be populated due to
-- no quota consumption. We are not merging this field with
-- \'quota_metrics\' field because of the complexity of scaling in Chemist
-- client code base. For simplicity, we will keep this field for Castor
-- (that scales quota usage) and \'quota_metrics\' for SuperQuota (that
-- doesn\'t scale quota usage).
qiQuotaConsumed :: Lens' QuotaInfo (Maybe QuotaInfoQuotaConsumed)
qiQuotaConsumed
  = lens _qiQuotaConsumed
      (\ s a -> s{_qiQuotaConsumed = a})

instance FromJSON QuotaInfo where
        parseJSON
          = withObject "QuotaInfo"
              (\ o ->
                 QuotaInfo' <$>
                   (o .:? "limitExceeded" .!= mempty) <*>
                     (o .:? "quotaMetrics" .!= mempty)
                     <*> (o .:? "quotaConsumed"))

instance ToJSON QuotaInfo where
        toJSON QuotaInfo'{..}
          = object
              (catMaybes
                 [("limitExceeded" .=) <$> _qiLimitExceeded,
                  ("quotaMetrics" .=) <$> _qiQuotaMetrics,
                  ("quotaConsumed" .=) <$> _qiQuotaConsumed])

-- | Represents an amount of money with its currency type.
--
-- /See:/ 'money' smart constructor.
data Money = Money'
    { _mCurrencyCode :: !(Maybe Text)
    , _mNanos :: !(Maybe (Textual Int32))
    , _mUnits :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Money' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mCurrencyCode'
--
-- * 'mNanos'
--
-- * 'mUnits'
money
    :: Money
money = 
    Money'
    { _mCurrencyCode = Nothing
    , _mNanos = Nothing
    , _mUnits = Nothing
    }

-- | The 3-letter currency code defined in ISO 4217.
mCurrencyCode :: Lens' Money (Maybe Text)
mCurrencyCode
  = lens _mCurrencyCode
      (\ s a -> s{_mCurrencyCode = a})

-- | Number of nano (10^-9) units of the amount. The value must be between
-- -999,999,999 and +999,999,999 inclusive. If \`units\` is positive,
-- \`nanos\` must be positive or zero. If \`units\` is zero, \`nanos\` can
-- be positive, zero, or negative. If \`units\` is negative, \`nanos\` must
-- be negative or zero. For example $-1.75 is represented as \`units\`=-1
-- and \`nanos\`=-750,000,000.
mNanos :: Lens' Money (Maybe Int32)
mNanos
  = lens _mNanos (\ s a -> s{_mNanos = a}) .
      mapping _Coerce

-- | The whole units of the amount. For example if \`currencyCode\` is
-- \`\"USD\"\`, then 1 unit is one US dollar.
mUnits :: Lens' Money (Maybe Int64)
mUnits
  = lens _mUnits (\ s a -> s{_mUnits = a}) .
      mapping _Coerce

instance FromJSON Money where
        parseJSON
          = withObject "Money"
              (\ o ->
                 Money' <$>
                   (o .:? "currencyCode") <*> (o .:? "nanos") <*>
                     (o .:? "units"))

instance ToJSON Money where
        toJSON Money'{..}
          = object
              (catMaybes
                 [("currencyCode" .=) <$> _mCurrencyCode,
                  ("nanos" .=) <$> _mNanos, ("units" .=) <$> _mUnits])

-- | \`ConsumerInfo\` provides information about the consumer project.
--
-- /See:/ 'consumerInfo' smart constructor.
newtype ConsumerInfo = ConsumerInfo'
    { _ciProjectNumber :: Maybe (Textual Int64)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConsumerInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciProjectNumber'
consumerInfo
    :: ConsumerInfo
consumerInfo = 
    ConsumerInfo'
    { _ciProjectNumber = Nothing
    }

-- | The Google cloud project number, e.g. 1234567890. A value of 0 indicates
-- no project number is found.
ciProjectNumber :: Lens' ConsumerInfo (Maybe Int64)
ciProjectNumber
  = lens _ciProjectNumber
      (\ s a -> s{_ciProjectNumber = a})
      . mapping _Coerce

instance FromJSON ConsumerInfo where
        parseJSON
          = withObject "ConsumerInfo"
              (\ o -> ConsumerInfo' <$> (o .:? "projectNumber"))

instance ToJSON ConsumerInfo where
        toJSON ConsumerInfo'{..}
          = object
              (catMaybes
                 [("projectNumber" .=) <$> _ciProjectNumber])

--
-- /See:/ 'allocateInfo' smart constructor.
newtype AllocateInfo = AllocateInfo'
    { _aiUnusedArguments :: Maybe [Text]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllocateInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiUnusedArguments'
allocateInfo
    :: AllocateInfo
allocateInfo = 
    AllocateInfo'
    { _aiUnusedArguments = Nothing
    }

-- | A list of label keys that were unused by the server in processing the
-- request. Thus, for similar requests repeated in a certain future time
-- window, the caller can choose to ignore these labels in the requests to
-- achieve better client-side cache hits and quota aggregation.
aiUnusedArguments :: Lens' AllocateInfo [Text]
aiUnusedArguments
  = lens _aiUnusedArguments
      (\ s a -> s{_aiUnusedArguments = a})
      . _Default
      . _Coerce

instance FromJSON AllocateInfo where
        parseJSON
          = withObject "AllocateInfo"
              (\ o ->
                 AllocateInfo' <$>
                   (o .:? "unusedArguments" .!= mempty))

instance ToJSON AllocateInfo where
        toJSON AllocateInfo'{..}
          = object
              (catMaybes
                 [("unusedArguments" .=) <$> _aiUnusedArguments])

-- | Represents information regarding a quota operation.
--
-- /See:/ 'quotaOperation' smart constructor.
data QuotaOperation = QuotaOperation'
    { _qoQuotaMode :: !(Maybe QuotaOperationQuotaMode)
    , _qoMethodName :: !(Maybe Text)
    , _qoQuotaMetrics :: !(Maybe [MetricValueSet])
    , _qoLabels :: !(Maybe QuotaOperationLabels)
    , _qoOperationId :: !(Maybe Text)
    , _qoConsumerId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'QuotaOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qoQuotaMode'
--
-- * 'qoMethodName'
--
-- * 'qoQuotaMetrics'
--
-- * 'qoLabels'
--
-- * 'qoOperationId'
--
-- * 'qoConsumerId'
quotaOperation
    :: QuotaOperation
quotaOperation = 
    QuotaOperation'
    { _qoQuotaMode = Nothing
    , _qoMethodName = Nothing
    , _qoQuotaMetrics = Nothing
    , _qoLabels = Nothing
    , _qoOperationId = Nothing
    , _qoConsumerId = Nothing
    }

-- | Quota mode for this operation.
qoQuotaMode :: Lens' QuotaOperation (Maybe QuotaOperationQuotaMode)
qoQuotaMode
  = lens _qoQuotaMode (\ s a -> s{_qoQuotaMode = a})

-- | Fully qualified name of the API method for which this quota operation is
-- requested. This name is used for matching quota rules or metric rules
-- and billing status rules defined in service configuration. This field
-- should not be set if any of the following is true: (1) the quota
-- operation is performed on non-API resources. (2) quota_metrics is set
-- because the caller is doing quota override. Example of an RPC method
-- name: google.example.library.v1.LibraryService.CreateShelf
qoMethodName :: Lens' QuotaOperation (Maybe Text)
qoMethodName
  = lens _qoMethodName (\ s a -> s{_qoMethodName = a})

-- | Represents information about this operation. Each MetricValueSet
-- corresponds to a metric defined in the service configuration. The data
-- type used in the MetricValueSet must agree with the data type specified
-- in the metric definition. Within a single operation, it is not allowed
-- to have more than one MetricValue instances that have the same metric
-- names and identical label value combinations. If a request has such
-- duplicated MetricValue instances, the entire request is rejected with an
-- invalid argument error. This field is mutually exclusive with
-- method_name.
qoQuotaMetrics :: Lens' QuotaOperation [MetricValueSet]
qoQuotaMetrics
  = lens _qoQuotaMetrics
      (\ s a -> s{_qoQuotaMetrics = a})
      . _Default
      . _Coerce

-- | Labels describing the operation.
qoLabels :: Lens' QuotaOperation (Maybe QuotaOperationLabels)
qoLabels = lens _qoLabels (\ s a -> s{_qoLabels = a})

-- | Identity of the operation. This is expected to be unique within the
-- scope of the service that generated the operation, and guarantees
-- idempotency in case of retries. UUID version 4 is recommended, though
-- not required. In scenarios where an operation is computed from existing
-- information and an idempotent id is desirable for deduplication purpose,
-- UUID version 5 is recommended. See RFC 4122 for details.
qoOperationId :: Lens' QuotaOperation (Maybe Text)
qoOperationId
  = lens _qoOperationId
      (\ s a -> s{_qoOperationId = a})

-- | Identity of the consumer for whom this quota operation is being
-- performed. This can be in one of the following formats: project:,
-- project_number:, api_key:.
qoConsumerId :: Lens' QuotaOperation (Maybe Text)
qoConsumerId
  = lens _qoConsumerId (\ s a -> s{_qoConsumerId = a})

instance FromJSON QuotaOperation where
        parseJSON
          = withObject "QuotaOperation"
              (\ o ->
                 QuotaOperation' <$>
                   (o .:? "quotaMode") <*> (o .:? "methodName") <*>
                     (o .:? "quotaMetrics" .!= mempty)
                     <*> (o .:? "labels")
                     <*> (o .:? "operationId")
                     <*> (o .:? "consumerId"))

instance ToJSON QuotaOperation where
        toJSON QuotaOperation'{..}
          = object
              (catMaybes
                 [("quotaMode" .=) <$> _qoQuotaMode,
                  ("methodName" .=) <$> _qoMethodName,
                  ("quotaMetrics" .=) <$> _qoQuotaMetrics,
                  ("labels" .=) <$> _qoLabels,
                  ("operationId" .=) <$> _qoOperationId,
                  ("consumerId" .=) <$> _qoConsumerId])

--
-- /See:/ 'statusDetailsItem' smart constructor.
newtype StatusDetailsItem = StatusDetailsItem'
    { _sdiAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StatusDetailsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdiAddtional'
statusDetailsItem
    :: HashMap Text JSONValue -- ^ 'sdiAddtional'
    -> StatusDetailsItem
statusDetailsItem pSdiAddtional_ = 
    StatusDetailsItem'
    { _sdiAddtional = _Coerce # pSdiAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
sdiAddtional :: Lens' StatusDetailsItem (HashMap Text JSONValue)
sdiAddtional
  = lens _sdiAddtional (\ s a -> s{_sdiAddtional = a})
      . _Coerce

instance FromJSON StatusDetailsItem where
        parseJSON
          = withObject "StatusDetailsItem"
              (\ o -> StatusDetailsItem' <$> (parseJSONObject o))

instance ToJSON StatusDetailsItem where
        toJSON = toJSON . _sdiAddtional

-- | Defines the errors to be returned in
-- google.api.servicecontrol.v1.CheckResponse.check_errors.
--
-- /See:/ 'checkError' smart constructor.
data CheckError = CheckError'
    { _ceCode :: !(Maybe CheckErrorCode)
    , _ceDetail :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CheckError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceCode'
--
-- * 'ceDetail'
checkError
    :: CheckError
checkError = 
    CheckError'
    { _ceCode = Nothing
    , _ceDetail = Nothing
    }

-- | The error code.
ceCode :: Lens' CheckError (Maybe CheckErrorCode)
ceCode = lens _ceCode (\ s a -> s{_ceCode = a})

-- | Free-form text providing details on the error cause of the error.
ceDetail :: Lens' CheckError (Maybe Text)
ceDetail = lens _ceDetail (\ s a -> s{_ceDetail = a})

instance FromJSON CheckError where
        parseJSON
          = withObject "CheckError"
              (\ o ->
                 CheckError' <$> (o .:? "code") <*> (o .:? "detail"))

instance ToJSON CheckError where
        toJSON CheckError'{..}
          = object
              (catMaybes
                 [("code" .=) <$> _ceCode,
                  ("detail" .=) <$> _ceDetail])

-- | Labels describing the operation.
--
-- /See:/ 'quotaOperationLabels' smart constructor.
newtype QuotaOperationLabels = QuotaOperationLabels'
    { _qolAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'QuotaOperationLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qolAddtional'
quotaOperationLabels
    :: HashMap Text Text -- ^ 'qolAddtional'
    -> QuotaOperationLabels
quotaOperationLabels pQolAddtional_ = 
    QuotaOperationLabels'
    { _qolAddtional = _Coerce # pQolAddtional_
    }

qolAddtional :: Lens' QuotaOperationLabels (HashMap Text Text)
qolAddtional
  = lens _qolAddtional (\ s a -> s{_qolAddtional = a})
      . _Coerce

instance FromJSON QuotaOperationLabels where
        parseJSON
          = withObject "QuotaOperationLabels"
              (\ o ->
                 QuotaOperationLabels' <$> (parseJSONObject o))

instance ToJSON QuotaOperationLabels where
        toJSON = toJSON . _qolAddtional

-- | Represents the processing error of one Operation in the request.
--
-- /See:/ 'reportError' smart constructor.
data ReportError = ReportError'
    { _reStatus :: !(Maybe Status)
    , _reOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReportError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reStatus'
--
-- * 'reOperationId'
reportError
    :: ReportError
reportError = 
    ReportError'
    { _reStatus = Nothing
    , _reOperationId = Nothing
    }

-- | Details of the error when processing the Operation.
reStatus :: Lens' ReportError (Maybe Status)
reStatus = lens _reStatus (\ s a -> s{_reStatus = a})

-- | The Operation.operation_id value from the request.
reOperationId :: Lens' ReportError (Maybe Text)
reOperationId
  = lens _reOperationId
      (\ s a -> s{_reOperationId = a})

instance FromJSON ReportError where
        parseJSON
          = withObject "ReportError"
              (\ o ->
                 ReportError' <$>
                   (o .:? "status") <*> (o .:? "operationId"))

instance ToJSON ReportError where
        toJSON ReportError'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _reStatus,
                  ("operationId" .=) <$> _reOperationId])

-- | Request message for the ReleaseQuota method.
--
-- /See:/ 'releaseQuotaRequest' smart constructor.
data ReleaseQuotaRequest = ReleaseQuotaRequest'
    { _rqrReleaseOperation :: !(Maybe QuotaOperation)
    , _rqrServiceConfigId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReleaseQuotaRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rqrReleaseOperation'
--
-- * 'rqrServiceConfigId'
releaseQuotaRequest
    :: ReleaseQuotaRequest
releaseQuotaRequest = 
    ReleaseQuotaRequest'
    { _rqrReleaseOperation = Nothing
    , _rqrServiceConfigId = Nothing
    }

-- | Operation that describes the quota release.
rqrReleaseOperation :: Lens' ReleaseQuotaRequest (Maybe QuotaOperation)
rqrReleaseOperation
  = lens _rqrReleaseOperation
      (\ s a -> s{_rqrReleaseOperation = a})

-- | Specifies which version of service configuration should be used to
-- process the request. If unspecified or no matching version can be found,
-- the latest one will be used.
rqrServiceConfigId :: Lens' ReleaseQuotaRequest (Maybe Text)
rqrServiceConfigId
  = lens _rqrServiceConfigId
      (\ s a -> s{_rqrServiceConfigId = a})

instance FromJSON ReleaseQuotaRequest where
        parseJSON
          = withObject "ReleaseQuotaRequest"
              (\ o ->
                 ReleaseQuotaRequest' <$>
                   (o .:? "releaseOperation") <*>
                     (o .:? "serviceConfigId"))

instance ToJSON ReleaseQuotaRequest where
        toJSON ReleaseQuotaRequest'{..}
          = object
              (catMaybes
                 [("releaseOperation" .=) <$> _rqrReleaseOperation,
                  ("serviceConfigId" .=) <$> _rqrServiceConfigId])

-- | Request message for QuotaController.EndReconciliation.
--
-- /See:/ 'endReconciliationRequest' smart constructor.
data EndReconciliationRequest = EndReconciliationRequest'
    { _errServiceConfigId :: !(Maybe Text)
    , _errReconciliationOperation :: !(Maybe QuotaOperation)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EndReconciliationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'errServiceConfigId'
--
-- * 'errReconciliationOperation'
endReconciliationRequest
    :: EndReconciliationRequest
endReconciliationRequest = 
    EndReconciliationRequest'
    { _errServiceConfigId = Nothing
    , _errReconciliationOperation = Nothing
    }

-- | Specifies which version of service configuration should be used to
-- process the request. If unspecified or no matching version can be found,
-- the latest one will be used.
errServiceConfigId :: Lens' EndReconciliationRequest (Maybe Text)
errServiceConfigId
  = lens _errServiceConfigId
      (\ s a -> s{_errServiceConfigId = a})

-- | Operation that describes the quota reconciliation.
errReconciliationOperation :: Lens' EndReconciliationRequest (Maybe QuotaOperation)
errReconciliationOperation
  = lens _errReconciliationOperation
      (\ s a -> s{_errReconciliationOperation = a})

instance FromJSON EndReconciliationRequest where
        parseJSON
          = withObject "EndReconciliationRequest"
              (\ o ->
                 EndReconciliationRequest' <$>
                   (o .:? "serviceConfigId") <*>
                     (o .:? "reconciliationOperation"))

instance ToJSON EndReconciliationRequest where
        toJSON EndReconciliationRequest'{..}
          = object
              (catMaybes
                 [("serviceConfigId" .=) <$> _errServiceConfigId,
                  ("reconciliationOperation" .=) <$>
                    _errReconciliationOperation])

-- | The third party identification (if any) of the authenticated user making
-- the request. When the JSON object represented here has a proto
-- equivalent, the proto name will be indicated in the \`\'type\` property.
--
-- /See:/ 'authenticationInfoThirdPartyPrincipal' smart constructor.
newtype AuthenticationInfoThirdPartyPrincipal = AuthenticationInfoThirdPartyPrincipal'
    { _aitppAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuthenticationInfoThirdPartyPrincipal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aitppAddtional'
authenticationInfoThirdPartyPrincipal
    :: HashMap Text JSONValue -- ^ 'aitppAddtional'
    -> AuthenticationInfoThirdPartyPrincipal
authenticationInfoThirdPartyPrincipal pAitppAddtional_ = 
    AuthenticationInfoThirdPartyPrincipal'
    { _aitppAddtional = _Coerce # pAitppAddtional_
    }

-- | Properties of the object.
aitppAddtional :: Lens' AuthenticationInfoThirdPartyPrincipal (HashMap Text JSONValue)
aitppAddtional
  = lens _aitppAddtional
      (\ s a -> s{_aitppAddtional = a})
      . _Coerce

instance FromJSON
         AuthenticationInfoThirdPartyPrincipal where
        parseJSON
          = withObject "AuthenticationInfoThirdPartyPrincipal"
              (\ o ->
                 AuthenticationInfoThirdPartyPrincipal' <$>
                   (parseJSONObject o))

instance ToJSON AuthenticationInfoThirdPartyPrincipal
         where
        toJSON = toJSON . _aitppAddtional

-- | Response message for the ReleaseQuota method.
--
-- /See:/ 'releaseQuotaResponse' smart constructor.
data ReleaseQuotaResponse = ReleaseQuotaResponse'
    { _rQuotaMetrics :: !(Maybe [MetricValueSet])
    , _rServiceConfigId :: !(Maybe Text)
    , _rReleaseErrors :: !(Maybe [QuotaError])
    , _rOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReleaseQuotaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rQuotaMetrics'
--
-- * 'rServiceConfigId'
--
-- * 'rReleaseErrors'
--
-- * 'rOperationId'
releaseQuotaResponse
    :: ReleaseQuotaResponse
releaseQuotaResponse = 
    ReleaseQuotaResponse'
    { _rQuotaMetrics = Nothing
    , _rServiceConfigId = Nothing
    , _rReleaseErrors = Nothing
    , _rOperationId = Nothing
    }

-- | Quota metrics to indicate the result of release. Depending on the
-- request, one or more of the following metrics will be included: 1. For
-- rate quota, per quota group or per quota metric released amount will be
-- specified using the following delta metric:
-- \"serviceruntime.googleapis.com\/api\/consumer\/quota_refund_count\" 2.
-- For allocation quota, per quota metric total usage will be specified
-- using the following gauge metric:
-- \"serviceruntime.googleapis.com\/allocation\/consumer\/quota_used_count\"
-- 3. For allocation quota, value for each quota limit associated with the
-- metrics will be specified using the following gauge metric:
-- \"serviceruntime.googleapis.com\/quota\/limit\"
rQuotaMetrics :: Lens' ReleaseQuotaResponse [MetricValueSet]
rQuotaMetrics
  = lens _rQuotaMetrics
      (\ s a -> s{_rQuotaMetrics = a})
      . _Default
      . _Coerce

-- | ID of the actual config used to process the request.
rServiceConfigId :: Lens' ReleaseQuotaResponse (Maybe Text)
rServiceConfigId
  = lens _rServiceConfigId
      (\ s a -> s{_rServiceConfigId = a})

-- | Indicates the decision of the release.
rReleaseErrors :: Lens' ReleaseQuotaResponse [QuotaError]
rReleaseErrors
  = lens _rReleaseErrors
      (\ s a -> s{_rReleaseErrors = a})
      . _Default
      . _Coerce

-- | The same operation_id value used in the ReleaseQuotaRequest. Used for
-- logging and diagnostics purposes.
rOperationId :: Lens' ReleaseQuotaResponse (Maybe Text)
rOperationId
  = lens _rOperationId (\ s a -> s{_rOperationId = a})

instance FromJSON ReleaseQuotaResponse where
        parseJSON
          = withObject "ReleaseQuotaResponse"
              (\ o ->
                 ReleaseQuotaResponse' <$>
                   (o .:? "quotaMetrics" .!= mempty) <*>
                     (o .:? "serviceConfigId")
                     <*> (o .:? "releaseErrors" .!= mempty)
                     <*> (o .:? "operationId"))

instance ToJSON ReleaseQuotaResponse where
        toJSON ReleaseQuotaResponse'{..}
          = object
              (catMaybes
                 [("quotaMetrics" .=) <$> _rQuotaMetrics,
                  ("serviceConfigId" .=) <$> _rServiceConfigId,
                  ("releaseErrors" .=) <$> _rReleaseErrors,
                  ("operationId" .=) <$> _rOperationId])

-- | Describes a resource associated with this operation.
--
-- /See:/ 'resourceInfo' smart constructor.
data ResourceInfo = ResourceInfo'
    { _riResourceName :: !(Maybe Text)
    , _riResourceContainer :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riResourceName'
--
-- * 'riResourceContainer'
resourceInfo
    :: ResourceInfo
resourceInfo = 
    ResourceInfo'
    { _riResourceName = Nothing
    , _riResourceContainer = Nothing
    }

-- | Name of the resource. This is used for auditing purposes.
riResourceName :: Lens' ResourceInfo (Maybe Text)
riResourceName
  = lens _riResourceName
      (\ s a -> s{_riResourceName = a})

-- | The identifier of the parent of this resource instance. Must be in one
-- of the following formats: - “projects\/” - “folders\/” -
-- “organizations\/”
riResourceContainer :: Lens' ResourceInfo (Maybe Text)
riResourceContainer
  = lens _riResourceContainer
      (\ s a -> s{_riResourceContainer = a})

instance FromJSON ResourceInfo where
        parseJSON
          = withObject "ResourceInfo"
              (\ o ->
                 ResourceInfo' <$>
                   (o .:? "resourceName") <*>
                     (o .:? "resourceContainer"))

instance ToJSON ResourceInfo where
        toJSON ResourceInfo'{..}
          = object
              (catMaybes
                 [("resourceName" .=) <$> _riResourceName,
                  ("resourceContainer" .=) <$> _riResourceContainer])

-- | Represents a single metric value.
--
-- /See:/ 'metricValue' smart constructor.
data MetricValue = MetricValue'
    { _mvBoolValue :: !(Maybe Bool)
    , _mvStartTime :: !(Maybe DateTime')
    , _mvMoneyValue :: !(Maybe Money)
    , _mvDoubleValue :: !(Maybe (Textual Double))
    , _mvStringValue :: !(Maybe Text)
    , _mvDistributionValue :: !(Maybe Distribution)
    , _mvEndTime :: !(Maybe DateTime')
    , _mvInt64Value :: !(Maybe (Textual Int64))
    , _mvLabels :: !(Maybe MetricValueLabels)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvBoolValue'
--
-- * 'mvStartTime'
--
-- * 'mvMoneyValue'
--
-- * 'mvDoubleValue'
--
-- * 'mvStringValue'
--
-- * 'mvDistributionValue'
--
-- * 'mvEndTime'
--
-- * 'mvInt64Value'
--
-- * 'mvLabels'
metricValue
    :: MetricValue
metricValue = 
    MetricValue'
    { _mvBoolValue = Nothing
    , _mvStartTime = Nothing
    , _mvMoneyValue = Nothing
    , _mvDoubleValue = Nothing
    , _mvStringValue = Nothing
    , _mvDistributionValue = Nothing
    , _mvEndTime = Nothing
    , _mvInt64Value = Nothing
    , _mvLabels = Nothing
    }

-- | A boolean value.
mvBoolValue :: Lens' MetricValue (Maybe Bool)
mvBoolValue
  = lens _mvBoolValue (\ s a -> s{_mvBoolValue = a})

-- | The start of the time period over which this metric value\'s measurement
-- applies. The time period has different semantics for different metric
-- types (cumulative, delta, and gauge). See the metric definition
-- documentation in the service configuration for details.
mvStartTime :: Lens' MetricValue (Maybe UTCTime)
mvStartTime
  = lens _mvStartTime (\ s a -> s{_mvStartTime = a}) .
      mapping _DateTime

-- | A money value.
mvMoneyValue :: Lens' MetricValue (Maybe Money)
mvMoneyValue
  = lens _mvMoneyValue (\ s a -> s{_mvMoneyValue = a})

-- | A double precision floating point value.
mvDoubleValue :: Lens' MetricValue (Maybe Double)
mvDoubleValue
  = lens _mvDoubleValue
      (\ s a -> s{_mvDoubleValue = a})
      . mapping _Coerce

-- | A text string value.
mvStringValue :: Lens' MetricValue (Maybe Text)
mvStringValue
  = lens _mvStringValue
      (\ s a -> s{_mvStringValue = a})

-- | A distribution value.
mvDistributionValue :: Lens' MetricValue (Maybe Distribution)
mvDistributionValue
  = lens _mvDistributionValue
      (\ s a -> s{_mvDistributionValue = a})

-- | The end of the time period over which this metric value\'s measurement
-- applies.
mvEndTime :: Lens' MetricValue (Maybe UTCTime)
mvEndTime
  = lens _mvEndTime (\ s a -> s{_mvEndTime = a}) .
      mapping _DateTime

-- | A signed 64-bit integer value.
mvInt64Value :: Lens' MetricValue (Maybe Int64)
mvInt64Value
  = lens _mvInt64Value (\ s a -> s{_mvInt64Value = a})
      . mapping _Coerce

-- | The labels describing the metric value. See comments on
-- google.api.servicecontrol.v1.Operation.labels for the overriding
-- relationship.
mvLabels :: Lens' MetricValue (Maybe MetricValueLabels)
mvLabels = lens _mvLabels (\ s a -> s{_mvLabels = a})

instance FromJSON MetricValue where
        parseJSON
          = withObject "MetricValue"
              (\ o ->
                 MetricValue' <$>
                   (o .:? "boolValue") <*> (o .:? "startTime") <*>
                     (o .:? "moneyValue")
                     <*> (o .:? "doubleValue")
                     <*> (o .:? "stringValue")
                     <*> (o .:? "distributionValue")
                     <*> (o .:? "endTime")
                     <*> (o .:? "int64Value")
                     <*> (o .:? "labels"))

instance ToJSON MetricValue where
        toJSON MetricValue'{..}
          = object
              (catMaybes
                 [("boolValue" .=) <$> _mvBoolValue,
                  ("startTime" .=) <$> _mvStartTime,
                  ("moneyValue" .=) <$> _mvMoneyValue,
                  ("doubleValue" .=) <$> _mvDoubleValue,
                  ("stringValue" .=) <$> _mvStringValue,
                  ("distributionValue" .=) <$> _mvDistributionValue,
                  ("endTime" .=) <$> _mvEndTime,
                  ("int64Value" .=) <$> _mvInt64Value,
                  ("labels" .=) <$> _mvLabels])

-- | Represents the properties needed for quota operations.
--
-- /See:/ 'quotaProperties' smart constructor.
newtype QuotaProperties = QuotaProperties'
    { _qpQuotaMode :: Maybe QuotaPropertiesQuotaMode
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'QuotaProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qpQuotaMode'
quotaProperties
    :: QuotaProperties
quotaProperties = 
    QuotaProperties'
    { _qpQuotaMode = Nothing
    }

-- | Quota mode for this operation.
qpQuotaMode :: Lens' QuotaProperties (Maybe QuotaPropertiesQuotaMode)
qpQuotaMode
  = lens _qpQuotaMode (\ s a -> s{_qpQuotaMode = a})

instance FromJSON QuotaProperties where
        parseJSON
          = withObject "QuotaProperties"
              (\ o -> QuotaProperties' <$> (o .:? "quotaMode"))

instance ToJSON QuotaProperties where
        toJSON QuotaProperties'{..}
          = object
              (catMaybes [("quotaMode" .=) <$> _qpQuotaMode])

-- | Request message for the AllocateQuota method.
--
-- /See:/ 'allocateQuotaRequest' smart constructor.
data AllocateQuotaRequest = AllocateQuotaRequest'
    { _aqrServiceConfigId :: !(Maybe Text)
    , _aqrAllocateOperation :: !(Maybe QuotaOperation)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllocateQuotaRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aqrServiceConfigId'
--
-- * 'aqrAllocateOperation'
allocateQuotaRequest
    :: AllocateQuotaRequest
allocateQuotaRequest = 
    AllocateQuotaRequest'
    { _aqrServiceConfigId = Nothing
    , _aqrAllocateOperation = Nothing
    }

-- | Specifies which version of service configuration should be used to
-- process the request. If unspecified or no matching version can be found,
-- the latest one will be used.
aqrServiceConfigId :: Lens' AllocateQuotaRequest (Maybe Text)
aqrServiceConfigId
  = lens _aqrServiceConfigId
      (\ s a -> s{_aqrServiceConfigId = a})

-- | Operation that describes the quota allocation.
aqrAllocateOperation :: Lens' AllocateQuotaRequest (Maybe QuotaOperation)
aqrAllocateOperation
  = lens _aqrAllocateOperation
      (\ s a -> s{_aqrAllocateOperation = a})

instance FromJSON AllocateQuotaRequest where
        parseJSON
          = withObject "AllocateQuotaRequest"
              (\ o ->
                 AllocateQuotaRequest' <$>
                   (o .:? "serviceConfigId") <*>
                     (o .:? "allocateOperation"))

instance ToJSON AllocateQuotaRequest where
        toJSON AllocateQuotaRequest'{..}
          = object
              (catMaybes
                 [("serviceConfigId" .=) <$> _aqrServiceConfigId,
                  ("allocateOperation" .=) <$> _aqrAllocateOperation])

-- | The operation response. This may not include all response elements, such
-- as those that are too large, privacy-sensitive, or duplicated elsewhere
-- in the log record. It should never include user-generated data, such as
-- file contents. When the JSON object represented here has a proto
-- equivalent, the proto name will be indicated in the \`\'type\` property.
--
-- /See:/ 'auditLogResponse' smart constructor.
newtype AuditLogResponse = AuditLogResponse'
    { _alrAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuditLogResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alrAddtional'
auditLogResponse
    :: HashMap Text JSONValue -- ^ 'alrAddtional'
    -> AuditLogResponse
auditLogResponse pAlrAddtional_ = 
    AuditLogResponse'
    { _alrAddtional = _Coerce # pAlrAddtional_
    }

-- | Properties of the object.
alrAddtional :: Lens' AuditLogResponse (HashMap Text JSONValue)
alrAddtional
  = lens _alrAddtional (\ s a -> s{_alrAddtional = a})
      . _Coerce

instance FromJSON AuditLogResponse where
        parseJSON
          = withObject "AuditLogResponse"
              (\ o -> AuditLogResponse' <$> (parseJSONObject o))

instance ToJSON AuditLogResponse where
        toJSON = toJSON . _alrAddtional

-- | Response message for the Check method.
--
-- /See:/ 'checkResponse' smart constructor.
data CheckResponse = CheckResponse'
    { _cCheckErrors :: !(Maybe [CheckError])
    , _cQuotaInfo :: !(Maybe QuotaInfo)
    , _cServiceConfigId :: !(Maybe Text)
    , _cCheckInfo :: !(Maybe CheckInfo)
    , _cOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCheckErrors'
--
-- * 'cQuotaInfo'
--
-- * 'cServiceConfigId'
--
-- * 'cCheckInfo'
--
-- * 'cOperationId'
checkResponse
    :: CheckResponse
checkResponse = 
    CheckResponse'
    { _cCheckErrors = Nothing
    , _cQuotaInfo = Nothing
    , _cServiceConfigId = Nothing
    , _cCheckInfo = Nothing
    , _cOperationId = Nothing
    }

-- | Indicate the decision of the check. If no check errors are present, the
-- service should process the operation. Otherwise the service should use
-- the list of errors to determine the appropriate action.
cCheckErrors :: Lens' CheckResponse [CheckError]
cCheckErrors
  = lens _cCheckErrors (\ s a -> s{_cCheckErrors = a})
      . _Default
      . _Coerce

-- | Quota information for the check request associated with this response.
cQuotaInfo :: Lens' CheckResponse (Maybe QuotaInfo)
cQuotaInfo
  = lens _cQuotaInfo (\ s a -> s{_cQuotaInfo = a})

-- | The actual config id used to process the request.
cServiceConfigId :: Lens' CheckResponse (Maybe Text)
cServiceConfigId
  = lens _cServiceConfigId
      (\ s a -> s{_cServiceConfigId = a})

-- | Feedback data returned from the server during processing a Check
-- request.
cCheckInfo :: Lens' CheckResponse (Maybe CheckInfo)
cCheckInfo
  = lens _cCheckInfo (\ s a -> s{_cCheckInfo = a})

-- | The same operation_id value used in the CheckRequest. Used for logging
-- and diagnostics purposes.
cOperationId :: Lens' CheckResponse (Maybe Text)
cOperationId
  = lens _cOperationId (\ s a -> s{_cOperationId = a})

instance FromJSON CheckResponse where
        parseJSON
          = withObject "CheckResponse"
              (\ o ->
                 CheckResponse' <$>
                   (o .:? "checkErrors" .!= mempty) <*>
                     (o .:? "quotaInfo")
                     <*> (o .:? "serviceConfigId")
                     <*> (o .:? "checkInfo")
                     <*> (o .:? "operationId"))

instance ToJSON CheckResponse where
        toJSON CheckResponse'{..}
          = object
              (catMaybes
                 [("checkErrors" .=) <$> _cCheckErrors,
                  ("quotaInfo" .=) <$> _cQuotaInfo,
                  ("serviceConfigId" .=) <$> _cServiceConfigId,
                  ("checkInfo" .=) <$> _cCheckInfo,
                  ("operationId" .=) <$> _cOperationId])

-- | Response message for the Report method.
--
-- /See:/ 'reportResponse' smart constructor.
data ReportResponse = ReportResponse'
    { _repReportErrors :: !(Maybe [ReportError])
    , _repReportInfos :: !(Maybe [ReportInfo])
    , _repServiceConfigId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'repReportErrors'
--
-- * 'repReportInfos'
--
-- * 'repServiceConfigId'
reportResponse
    :: ReportResponse
reportResponse = 
    ReportResponse'
    { _repReportErrors = Nothing
    , _repReportInfos = Nothing
    , _repServiceConfigId = Nothing
    }

-- | Partial failures, one for each \`Operation\` in the request that failed
-- processing. There are three possible combinations of the RPC status: 1.
-- The combination of a successful RPC status and an empty
-- \`report_errors\` list indicates a complete success where all
-- \`Operations\` in the request are processed successfully. 2. The
-- combination of a successful RPC status and a non-empty \`report_errors\`
-- list indicates a partial success where some \`Operations\` in the
-- request succeeded. Each \`Operation\` that failed processing has a
-- corresponding item in this list. 3. A failed RPC status indicates a
-- general non-deterministic failure. When this happens, it\'s impossible
-- to know which of the \'Operations\' in the request succeeded or failed.
repReportErrors :: Lens' ReportResponse [ReportError]
repReportErrors
  = lens _repReportErrors
      (\ s a -> s{_repReportErrors = a})
      . _Default
      . _Coerce

-- | Quota usage for each quota release \`Operation\` request. Fully or
-- partially failed quota release request may or may not be present in
-- \`report_quota_info\`. For example, a failed quota release request will
-- have the current quota usage info when precise quota library returns the
-- info. A deadline exceeded quota request will not have quota usage info.
-- If there is no quota release request, report_quota_info will be empty.
repReportInfos :: Lens' ReportResponse [ReportInfo]
repReportInfos
  = lens _repReportInfos
      (\ s a -> s{_repReportInfos = a})
      . _Default
      . _Coerce

-- | The actual config id used to process the request.
repServiceConfigId :: Lens' ReportResponse (Maybe Text)
repServiceConfigId
  = lens _repServiceConfigId
      (\ s a -> s{_repServiceConfigId = a})

instance FromJSON ReportResponse where
        parseJSON
          = withObject "ReportResponse"
              (\ o ->
                 ReportResponse' <$>
                   (o .:? "reportErrors" .!= mempty) <*>
                     (o .:? "reportInfos" .!= mempty)
                     <*> (o .:? "serviceConfigId"))

instance ToJSON ReportResponse where
        toJSON ReportResponse'{..}
          = object
              (catMaybes
                 [("reportErrors" .=) <$> _repReportErrors,
                  ("reportInfos" .=) <$> _repReportInfos,
                  ("serviceConfigId" .=) <$> _repServiceConfigId])

-- | Contains additional info about the report operation.
--
-- /See:/ 'reportInfo' smart constructor.
data ReportInfo = ReportInfo'
    { _riQuotaInfo :: !(Maybe QuotaInfo)
    , _riOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReportInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riQuotaInfo'
--
-- * 'riOperationId'
reportInfo
    :: ReportInfo
reportInfo = 
    ReportInfo'
    { _riQuotaInfo = Nothing
    , _riOperationId = Nothing
    }

-- | Quota usage info when processing the \`Operation\`.
riQuotaInfo :: Lens' ReportInfo (Maybe QuotaInfo)
riQuotaInfo
  = lens _riQuotaInfo (\ s a -> s{_riQuotaInfo = a})

-- | The Operation.operation_id value from the request.
riOperationId :: Lens' ReportInfo (Maybe Text)
riOperationId
  = lens _riOperationId
      (\ s a -> s{_riOperationId = a})

instance FromJSON ReportInfo where
        parseJSON
          = withObject "ReportInfo"
              (\ o ->
                 ReportInfo' <$>
                   (o .:? "quotaInfo") <*> (o .:? "operationId"))

instance ToJSON ReportInfo where
        toJSON ReportInfo'{..}
          = object
              (catMaybes
                 [("quotaInfo" .=) <$> _riQuotaInfo,
                  ("operationId" .=) <$> _riOperationId])

-- | An individual log entry.
--
-- /See:/ 'logEntry' smart constructor.
data LogEntry = LogEntry'
    { _leSeverity :: !(Maybe LogEntrySeverity)
    , _leTextPayload :: !(Maybe Text)
    , _leStructPayload :: !(Maybe LogEntryStructPayload)
    , _leName :: !(Maybe Text)
    , _leInsertId :: !(Maybe Text)
    , _leLabels :: !(Maybe LogEntryLabels)
    , _leProtoPayload :: !(Maybe LogEntryProtoPayload)
    , _leTimestamp :: !(Maybe DateTime')
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LogEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leSeverity'
--
-- * 'leTextPayload'
--
-- * 'leStructPayload'
--
-- * 'leName'
--
-- * 'leInsertId'
--
-- * 'leLabels'
--
-- * 'leProtoPayload'
--
-- * 'leTimestamp'
logEntry
    :: LogEntry
logEntry = 
    LogEntry'
    { _leSeverity = Nothing
    , _leTextPayload = Nothing
    , _leStructPayload = Nothing
    , _leName = Nothing
    , _leInsertId = Nothing
    , _leLabels = Nothing
    , _leProtoPayload = Nothing
    , _leTimestamp = Nothing
    }

-- | The severity of the log entry. The default value is
-- \`LogSeverity.DEFAULT\`.
leSeverity :: Lens' LogEntry (Maybe LogEntrySeverity)
leSeverity
  = lens _leSeverity (\ s a -> s{_leSeverity = a})

-- | The log entry payload, represented as a Unicode string (UTF-8).
leTextPayload :: Lens' LogEntry (Maybe Text)
leTextPayload
  = lens _leTextPayload
      (\ s a -> s{_leTextPayload = a})

-- | The log entry payload, represented as a structure that is expressed as a
-- JSON object.
leStructPayload :: Lens' LogEntry (Maybe LogEntryStructPayload)
leStructPayload
  = lens _leStructPayload
      (\ s a -> s{_leStructPayload = a})

-- | Required. The log to which this log entry belongs. Examples:
-- \`\"syslog\"\`, \`\"book_log\"\`.
leName :: Lens' LogEntry (Maybe Text)
leName = lens _leName (\ s a -> s{_leName = a})

-- | A unique ID for the log entry used for deduplication. If omitted, the
-- implementation will generate one based on operation_id.
leInsertId :: Lens' LogEntry (Maybe Text)
leInsertId
  = lens _leInsertId (\ s a -> s{_leInsertId = a})

-- | A set of user-defined (key, value) data that provides additional
-- information about the log entry.
leLabels :: Lens' LogEntry (Maybe LogEntryLabels)
leLabels = lens _leLabels (\ s a -> s{_leLabels = a})

-- | The log entry payload, represented as a protocol buffer that is
-- expressed as a JSON object. The only accepted type currently is
-- AuditLog.
leProtoPayload :: Lens' LogEntry (Maybe LogEntryProtoPayload)
leProtoPayload
  = lens _leProtoPayload
      (\ s a -> s{_leProtoPayload = a})

-- | The time the event described by the log entry occurred. If omitted,
-- defaults to operation start time.
leTimestamp :: Lens' LogEntry (Maybe UTCTime)
leTimestamp
  = lens _leTimestamp (\ s a -> s{_leTimestamp = a}) .
      mapping _DateTime

instance FromJSON LogEntry where
        parseJSON
          = withObject "LogEntry"
              (\ o ->
                 LogEntry' <$>
                   (o .:? "severity") <*> (o .:? "textPayload") <*>
                     (o .:? "structPayload")
                     <*> (o .:? "name")
                     <*> (o .:? "insertId")
                     <*> (o .:? "labels")
                     <*> (o .:? "protoPayload")
                     <*> (o .:? "timestamp"))

instance ToJSON LogEntry where
        toJSON LogEntry'{..}
          = object
              (catMaybes
                 [("severity" .=) <$> _leSeverity,
                  ("textPayload" .=) <$> _leTextPayload,
                  ("structPayload" .=) <$> _leStructPayload,
                  ("name" .=) <$> _leName,
                  ("insertId" .=) <$> _leInsertId,
                  ("labels" .=) <$> _leLabels,
                  ("protoPayload" .=) <$> _leProtoPayload,
                  ("timestamp" .=) <$> _leTimestamp])

-- | The operation request. This may not include all request parameters, such
-- as those that are too large, privacy-sensitive, or duplicated elsewhere
-- in the log record. It should never include user-generated data, such as
-- file contents. When the JSON object represented here has a proto
-- equivalent, the proto name will be indicated in the \`\'type\` property.
--
-- /See:/ 'auditLogRequest' smart constructor.
newtype AuditLogRequest = AuditLogRequest'
    { _aAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuditLogRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAddtional'
auditLogRequest
    :: HashMap Text JSONValue -- ^ 'aAddtional'
    -> AuditLogRequest
auditLogRequest pAAddtional_ = 
    AuditLogRequest'
    { _aAddtional = _Coerce # pAAddtional_
    }

-- | Properties of the object.
aAddtional :: Lens' AuditLogRequest (HashMap Text JSONValue)
aAddtional
  = lens _aAddtional (\ s a -> s{_aAddtional = a}) .
      _Coerce

instance FromJSON AuditLogRequest where
        parseJSON
          = withObject "AuditLogRequest"
              (\ o -> AuditLogRequest' <$> (parseJSONObject o))

instance ToJSON AuditLogRequest where
        toJSON = toJSON . _aAddtional

-- | Labels describing the operation. Only the following labels are allowed:
-- - Labels describing monitored resources as defined in the service
-- configuration. - Default labels of metric values. When specified, labels
-- defined in the metric value override these default. - The following
-- labels defined by Google Cloud Platform: -
-- \`cloud.googleapis.com\/location\` describing the location where the
-- operation happened, - \`servicecontrol.googleapis.com\/user_agent\`
-- describing the user agent of the API request, -
-- \`servicecontrol.googleapis.com\/service_agent\` describing the service
-- used to handle the API request (e.g. ESP), -
-- \`servicecontrol.googleapis.com\/platform\` describing the platform
-- where the API is served (e.g. GAE, GCE, GKE).
--
-- /See:/ 'operationLabels' smart constructor.
newtype OperationLabels = OperationLabels'
    { _olAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olAddtional'
operationLabels
    :: HashMap Text Text -- ^ 'olAddtional'
    -> OperationLabels
operationLabels pOlAddtional_ = 
    OperationLabels'
    { _olAddtional = _Coerce # pOlAddtional_
    }

olAddtional :: Lens' OperationLabels (HashMap Text Text)
olAddtional
  = lens _olAddtional (\ s a -> s{_olAddtional = a}) .
      _Coerce

instance FromJSON OperationLabels where
        parseJSON
          = withObject "OperationLabels"
              (\ o -> OperationLabels' <$> (parseJSONObject o))

instance ToJSON OperationLabels where
        toJSON = toJSON . _olAddtional

-- | Contains additional information about the check operation.
--
-- /See:/ 'checkInfo' smart constructor.
data CheckInfo = CheckInfo'
    { _ciConsumerInfo :: !(Maybe ConsumerInfo)
    , _ciUnusedArguments :: !(Maybe [Text])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CheckInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciConsumerInfo'
--
-- * 'ciUnusedArguments'
checkInfo
    :: CheckInfo
checkInfo = 
    CheckInfo'
    { _ciConsumerInfo = Nothing
    , _ciUnusedArguments = Nothing
    }

-- | Consumer info of this check.
ciConsumerInfo :: Lens' CheckInfo (Maybe ConsumerInfo)
ciConsumerInfo
  = lens _ciConsumerInfo
      (\ s a -> s{_ciConsumerInfo = a})

-- | A list of fields and label keys that are ignored by the server. The
-- client doesn\'t need to send them for following requests to improve
-- performance and allow better aggregation.
ciUnusedArguments :: Lens' CheckInfo [Text]
ciUnusedArguments
  = lens _ciUnusedArguments
      (\ s a -> s{_ciUnusedArguments = a})
      . _Default
      . _Coerce

instance FromJSON CheckInfo where
        parseJSON
          = withObject "CheckInfo"
              (\ o ->
                 CheckInfo' <$>
                   (o .:? "consumerInfo") <*>
                     (o .:? "unusedArguments" .!= mempty))

instance ToJSON CheckInfo where
        toJSON CheckInfo'{..}
          = object
              (catMaybes
                 [("consumerInfo" .=) <$> _ciConsumerInfo,
                  ("unusedArguments" .=) <$> _ciUnusedArguments])

-- | Response message for the AllocateQuota method.
--
-- /See:/ 'allocateQuotaResponse' smart constructor.
data AllocateQuotaResponse = AllocateQuotaResponse'
    { _aAllocateInfo :: !(Maybe AllocateInfo)
    , _aAllocateErrors :: !(Maybe [QuotaError])
    , _aQuotaMetrics :: !(Maybe [MetricValueSet])
    , _aServiceConfigId :: !(Maybe Text)
    , _aOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllocateQuotaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAllocateInfo'
--
-- * 'aAllocateErrors'
--
-- * 'aQuotaMetrics'
--
-- * 'aServiceConfigId'
--
-- * 'aOperationId'
allocateQuotaResponse
    :: AllocateQuotaResponse
allocateQuotaResponse = 
    AllocateQuotaResponse'
    { _aAllocateInfo = Nothing
    , _aAllocateErrors = Nothing
    , _aQuotaMetrics = Nothing
    , _aServiceConfigId = Nothing
    , _aOperationId = Nothing
    }

-- | WARNING: DO NOT use this field until this warning message is removed.
aAllocateInfo :: Lens' AllocateQuotaResponse (Maybe AllocateInfo)
aAllocateInfo
  = lens _aAllocateInfo
      (\ s a -> s{_aAllocateInfo = a})

-- | Indicates the decision of the allocate.
aAllocateErrors :: Lens' AllocateQuotaResponse [QuotaError]
aAllocateErrors
  = lens _aAllocateErrors
      (\ s a -> s{_aAllocateErrors = a})
      . _Default
      . _Coerce

-- | Quota metrics to indicate the result of allocation. Depending on the
-- request, one or more of the following metrics will be included: 1. Per
-- quota group or per quota metric incremental usage will be specified
-- using the following delta metric :
-- \"serviceruntime.googleapis.com\/api\/consumer\/quota_used_count\" 2.
-- The quota limit reached condition will be specified using the following
-- boolean metric : \"serviceruntime.googleapis.com\/quota\/exceeded\"
aQuotaMetrics :: Lens' AllocateQuotaResponse [MetricValueSet]
aQuotaMetrics
  = lens _aQuotaMetrics
      (\ s a -> s{_aQuotaMetrics = a})
      . _Default
      . _Coerce

-- | ID of the actual config used to process the request.
aServiceConfigId :: Lens' AllocateQuotaResponse (Maybe Text)
aServiceConfigId
  = lens _aServiceConfigId
      (\ s a -> s{_aServiceConfigId = a})

-- | The same operation_id value used in the AllocateQuotaRequest. Used for
-- logging and diagnostics purposes.
aOperationId :: Lens' AllocateQuotaResponse (Maybe Text)
aOperationId
  = lens _aOperationId (\ s a -> s{_aOperationId = a})

instance FromJSON AllocateQuotaResponse where
        parseJSON
          = withObject "AllocateQuotaResponse"
              (\ o ->
                 AllocateQuotaResponse' <$>
                   (o .:? "allocateInfo") <*>
                     (o .:? "allocateErrors" .!= mempty)
                     <*> (o .:? "quotaMetrics" .!= mempty)
                     <*> (o .:? "serviceConfigId")
                     <*> (o .:? "operationId"))

instance ToJSON AllocateQuotaResponse where
        toJSON AllocateQuotaResponse'{..}
          = object
              (catMaybes
                 [("allocateInfo" .=) <$> _aAllocateInfo,
                  ("allocateErrors" .=) <$> _aAllocateErrors,
                  ("quotaMetrics" .=) <$> _aQuotaMetrics,
                  ("serviceConfigId" .=) <$> _aServiceConfigId,
                  ("operationId" .=) <$> _aOperationId])

-- | Response message for QuotaController.EndReconciliation.
--
-- /See:/ 'endReconciliationResponse' smart constructor.
data EndReconciliationResponse = EndReconciliationResponse'
    { _eQuotaMetrics :: !(Maybe [MetricValueSet])
    , _eReconciliationErrors :: !(Maybe [QuotaError])
    , _eServiceConfigId :: !(Maybe Text)
    , _eOperationId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EndReconciliationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eQuotaMetrics'
--
-- * 'eReconciliationErrors'
--
-- * 'eServiceConfigId'
--
-- * 'eOperationId'
endReconciliationResponse
    :: EndReconciliationResponse
endReconciliationResponse = 
    EndReconciliationResponse'
    { _eQuotaMetrics = Nothing
    , _eReconciliationErrors = Nothing
    , _eServiceConfigId = Nothing
    , _eOperationId = Nothing
    }

-- | Metric values as tracked by One Platform before the adjustment was made.
-- The following metrics will be included: 1. Per quota metric total usage
-- will be specified using the following gauge metric:
-- \"serviceruntime.googleapis.com\/allocation\/consumer\/quota_used_count\"
-- 2. Value for each quota limit associated with the metrics will be
-- specified using the following gauge metric:
-- \"serviceruntime.googleapis.com\/quota\/limit\" 3. Delta value of the
-- usage after the reconciliation for limits associated with the metrics
-- will be specified using the following metric:
-- \"serviceruntime.googleapis.com\/allocation\/reconciliation_delta\" The
-- delta value is defined as: new_usage_from_client -
-- existing_value_in_spanner. This metric is not defined in
-- serviceruntime.yaml or in Cloud Monarch. This metric is meant for
-- callers\' use only. Since this metric is not defined in the monitoring
-- backend, reporting on this metric will result in an error.
eQuotaMetrics :: Lens' EndReconciliationResponse [MetricValueSet]
eQuotaMetrics
  = lens _eQuotaMetrics
      (\ s a -> s{_eQuotaMetrics = a})
      . _Default
      . _Coerce

-- | Indicates the decision of the reconciliation end.
eReconciliationErrors :: Lens' EndReconciliationResponse [QuotaError]
eReconciliationErrors
  = lens _eReconciliationErrors
      (\ s a -> s{_eReconciliationErrors = a})
      . _Default
      . _Coerce

-- | ID of the actual config used to process the request.
eServiceConfigId :: Lens' EndReconciliationResponse (Maybe Text)
eServiceConfigId
  = lens _eServiceConfigId
      (\ s a -> s{_eServiceConfigId = a})

-- | The same operation_id value used in the EndReconciliationRequest. Used
-- for logging and diagnostics purposes.
eOperationId :: Lens' EndReconciliationResponse (Maybe Text)
eOperationId
  = lens _eOperationId (\ s a -> s{_eOperationId = a})

instance FromJSON EndReconciliationResponse where
        parseJSON
          = withObject "EndReconciliationResponse"
              (\ o ->
                 EndReconciliationResponse' <$>
                   (o .:? "quotaMetrics" .!= mempty) <*>
                     (o .:? "reconciliationErrors" .!= mempty)
                     <*> (o .:? "serviceConfigId")
                     <*> (o .:? "operationId"))

instance ToJSON EndReconciliationResponse where
        toJSON EndReconciliationResponse'{..}
          = object
              (catMaybes
                 [("quotaMetrics" .=) <$> _eQuotaMetrics,
                  ("reconciliationErrors" .=) <$>
                    _eReconciliationErrors,
                  ("serviceConfigId" .=) <$> _eServiceConfigId,
                  ("operationId" .=) <$> _eOperationId])

-- | Describing buckets with arbitrary user-provided width.
--
-- /See:/ 'explicitBuckets' smart constructor.
newtype ExplicitBuckets = ExplicitBuckets'
    { _ebBounds :: Maybe [Textual Double]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExplicitBuckets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebBounds'
explicitBuckets
    :: ExplicitBuckets
explicitBuckets = 
    ExplicitBuckets'
    { _ebBounds = Nothing
    }

-- | \'bound\' is a list of strictly increasing boundaries between buckets.
-- Note that a list of length N-1 defines N buckets because of
-- fenceposting. See comments on \`bucket_options\` for details. The i\'th
-- finite bucket covers the interval [bound[i-1], bound[i]) where i ranges
-- from 1 to bound_size() - 1. Note that there are no finite buckets at all
-- if \'bound\' only contains a single element; in that special case the
-- single bound defines the boundary between the underflow and overflow
-- buckets. bucket number lower bound upper bound i == 0 (underflow) -inf
-- bound[i] 0 \< i \< bound_size() bound[i-1] bound[i] i == bound_size()
-- (overflow) bound[i-1] +inf
ebBounds :: Lens' ExplicitBuckets [Double]
ebBounds
  = lens _ebBounds (\ s a -> s{_ebBounds = a}) .
      _Default
      . _Coerce

instance FromJSON ExplicitBuckets where
        parseJSON
          = withObject "ExplicitBuckets"
              (\ o ->
                 ExplicitBuckets' <$> (o .:? "bounds" .!= mempty))

instance ToJSON ExplicitBuckets where
        toJSON ExplicitBuckets'{..}
          = object (catMaybes [("bounds" .=) <$> _ebBounds])

-- | Represents error information for QuotaOperation.
--
-- /See:/ 'quotaError' smart constructor.
data QuotaError = QuotaError'
    { _qeSubject :: !(Maybe Text)
    , _qeCode :: !(Maybe QuotaErrorCode)
    , _qeDescription :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'QuotaError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qeSubject'
--
-- * 'qeCode'
--
-- * 'qeDescription'
quotaError
    :: QuotaError
quotaError = 
    QuotaError'
    { _qeSubject = Nothing
    , _qeCode = Nothing
    , _qeDescription = Nothing
    }

-- | Subject to whom this error applies. See the specific enum for more
-- details on this field. For example, \"clientip:\" or \"project:\".
qeSubject :: Lens' QuotaError (Maybe Text)
qeSubject
  = lens _qeSubject (\ s a -> s{_qeSubject = a})

-- | Error code.
qeCode :: Lens' QuotaError (Maybe QuotaErrorCode)
qeCode = lens _qeCode (\ s a -> s{_qeCode = a})

-- | Free-form text that provides details on the cause of the error.
qeDescription :: Lens' QuotaError (Maybe Text)
qeDescription
  = lens _qeDescription
      (\ s a -> s{_qeDescription = a})

instance FromJSON QuotaError where
        parseJSON
          = withObject "QuotaError"
              (\ o ->
                 QuotaError' <$>
                   (o .:? "subject") <*> (o .:? "code") <*>
                     (o .:? "description"))

instance ToJSON QuotaError where
        toJSON QuotaError'{..}
          = object
              (catMaybes
                 [("subject" .=) <$> _qeSubject,
                  ("code" .=) <$> _qeCode,
                  ("description" .=) <$> _qeDescription])

-- | Request message for QuotaController.StartReconciliation.
--
-- /See:/ 'startReconciliationRequest' smart constructor.
data StartReconciliationRequest = StartReconciliationRequest'
    { _sServiceConfigId :: !(Maybe Text)
    , _sReconciliationOperation :: !(Maybe QuotaOperation)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartReconciliationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sServiceConfigId'
--
-- * 'sReconciliationOperation'
startReconciliationRequest
    :: StartReconciliationRequest
startReconciliationRequest = 
    StartReconciliationRequest'
    { _sServiceConfigId = Nothing
    , _sReconciliationOperation = Nothing
    }

-- | Specifies which version of service configuration should be used to
-- process the request. If unspecified or no matching version can be found,
-- the latest one will be used.
sServiceConfigId :: Lens' StartReconciliationRequest (Maybe Text)
sServiceConfigId
  = lens _sServiceConfigId
      (\ s a -> s{_sServiceConfigId = a})

-- | Operation that describes the quota reconciliation.
sReconciliationOperation :: Lens' StartReconciliationRequest (Maybe QuotaOperation)
sReconciliationOperation
  = lens _sReconciliationOperation
      (\ s a -> s{_sReconciliationOperation = a})

instance FromJSON StartReconciliationRequest where
        parseJSON
          = withObject "StartReconciliationRequest"
              (\ o ->
                 StartReconciliationRequest' <$>
                   (o .:? "serviceConfigId") <*>
                     (o .:? "reconciliationOperation"))

instance ToJSON StartReconciliationRequest where
        toJSON StartReconciliationRequest'{..}
          = object
              (catMaybes
                 [("serviceConfigId" .=) <$> _sServiceConfigId,
                  ("reconciliationOperation" .=) <$>
                    _sReconciliationOperation])

-- | Authentication information for the operation.
--
-- /See:/ 'authenticationInfo' smart constructor.
data AuthenticationInfo = AuthenticationInfo'
    { _aiThirdPartyPrincipal :: !(Maybe AuthenticationInfoThirdPartyPrincipal)
    , _aiPrincipalEmail :: !(Maybe Text)
    , _aiAuthoritySelector :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuthenticationInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiThirdPartyPrincipal'
--
-- * 'aiPrincipalEmail'
--
-- * 'aiAuthoritySelector'
authenticationInfo
    :: AuthenticationInfo
authenticationInfo = 
    AuthenticationInfo'
    { _aiThirdPartyPrincipal = Nothing
    , _aiPrincipalEmail = Nothing
    , _aiAuthoritySelector = Nothing
    }

-- | The third party identification (if any) of the authenticated user making
-- the request. When the JSON object represented here has a proto
-- equivalent, the proto name will be indicated in the \`\'type\` property.
aiThirdPartyPrincipal :: Lens' AuthenticationInfo (Maybe AuthenticationInfoThirdPartyPrincipal)
aiThirdPartyPrincipal
  = lens _aiThirdPartyPrincipal
      (\ s a -> s{_aiThirdPartyPrincipal = a})

-- | The email address of the authenticated user (or service account on
-- behalf of third party principal) making the request. For privacy
-- reasons, the principal email address is redacted for all read-only
-- operations that fail with a \"permission denied\" error.
aiPrincipalEmail :: Lens' AuthenticationInfo (Maybe Text)
aiPrincipalEmail
  = lens _aiPrincipalEmail
      (\ s a -> s{_aiPrincipalEmail = a})

-- | The authority selector specified by the requestor, if any. It is not
-- guaranteed that the principal was allowed to use this authority.
aiAuthoritySelector :: Lens' AuthenticationInfo (Maybe Text)
aiAuthoritySelector
  = lens _aiAuthoritySelector
      (\ s a -> s{_aiAuthoritySelector = a})

instance FromJSON AuthenticationInfo where
        parseJSON
          = withObject "AuthenticationInfo"
              (\ o ->
                 AuthenticationInfo' <$>
                   (o .:? "thirdPartyPrincipal") <*>
                     (o .:? "principalEmail")
                     <*> (o .:? "authoritySelector"))

instance ToJSON AuthenticationInfo where
        toJSON AuthenticationInfo'{..}
          = object
              (catMaybes
                 [("thirdPartyPrincipal" .=) <$>
                    _aiThirdPartyPrincipal,
                  ("principalEmail" .=) <$> _aiPrincipalEmail,
                  ("authoritySelector" .=) <$> _aiAuthoritySelector])

-- | Map of quota group name to the actual number of tokens consumed. If the
-- quota check was not successful, then this will not be populated due to
-- no quota consumption. We are not merging this field with
-- \'quota_metrics\' field because of the complexity of scaling in Chemist
-- client code base. For simplicity, we will keep this field for Castor
-- (that scales quota usage) and \'quota_metrics\' for SuperQuota (that
-- doesn\'t scale quota usage).
--
-- /See:/ 'quotaInfoQuotaConsumed' smart constructor.
newtype QuotaInfoQuotaConsumed = QuotaInfoQuotaConsumed'
    { _qiqcAddtional :: HashMap Text (Textual Int32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'QuotaInfoQuotaConsumed' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qiqcAddtional'
quotaInfoQuotaConsumed
    :: HashMap Text Int32 -- ^ 'qiqcAddtional'
    -> QuotaInfoQuotaConsumed
quotaInfoQuotaConsumed pQiqcAddtional_ = 
    QuotaInfoQuotaConsumed'
    { _qiqcAddtional = _Coerce # pQiqcAddtional_
    }

qiqcAddtional :: Lens' QuotaInfoQuotaConsumed (HashMap Text Int32)
qiqcAddtional
  = lens _qiqcAddtional
      (\ s a -> s{_qiqcAddtional = a})
      . _Coerce

instance FromJSON QuotaInfoQuotaConsumed where
        parseJSON
          = withObject "QuotaInfoQuotaConsumed"
              (\ o ->
                 QuotaInfoQuotaConsumed' <$> (parseJSONObject o))

instance ToJSON QuotaInfoQuotaConsumed where
        toJSON = toJSON . _qiqcAddtional

-- | Common audit log format for Google Cloud Platform API operations.
--
-- /See:/ 'auditLog' smart constructor.
data AuditLog = AuditLog'
    { _alRequestMetadata :: !(Maybe RequestMetadata)
    , _alStatus :: !(Maybe Status)
    , _alResourceName :: !(Maybe Text)
    , _alAuthorizationInfo :: !(Maybe [AuthorizationInfo])
    , _alServiceData :: !(Maybe AuditLogServiceData)
    , _alMethodName :: !(Maybe Text)
    , _alResponse :: !(Maybe AuditLogResponse)
    , _alServiceName :: !(Maybe Text)
    , _alMetadata :: !(Maybe AuditLogMetadata)
    , _alNumResponseItems :: !(Maybe (Textual Int64))
    , _alAuthenticationInfo :: !(Maybe AuthenticationInfo)
    , _alRequest :: !(Maybe AuditLogRequest)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AuditLog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alRequestMetadata'
--
-- * 'alStatus'
--
-- * 'alResourceName'
--
-- * 'alAuthorizationInfo'
--
-- * 'alServiceData'
--
-- * 'alMethodName'
--
-- * 'alResponse'
--
-- * 'alServiceName'
--
-- * 'alMetadata'
--
-- * 'alNumResponseItems'
--
-- * 'alAuthenticationInfo'
--
-- * 'alRequest'
auditLog
    :: AuditLog
auditLog = 
    AuditLog'
    { _alRequestMetadata = Nothing
    , _alStatus = Nothing
    , _alResourceName = Nothing
    , _alAuthorizationInfo = Nothing
    , _alServiceData = Nothing
    , _alMethodName = Nothing
    , _alResponse = Nothing
    , _alServiceName = Nothing
    , _alMetadata = Nothing
    , _alNumResponseItems = Nothing
    , _alAuthenticationInfo = Nothing
    , _alRequest = Nothing
    }

-- | Metadata about the operation.
alRequestMetadata :: Lens' AuditLog (Maybe RequestMetadata)
alRequestMetadata
  = lens _alRequestMetadata
      (\ s a -> s{_alRequestMetadata = a})

-- | The status of the overall operation.
alStatus :: Lens' AuditLog (Maybe Status)
alStatus = lens _alStatus (\ s a -> s{_alStatus = a})

-- | The resource or collection that is the target of the operation. The name
-- is a scheme-less URI, not including the API service name. For example:
-- \"shelves\/SHELF_ID\/books\" \"shelves\/SHELF_ID\/books\/BOOK_ID\"
alResourceName :: Lens' AuditLog (Maybe Text)
alResourceName
  = lens _alResourceName
      (\ s a -> s{_alResourceName = a})

-- | Authorization information. If there are multiple resources or
-- permissions involved, then there is one AuthorizationInfo element for
-- each {resource, permission} tuple.
alAuthorizationInfo :: Lens' AuditLog [AuthorizationInfo]
alAuthorizationInfo
  = lens _alAuthorizationInfo
      (\ s a -> s{_alAuthorizationInfo = a})
      . _Default
      . _Coerce

-- | Deprecated, use \`metadata\` field instead. Other service-specific data
-- about the request, response, and other activities.
alServiceData :: Lens' AuditLog (Maybe AuditLogServiceData)
alServiceData
  = lens _alServiceData
      (\ s a -> s{_alServiceData = a})

-- | The name of the service method or operation. For API calls, this should
-- be the name of the API method. For example,
-- \"google.datastore.v1.Datastore.RunQuery\"
-- \"google.logging.v1.LoggingService.DeleteLog\"
alMethodName :: Lens' AuditLog (Maybe Text)
alMethodName
  = lens _alMethodName (\ s a -> s{_alMethodName = a})

-- | The operation response. This may not include all response elements, such
-- as those that are too large, privacy-sensitive, or duplicated elsewhere
-- in the log record. It should never include user-generated data, such as
-- file contents. When the JSON object represented here has a proto
-- equivalent, the proto name will be indicated in the \`\'type\` property.
alResponse :: Lens' AuditLog (Maybe AuditLogResponse)
alResponse
  = lens _alResponse (\ s a -> s{_alResponse = a})

-- | The name of the API service performing the operation. For example,
-- \`\"datastore.googleapis.com\"\`.
alServiceName :: Lens' AuditLog (Maybe Text)
alServiceName
  = lens _alServiceName
      (\ s a -> s{_alServiceName = a})

-- | Other service-specific data about the request, response, and other
-- information associated with the current audited event.
alMetadata :: Lens' AuditLog (Maybe AuditLogMetadata)
alMetadata
  = lens _alMetadata (\ s a -> s{_alMetadata = a})

-- | The number of items returned from a List or Query API method, if
-- applicable.
alNumResponseItems :: Lens' AuditLog (Maybe Int64)
alNumResponseItems
  = lens _alNumResponseItems
      (\ s a -> s{_alNumResponseItems = a})
      . mapping _Coerce

-- | Authentication information.
alAuthenticationInfo :: Lens' AuditLog (Maybe AuthenticationInfo)
alAuthenticationInfo
  = lens _alAuthenticationInfo
      (\ s a -> s{_alAuthenticationInfo = a})

-- | The operation request. This may not include all request parameters, such
-- as those that are too large, privacy-sensitive, or duplicated elsewhere
-- in the log record. It should never include user-generated data, such as
-- file contents. When the JSON object represented here has a proto
-- equivalent, the proto name will be indicated in the \`\'type\` property.
alRequest :: Lens' AuditLog (Maybe AuditLogRequest)
alRequest
  = lens _alRequest (\ s a -> s{_alRequest = a})

instance FromJSON AuditLog where
        parseJSON
          = withObject "AuditLog"
              (\ o ->
                 AuditLog' <$>
                   (o .:? "requestMetadata") <*> (o .:? "status") <*>
                     (o .:? "resourceName")
                     <*> (o .:? "authorizationInfo" .!= mempty)
                     <*> (o .:? "serviceData")
                     <*> (o .:? "methodName")
                     <*> (o .:? "response")
                     <*> (o .:? "serviceName")
                     <*> (o .:? "metadata")
                     <*> (o .:? "numResponseItems")
                     <*> (o .:? "authenticationInfo")
                     <*> (o .:? "request"))

instance ToJSON AuditLog where
        toJSON AuditLog'{..}
          = object
              (catMaybes
                 [("requestMetadata" .=) <$> _alRequestMetadata,
                  ("status" .=) <$> _alStatus,
                  ("resourceName" .=) <$> _alResourceName,
                  ("authorizationInfo" .=) <$> _alAuthorizationInfo,
                  ("serviceData" .=) <$> _alServiceData,
                  ("methodName" .=) <$> _alMethodName,
                  ("response" .=) <$> _alResponse,
                  ("serviceName" .=) <$> _alServiceName,
                  ("metadata" .=) <$> _alMetadata,
                  ("numResponseItems" .=) <$> _alNumResponseItems,
                  ("authenticationInfo" .=) <$> _alAuthenticationInfo,
                  ("request" .=) <$> _alRequest])

-- | User defined labels for the resource that this operation is associated
-- with. Only a combination of 1000 user labels per consumer project are
-- allowed.
--
-- /See:/ 'operationUserLabels' smart constructor.
newtype OperationUserLabels = OperationUserLabels'
    { _oulAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationUserLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oulAddtional'
operationUserLabels
    :: HashMap Text Text -- ^ 'oulAddtional'
    -> OperationUserLabels
operationUserLabels pOulAddtional_ = 
    OperationUserLabels'
    { _oulAddtional = _Coerce # pOulAddtional_
    }

oulAddtional :: Lens' OperationUserLabels (HashMap Text Text)
oulAddtional
  = lens _oulAddtional (\ s a -> s{_oulAddtional = a})
      . _Coerce

instance FromJSON OperationUserLabels where
        parseJSON
          = withObject "OperationUserLabels"
              (\ o -> OperationUserLabels' <$> (parseJSONObject o))

instance ToJSON OperationUserLabels where
        toJSON = toJSON . _oulAddtional

-- | The log entry payload, represented as a structure that is expressed as a
-- JSON object.
--
-- /See:/ 'logEntryStructPayload' smart constructor.
newtype LogEntryStructPayload = LogEntryStructPayload'
    { _lespAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LogEntryStructPayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lespAddtional'
logEntryStructPayload
    :: HashMap Text JSONValue -- ^ 'lespAddtional'
    -> LogEntryStructPayload
logEntryStructPayload pLespAddtional_ = 
    LogEntryStructPayload'
    { _lespAddtional = _Coerce # pLespAddtional_
    }

-- | Properties of the object.
lespAddtional :: Lens' LogEntryStructPayload (HashMap Text JSONValue)
lespAddtional
  = lens _lespAddtional
      (\ s a -> s{_lespAddtional = a})
      . _Coerce

instance FromJSON LogEntryStructPayload where
        parseJSON
          = withObject "LogEntryStructPayload"
              (\ o ->
                 LogEntryStructPayload' <$> (parseJSONObject o))

instance ToJSON LogEntryStructPayload where
        toJSON = toJSON . _lespAddtional
