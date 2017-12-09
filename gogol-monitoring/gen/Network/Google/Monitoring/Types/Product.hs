{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Monitoring.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Monitoring.Types.Product where

import Network.Google.Monitoring.Types.Sum
import Network.Google.Prelude

-- | An object that describes the schema of a MonitoredResource object using
-- a type name and a set of labels. For example, the monitored resource
-- descriptor for Google Compute Engine VM instances has a type of
-- \"gce_instance\" and specifies the use of the labels \"instance_id\" and
-- \"zone\" to identify particular VM instances.Different APIs can support
-- different monitored resource types. APIs generally provide a list method
-- that returns the monitored resource descriptors used by the API.
--
-- /See:/ 'monitoredResourceDescriptor' smart constructor.
data MonitoredResourceDescriptor = MonitoredResourceDescriptor'
    { _mrdName :: !(Maybe Text)
    , _mrdDisplayName :: !(Maybe Text)
    , _mrdLabels :: !(Maybe [LabelDescriptor])
    , _mrdType :: !(Maybe Text)
    , _mrdDescription :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MonitoredResourceDescriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrdName'
--
-- * 'mrdDisplayName'
--
-- * 'mrdLabels'
--
-- * 'mrdType'
--
-- * 'mrdDescription'
monitoredResourceDescriptor
    :: MonitoredResourceDescriptor
monitoredResourceDescriptor = 
    MonitoredResourceDescriptor'
    { _mrdName = Nothing
    , _mrdDisplayName = Nothing
    , _mrdLabels = Nothing
    , _mrdType = Nothing
    , _mrdDescription = Nothing
    }

-- | Optional. The resource name of the monitored resource descriptor:
-- \"projects\/{project_id}\/monitoredResourceDescriptors\/{type}\" where
-- {type} is the value of the type field in this object and {project_id} is
-- a project ID that provides API-specific context for accessing the type.
-- APIs that do not use project information can use the resource name
-- format \"monitoredResourceDescriptors\/{type}\".
mrdName :: Lens' MonitoredResourceDescriptor (Maybe Text)
mrdName = lens _mrdName (\ s a -> s{_mrdName = a})

-- | Optional. A concise name for the monitored resource type that might be
-- displayed in user interfaces. It should be a Title Cased Noun Phrase,
-- without any article or other determiners. For example, \"Google Cloud
-- SQL Database\".
mrdDisplayName :: Lens' MonitoredResourceDescriptor (Maybe Text)
mrdDisplayName
  = lens _mrdDisplayName
      (\ s a -> s{_mrdDisplayName = a})

-- | Required. A set of labels used to describe instances of this monitored
-- resource type. For example, an individual Google Cloud SQL database is
-- identified by values for the labels \"database_id\" and \"zone\".
mrdLabels :: Lens' MonitoredResourceDescriptor [LabelDescriptor]
mrdLabels
  = lens _mrdLabels (\ s a -> s{_mrdLabels = a}) .
      _Default
      . _Coerce

-- | Required. The monitored resource type. For example, the type
-- \"cloudsql_database\" represents databases in Google Cloud SQL. The
-- maximum length of this value is 256 characters.
mrdType :: Lens' MonitoredResourceDescriptor (Maybe Text)
mrdType = lens _mrdType (\ s a -> s{_mrdType = a})

-- | Optional. A detailed description of the monitored resource type that
-- might be used in documentation.
mrdDescription :: Lens' MonitoredResourceDescriptor (Maybe Text)
mrdDescription
  = lens _mrdDescription
      (\ s a -> s{_mrdDescription = a})

instance FromJSON MonitoredResourceDescriptor where
        parseJSON
          = withObject "MonitoredResourceDescriptor"
              (\ o ->
                 MonitoredResourceDescriptor' <$>
                   (o .:? "name") <*> (o .:? "displayName") <*>
                     (o .:? "labels" .!= mempty)
                     <*> (o .:? "type")
                     <*> (o .:? "description"))

instance ToJSON MonitoredResourceDescriptor where
        toJSON MonitoredResourceDescriptor'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _mrdName,
                  ("displayName" .=) <$> _mrdDisplayName,
                  ("labels" .=) <$> _mrdLabels,
                  ("type" .=) <$> _mrdType,
                  ("description" .=) <$> _mrdDescription])

-- | The Status type defines a logical error model that is suitable for
-- different programming environments, including REST APIs and RPC APIs. It
-- is used by gRPC (https:\/\/github.com\/grpc). The error model is
-- designed to be: Simple to use and understand for most users Flexible
-- enough to meet unexpected needsOverviewThe Status message contains three
-- pieces of data: error code, error message, and error details. The error
-- code should be an enum value of google.rpc.Code, but it may accept
-- additional error codes if needed. The error message should be a
-- developer-facing English message that helps developers understand and
-- resolve the error. If a localized user-facing error message is needed,
-- put the localized message in the error details or localize it in the
-- client. The optional error details may contain arbitrary information
-- about the error. There is a predefined set of error detail types in the
-- package google.rpc that can be used for common error conditions.Language
-- mappingThe Status message is the logical representation of the error
-- model, but it is not necessarily the actual wire format. When the Status
-- message is exposed in different client libraries and different wire
-- protocols, it can be mapped differently. For example, it will likely be
-- mapped to some exceptions in Java, but more likely mapped to some error
-- codes in C.Other usesThe error model and the Status message can be used
-- in a variety of environments, either with or without APIs, to provide a
-- consistent developer experience across different environments.Example
-- uses of this error model include: Partial errors. If a service needs to
-- return partial errors to the client, it may embed the Status in the
-- normal response to indicate the partial errors. Workflow errors. A
-- typical workflow has multiple steps. Each step may have a Status message
-- for error reporting. Batch operations. If a client uses batch request
-- and batch response, the Status message should be used directly inside
-- batch response, one for each error sub-response. Asynchronous
-- operations. If an API call embeds asynchronous operation results in its
-- response, the status of those operations should be represented directly
-- using the Status message. Logging. If some API errors are stored in
-- logs, the message Status could be used directly after any stripping
-- needed for security\/privacy reasons.
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

-- | The ListTimeSeries response.
--
-- /See:/ 'listTimeSeriesResponse' smart constructor.
data ListTimeSeriesResponse = ListTimeSeriesResponse'
    { _ltsrNextPageToken :: !(Maybe Text)
    , _ltsrTimeSeries :: !(Maybe [TimeSeries])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTimeSeriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsrNextPageToken'
--
-- * 'ltsrTimeSeries'
listTimeSeriesResponse
    :: ListTimeSeriesResponse
listTimeSeriesResponse = 
    ListTimeSeriesResponse'
    { _ltsrNextPageToken = Nothing
    , _ltsrTimeSeries = Nothing
    }

-- | If there are more results than have been returned, then this field is
-- set to a non-empty value. To see the additional results, use that value
-- as pageToken in the next call to this method.
ltsrNextPageToken :: Lens' ListTimeSeriesResponse (Maybe Text)
ltsrNextPageToken
  = lens _ltsrNextPageToken
      (\ s a -> s{_ltsrNextPageToken = a})

-- | One or more time series that match the filter included in the request.
ltsrTimeSeries :: Lens' ListTimeSeriesResponse [TimeSeries]
ltsrTimeSeries
  = lens _ltsrTimeSeries
      (\ s a -> s{_ltsrTimeSeries = a})
      . _Default
      . _Coerce

instance FromJSON ListTimeSeriesResponse where
        parseJSON
          = withObject "ListTimeSeriesResponse"
              (\ o ->
                 ListTimeSeriesResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "timeSeries" .!= mempty))

instance ToJSON ListTimeSeriesResponse where
        toJSON ListTimeSeriesResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _ltsrNextPageToken,
                  ("timeSeries" .=) <$> _ltsrTimeSeries])

-- | Defines a metric type and its schema. Once a metric descriptor is
-- created, deleting or altering it stops data collection and makes the
-- metric type\'s existing data unusable.
--
-- /See:/ 'metricDescriptor' smart constructor.
data MetricDescriptor = MetricDescriptor'
    { _mdMetricKind :: !(Maybe MetricDescriptorMetricKind)
    , _mdName :: !(Maybe Text)
    , _mdDisplayName :: !(Maybe Text)
    , _mdLabels :: !(Maybe [LabelDescriptor])
    , _mdType :: !(Maybe Text)
    , _mdValueType :: !(Maybe MetricDescriptorValueType)
    , _mdDescription :: !(Maybe Text)
    , _mdUnit :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricDescriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdMetricKind'
--
-- * 'mdName'
--
-- * 'mdDisplayName'
--
-- * 'mdLabels'
--
-- * 'mdType'
--
-- * 'mdValueType'
--
-- * 'mdDescription'
--
-- * 'mdUnit'
metricDescriptor
    :: MetricDescriptor
metricDescriptor = 
    MetricDescriptor'
    { _mdMetricKind = Nothing
    , _mdName = Nothing
    , _mdDisplayName = Nothing
    , _mdLabels = Nothing
    , _mdType = Nothing
    , _mdValueType = Nothing
    , _mdDescription = Nothing
    , _mdUnit = Nothing
    }

-- | Whether the metric records instantaneous values, changes to a value,
-- etc. Some combinations of metric_kind and value_type might not be
-- supported.
mdMetricKind :: Lens' MetricDescriptor (Maybe MetricDescriptorMetricKind)
mdMetricKind
  = lens _mdMetricKind (\ s a -> s{_mdMetricKind = a})

-- | The resource name of the metric descriptor.
mdName :: Lens' MetricDescriptor (Maybe Text)
mdName = lens _mdName (\ s a -> s{_mdName = a})

-- | A concise name for the metric, which can be displayed in user
-- interfaces. Use sentence case without an ending period, for example
-- \"Request count\". This field is optional but it is recommended to be
-- set for any metrics associated with user-visible concepts, such as
-- Quota.
mdDisplayName :: Lens' MetricDescriptor (Maybe Text)
mdDisplayName
  = lens _mdDisplayName
      (\ s a -> s{_mdDisplayName = a})

-- | The set of labels that can be used to describe a specific instance of
-- this metric type. For example, the
-- appengine.googleapis.com\/http\/server\/response_latencies metric type
-- has a label for the HTTP response code, response_code, so you can look
-- at latencies for successful responses or just for responses that failed.
mdLabels :: Lens' MetricDescriptor [LabelDescriptor]
mdLabels
  = lens _mdLabels (\ s a -> s{_mdLabels = a}) .
      _Default
      . _Coerce

-- | The metric type, including its DNS name prefix. The type is not
-- URL-encoded. All user-defined custom metric types have the DNS name
-- custom.googleapis.com. Metric types should use a natural hierarchical
-- grouping. For example: \"custom.googleapis.com\/invoice\/paid\/amount\"
-- \"appengine.googleapis.com\/http\/server\/response_latencies\"
mdType :: Lens' MetricDescriptor (Maybe Text)
mdType = lens _mdType (\ s a -> s{_mdType = a})

-- | Whether the measurement is an integer, a floating-point number, etc.
-- Some combinations of metric_kind and value_type might not be supported.
mdValueType :: Lens' MetricDescriptor (Maybe MetricDescriptorValueType)
mdValueType
  = lens _mdValueType (\ s a -> s{_mdValueType = a})

-- | A detailed description of the metric, which can be used in
-- documentation.
mdDescription :: Lens' MetricDescriptor (Maybe Text)
mdDescription
  = lens _mdDescription
      (\ s a -> s{_mdDescription = a})

-- | Optional. The unit in which the metric value is reported. For example,
-- kBy\/s means kilobytes\/sec, and 1 is the dimensionless unit. The
-- supported units are a subset of The Unified Code for Units of Measure
-- standard (http:\/\/unitsofmeasure.org\/ucum.html).
-- This field is part of the metric\'s documentation, but it is ignored by
-- Stackdriver.
mdUnit :: Lens' MetricDescriptor (Maybe Text)
mdUnit = lens _mdUnit (\ s a -> s{_mdUnit = a})

instance FromJSON MetricDescriptor where
        parseJSON
          = withObject "MetricDescriptor"
              (\ o ->
                 MetricDescriptor' <$>
                   (o .:? "metricKind") <*> (o .:? "name") <*>
                     (o .:? "displayName")
                     <*> (o .:? "labels" .!= mempty)
                     <*> (o .:? "type")
                     <*> (o .:? "valueType")
                     <*> (o .:? "description")
                     <*> (o .:? "unit"))

instance ToJSON MetricDescriptor where
        toJSON MetricDescriptor'{..}
          = object
              (catMaybes
                 [("metricKind" .=) <$> _mdMetricKind,
                  ("name" .=) <$> _mdName,
                  ("displayName" .=) <$> _mdDisplayName,
                  ("labels" .=) <$> _mdLabels, ("type" .=) <$> _mdType,
                  ("valueType" .=) <$> _mdValueType,
                  ("description" .=) <$> _mdDescription,
                  ("unit" .=) <$> _mdUnit])

-- | The description of a dynamic collection of monitored resources. Each
-- group has a filter that is matched against monitored resources and their
-- associated metadata. If a group\'s filter matches an available monitored
-- resource, then that resource is a member of that group. Groups can
-- contain any number of monitored resources, and each monitored resource
-- can be a member of any number of groups.Groups can be nested in
-- parent-child hierarchies. The parentName field identifies an optional
-- parent for each group. If a group has a parent, then the only monitored
-- resources available to be matched by the group\'s filter are the
-- resources contained in the parent group. In other words, a group
-- contains the monitored resources that match its filter and the filters
-- of all the group\'s ancestors. A group without a parent can contain any
-- monitored resource.For example, consider an infrastructure running a set
-- of instances with two user-defined tags: \"environment\" and \"role\". A
-- parent group has a filter, environment=\"production\". A child of that
-- parent group has a filter, role=\"transcoder\". The parent group
-- contains all instances in the production environment, regardless of
-- their roles. The child group contains instances that have the transcoder
-- role and are in the production environment.The monitored resources
-- contained in a group can change at any moment, depending on what
-- resources exist and what filters are associated with the group and its
-- ancestors.
--
-- /See:/ 'group'' smart constructor.
data Group = Group'
    { _gName :: !(Maybe Text)
    , _gDisplayName :: !(Maybe Text)
    , _gFilter :: !(Maybe Text)
    , _gIsCluster :: !(Maybe Bool)
    , _gParentName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gName'
--
-- * 'gDisplayName'
--
-- * 'gFilter'
--
-- * 'gIsCluster'
--
-- * 'gParentName'
group'
    :: Group
group' = 
    Group'
    { _gName = Nothing
    , _gDisplayName = Nothing
    , _gFilter = Nothing
    , _gIsCluster = Nothing
    , _gParentName = Nothing
    }

-- | Output only. The name of this group. The format is
-- \"projects\/{project_id_or_number}\/groups\/{group_id}\". When creating
-- a group, this field is ignored and a new name is created consisting of
-- the project specified in the call to CreateGroup and a unique {group_id}
-- that is generated automatically.
gName :: Lens' Group (Maybe Text)
gName = lens _gName (\ s a -> s{_gName = a})

-- | A user-assigned name for this group, used only for display purposes.
gDisplayName :: Lens' Group (Maybe Text)
gDisplayName
  = lens _gDisplayName (\ s a -> s{_gDisplayName = a})

-- | The filter used to determine which monitored resources belong to this
-- group.
gFilter :: Lens' Group (Maybe Text)
gFilter = lens _gFilter (\ s a -> s{_gFilter = a})

-- | If true, the members of this group are considered to be a cluster. The
-- system can perform additional analysis on groups that are clusters.
gIsCluster :: Lens' Group (Maybe Bool)
gIsCluster
  = lens _gIsCluster (\ s a -> s{_gIsCluster = a})

-- | The name of the group\'s parent, if it has one. The format is
-- \"projects\/{project_id_or_number}\/groups\/{group_id}\". For groups
-- with no parent, parentName is the empty string, \"\".
gParentName :: Lens' Group (Maybe Text)
gParentName
  = lens _gParentName (\ s a -> s{_gParentName = a})

instance FromJSON Group where
        parseJSON
          = withObject "Group"
              (\ o ->
                 Group' <$>
                   (o .:? "name") <*> (o .:? "displayName") <*>
                     (o .:? "filter")
                     <*> (o .:? "isCluster")
                     <*> (o .:? "parentName"))

instance ToJSON Group where
        toJSON Group'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _gName,
                  ("displayName" .=) <$> _gDisplayName,
                  ("filter" .=) <$> _gFilter,
                  ("isCluster" .=) <$> _gIsCluster,
                  ("parentName" .=) <$> _gParentName])

-- | A single strongly-typed value.
--
-- /See:/ 'typedValue' smart constructor.
data TypedValue = TypedValue'
    { _tvBoolValue :: !(Maybe Bool)
    , _tvDoubleValue :: !(Maybe (Textual Double))
    , _tvStringValue :: !(Maybe Text)
    , _tvDistributionValue :: !(Maybe Distribution)
    , _tvInt64Value :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TypedValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tvBoolValue'
--
-- * 'tvDoubleValue'
--
-- * 'tvStringValue'
--
-- * 'tvDistributionValue'
--
-- * 'tvInt64Value'
typedValue
    :: TypedValue
typedValue = 
    TypedValue'
    { _tvBoolValue = Nothing
    , _tvDoubleValue = Nothing
    , _tvStringValue = Nothing
    , _tvDistributionValue = Nothing
    , _tvInt64Value = Nothing
    }

-- | A Boolean value: true or false.
tvBoolValue :: Lens' TypedValue (Maybe Bool)
tvBoolValue
  = lens _tvBoolValue (\ s a -> s{_tvBoolValue = a})

-- | A 64-bit double-precision floating-point number. Its magnitude is
-- approximately ±10±300 and it has 16 significant digits of precision.
tvDoubleValue :: Lens' TypedValue (Maybe Double)
tvDoubleValue
  = lens _tvDoubleValue
      (\ s a -> s{_tvDoubleValue = a})
      . mapping _Coerce

-- | A variable-length string value.
tvStringValue :: Lens' TypedValue (Maybe Text)
tvStringValue
  = lens _tvStringValue
      (\ s a -> s{_tvStringValue = a})

-- | A distribution value.
tvDistributionValue :: Lens' TypedValue (Maybe Distribution)
tvDistributionValue
  = lens _tvDistributionValue
      (\ s a -> s{_tvDistributionValue = a})

-- | A 64-bit integer. Its range is approximately ±9.2x1018.
tvInt64Value :: Lens' TypedValue (Maybe Int64)
tvInt64Value
  = lens _tvInt64Value (\ s a -> s{_tvInt64Value = a})
      . mapping _Coerce

instance FromJSON TypedValue where
        parseJSON
          = withObject "TypedValue"
              (\ o ->
                 TypedValue' <$>
                   (o .:? "boolValue") <*> (o .:? "doubleValue") <*>
                     (o .:? "stringValue")
                     <*> (o .:? "distributionValue")
                     <*> (o .:? "int64Value"))

instance ToJSON TypedValue where
        toJSON TypedValue'{..}
          = object
              (catMaybes
                 [("boolValue" .=) <$> _tvBoolValue,
                  ("doubleValue" .=) <$> _tvDoubleValue,
                  ("stringValue" .=) <$> _tvStringValue,
                  ("distributionValue" .=) <$> _tvDistributionValue,
                  ("int64Value" .=) <$> _tvInt64Value])

-- | Required. Values for all of the labels listed in the associated
-- monitored resource descriptor. For example, Compute Engine VM instances
-- use the labels \"project_id\", \"instance_id\", and \"zone\".
--
-- /See:/ 'monitoredResourceLabels' smart constructor.
newtype MonitoredResourceLabels = MonitoredResourceLabels'
    { _mrlAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MonitoredResourceLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrlAddtional'
monitoredResourceLabels
    :: HashMap Text Text -- ^ 'mrlAddtional'
    -> MonitoredResourceLabels
monitoredResourceLabels pMrlAddtional_ = 
    MonitoredResourceLabels'
    { _mrlAddtional = _Coerce # pMrlAddtional_
    }

mrlAddtional :: Lens' MonitoredResourceLabels (HashMap Text Text)
mrlAddtional
  = lens _mrlAddtional (\ s a -> s{_mrlAddtional = a})
      . _Coerce

instance FromJSON MonitoredResourceLabels where
        parseJSON
          = withObject "MonitoredResourceLabels"
              (\ o ->
                 MonitoredResourceLabels' <$> (parseJSONObject o))

instance ToJSON MonitoredResourceLabels where
        toJSON = toJSON . _mrlAddtional

-- | SourceContext represents information about the source of a protobuf
-- element, like the file in which it is defined.
--
-- /See:/ 'sourceContext' smart constructor.
newtype SourceContext = SourceContext'
    { _scFileName :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SourceContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scFileName'
sourceContext
    :: SourceContext
sourceContext = 
    SourceContext'
    { _scFileName = Nothing
    }

-- | The path-qualified name of the .proto file that contained the associated
-- protobuf element. For example:
-- \"google\/protobuf\/source_context.proto\".
scFileName :: Lens' SourceContext (Maybe Text)
scFileName
  = lens _scFileName (\ s a -> s{_scFileName = a})

instance FromJSON SourceContext where
        parseJSON
          = withObject "SourceContext"
              (\ o -> SourceContext' <$> (o .:? "fileName"))

instance ToJSON SourceContext where
        toJSON SourceContext'{..}
          = object
              (catMaybes [("fileName" .=) <$> _scFileName])

-- | A type of authentication to perform against the specified resource or
-- URL that uses username and password. Currently, only Basic
-- authentication is supported in Uptime Monitoring.
--
-- /See:/ 'basicAuthentication' smart constructor.
data BasicAuthentication = BasicAuthentication'
    { _baUsername :: !(Maybe Text)
    , _baPassword :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BasicAuthentication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baUsername'
--
-- * 'baPassword'
basicAuthentication
    :: BasicAuthentication
basicAuthentication = 
    BasicAuthentication'
    { _baUsername = Nothing
    , _baPassword = Nothing
    }

-- | The username to authenticate.
baUsername :: Lens' BasicAuthentication (Maybe Text)
baUsername
  = lens _baUsername (\ s a -> s{_baUsername = a})

-- | The password to authenticate.
baPassword :: Lens' BasicAuthentication (Maybe Text)
baPassword
  = lens _baPassword (\ s a -> s{_baPassword = a})

instance FromJSON BasicAuthentication where
        parseJSON
          = withObject "BasicAuthentication"
              (\ o ->
                 BasicAuthentication' <$>
                   (o .:? "username") <*> (o .:? "password"))

instance ToJSON BasicAuthentication where
        toJSON BasicAuthentication'{..}
          = object
              (catMaybes
                 [("username" .=) <$> _baUsername,
                  ("password" .=) <$> _baPassword])

-- | Distribution contains summary statistics for a population of values. It
-- optionally contains a histogram representing the distribution of those
-- values across a set of buckets.The summary statistics are the count,
-- mean, sum of the squared deviation from the mean, the minimum, and the
-- maximum of the set of population of values. The histogram is based on a
-- sequence of buckets and gives a count of values that fall into each
-- bucket. The boundaries of the buckets are given either explicitly or by
-- formulas for buckets of fixed or exponentially increasing
-- widths.Although it is not forbidden, it is generally a bad idea to
-- include non-finite values (infinities or NaNs) in the population of
-- values, as this will render the mean and sum_of_squared_deviation fields
-- meaningless.
--
-- /See:/ 'distribution' smart constructor.
data Distribution = Distribution'
    { _dSumOfSquaredDeviation :: !(Maybe (Textual Double))
    , _dMean :: !(Maybe (Textual Double))
    , _dCount :: !(Maybe (Textual Int64))
    , _dBucketCounts :: !(Maybe [Textual Int64])
    , _dRange :: !(Maybe Range)
    , _dBucketOptions :: !(Maybe BucketOptions)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Distribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSumOfSquaredDeviation'
--
-- * 'dMean'
--
-- * 'dCount'
--
-- * 'dBucketCounts'
--
-- * 'dRange'
--
-- * 'dBucketOptions'
distribution
    :: Distribution
distribution = 
    Distribution'
    { _dSumOfSquaredDeviation = Nothing
    , _dMean = Nothing
    , _dCount = Nothing
    , _dBucketCounts = Nothing
    , _dRange = Nothing
    , _dBucketOptions = Nothing
    }

-- | The sum of squared deviations from the mean of the values in the
-- population. For values x_i this is: Sum[i=1..n]((x_i - mean)^2) Knuth,
-- \"The Art of Computer Programming\", Vol. 2, page 323, 3rd edition
-- describes Welford\'s method for accumulating this sum in one pass.If
-- count is zero then this field must be zero.
dSumOfSquaredDeviation :: Lens' Distribution (Maybe Double)
dSumOfSquaredDeviation
  = lens _dSumOfSquaredDeviation
      (\ s a -> s{_dSumOfSquaredDeviation = a})
      . mapping _Coerce

-- | The arithmetic mean of the values in the population. If count is zero
-- then this field must be zero.
dMean :: Lens' Distribution (Maybe Double)
dMean
  = lens _dMean (\ s a -> s{_dMean = a}) .
      mapping _Coerce

-- | The number of values in the population. Must be non-negative. This value
-- must equal the sum of the values in bucket_counts if a histogram is
-- provided.
dCount :: Lens' Distribution (Maybe Int64)
dCount
  = lens _dCount (\ s a -> s{_dCount = a}) .
      mapping _Coerce

-- | Required in the Stackdriver Monitoring API v3. The values for each
-- bucket specified in bucket_options. The sum of the values in
-- bucketCounts must equal the value in the count field of the Distribution
-- object. The order of the bucket counts follows the numbering schemes
-- described for the three bucket types. The underflow bucket has number 0;
-- the finite buckets, if any, have numbers 1 through N-2; and the overflow
-- bucket has number N-1. The size of bucket_counts must not be greater
-- than N. If the size is less than N, then the remaining buckets are
-- assigned values of zero.
dBucketCounts :: Lens' Distribution [Int64]
dBucketCounts
  = lens _dBucketCounts
      (\ s a -> s{_dBucketCounts = a})
      . _Default
      . _Coerce

-- | If specified, contains the range of the population values. The field
-- must not be present if the count is zero. This field is presently
-- ignored by the Stackdriver Monitoring API v3.
dRange :: Lens' Distribution (Maybe Range)
dRange = lens _dRange (\ s a -> s{_dRange = a})

-- | Required in the Stackdriver Monitoring API v3. Defines the histogram
-- bucket boundaries.
dBucketOptions :: Lens' Distribution (Maybe BucketOptions)
dBucketOptions
  = lens _dBucketOptions
      (\ s a -> s{_dBucketOptions = a})

instance FromJSON Distribution where
        parseJSON
          = withObject "Distribution"
              (\ o ->
                 Distribution' <$>
                   (o .:? "sumOfSquaredDeviation") <*> (o .:? "mean")
                     <*> (o .:? "count")
                     <*> (o .:? "bucketCounts" .!= mempty)
                     <*> (o .:? "range")
                     <*> (o .:? "bucketOptions"))

instance ToJSON Distribution where
        toJSON Distribution'{..}
          = object
              (catMaybes
                 [("sumOfSquaredDeviation" .=) <$>
                    _dSumOfSquaredDeviation,
                  ("mean" .=) <$> _dMean, ("count" .=) <$> _dCount,
                  ("bucketCounts" .=) <$> _dBucketCounts,
                  ("range" .=) <$> _dRange,
                  ("bucketOptions" .=) <$> _dBucketOptions])

-- | A single field of a message type.
--
-- /See:/ 'field' smart constructor.
data Field = Field'
    { _fKind :: !(Maybe FieldKind)
    , _fOneofIndex :: !(Maybe (Textual Int32))
    , _fName :: !(Maybe Text)
    , _fJSONName :: !(Maybe Text)
    , _fCardinality :: !(Maybe FieldCardinality)
    , _fOptions :: !(Maybe [Option])
    , _fPacked :: !(Maybe Bool)
    , _fDefaultValue :: !(Maybe Text)
    , _fNumber :: !(Maybe (Textual Int32))
    , _fTypeURL :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Field' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fKind'
--
-- * 'fOneofIndex'
--
-- * 'fName'
--
-- * 'fJSONName'
--
-- * 'fCardinality'
--
-- * 'fOptions'
--
-- * 'fPacked'
--
-- * 'fDefaultValue'
--
-- * 'fNumber'
--
-- * 'fTypeURL'
field
    :: Field
field = 
    Field'
    { _fKind = Nothing
    , _fOneofIndex = Nothing
    , _fName = Nothing
    , _fJSONName = Nothing
    , _fCardinality = Nothing
    , _fOptions = Nothing
    , _fPacked = Nothing
    , _fDefaultValue = Nothing
    , _fNumber = Nothing
    , _fTypeURL = Nothing
    }

-- | The field type.
fKind :: Lens' Field (Maybe FieldKind)
fKind = lens _fKind (\ s a -> s{_fKind = a})

-- | The index of the field type in Type.oneofs, for message or enumeration
-- types. The first type has index 1; zero means the type is not in the
-- list.
fOneofIndex :: Lens' Field (Maybe Int32)
fOneofIndex
  = lens _fOneofIndex (\ s a -> s{_fOneofIndex = a}) .
      mapping _Coerce

-- | The field name.
fName :: Lens' Field (Maybe Text)
fName = lens _fName (\ s a -> s{_fName = a})

-- | The field JSON name.
fJSONName :: Lens' Field (Maybe Text)
fJSONName
  = lens _fJSONName (\ s a -> s{_fJSONName = a})

-- | The field cardinality.
fCardinality :: Lens' Field (Maybe FieldCardinality)
fCardinality
  = lens _fCardinality (\ s a -> s{_fCardinality = a})

-- | The protocol buffer options.
fOptions :: Lens' Field [Option]
fOptions
  = lens _fOptions (\ s a -> s{_fOptions = a}) .
      _Default
      . _Coerce

-- | Whether to use alternative packed wire representation.
fPacked :: Lens' Field (Maybe Bool)
fPacked = lens _fPacked (\ s a -> s{_fPacked = a})

-- | The string value of the default value of this field. Proto2 syntax only.
fDefaultValue :: Lens' Field (Maybe Text)
fDefaultValue
  = lens _fDefaultValue
      (\ s a -> s{_fDefaultValue = a})

-- | The field number.
fNumber :: Lens' Field (Maybe Int32)
fNumber
  = lens _fNumber (\ s a -> s{_fNumber = a}) .
      mapping _Coerce

-- | The field type URL, without the scheme, for message or enumeration
-- types. Example: \"type.googleapis.com\/google.protobuf.Timestamp\".
fTypeURL :: Lens' Field (Maybe Text)
fTypeURL = lens _fTypeURL (\ s a -> s{_fTypeURL = a})

instance FromJSON Field where
        parseJSON
          = withObject "Field"
              (\ o ->
                 Field' <$>
                   (o .:? "kind") <*> (o .:? "oneofIndex") <*>
                     (o .:? "name")
                     <*> (o .:? "jsonName")
                     <*> (o .:? "cardinality")
                     <*> (o .:? "options" .!= mempty)
                     <*> (o .:? "packed")
                     <*> (o .:? "defaultValue")
                     <*> (o .:? "number")
                     <*> (o .:? "typeUrl"))

instance ToJSON Field where
        toJSON Field'{..}
          = object
              (catMaybes
                 [("kind" .=) <$> _fKind,
                  ("oneofIndex" .=) <$> _fOneofIndex,
                  ("name" .=) <$> _fName,
                  ("jsonName" .=) <$> _fJSONName,
                  ("cardinality" .=) <$> _fCardinality,
                  ("options" .=) <$> _fOptions,
                  ("packed" .=) <$> _fPacked,
                  ("defaultValue" .=) <$> _fDefaultValue,
                  ("number" .=) <$> _fNumber,
                  ("typeUrl" .=) <$> _fTypeURL])

-- | A generic empty message that you can re-use to avoid defining duplicated
-- empty messages in your APIs. A typical example is to use it as the
-- request or the response type of an API method. For instance: service Foo
-- { rpc Bar(google.protobuf.Empty) returns (google.protobuf.Empty); } The
-- JSON representation for Empty is empty JSON object {}.
--
-- /See:/ 'empty' smart constructor.
data Empty =
    Empty' 
    deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Empty' with the minimum fields required to make a request.
--
empty
    :: Empty
empty = Empty'

instance FromJSON Empty where
        parseJSON = withObject "Empty" (\ o -> pure Empty')

instance ToJSON Empty where
        toJSON = const emptyObject

-- | The ListGroups response.
--
-- /See:/ 'listGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
    { _lgrNextPageToken :: !(Maybe Text)
    , _lgrGroup :: !(Maybe [Group])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrNextPageToken'
--
-- * 'lgrGroup'
listGroupsResponse
    :: ListGroupsResponse
listGroupsResponse = 
    ListGroupsResponse'
    { _lgrNextPageToken = Nothing
    , _lgrGroup = Nothing
    }

-- | If there are more results than have been returned, then this field is
-- set to a non-empty value. To see the additional results, use that value
-- as pageToken in the next call to this method.
lgrNextPageToken :: Lens' ListGroupsResponse (Maybe Text)
lgrNextPageToken
  = lens _lgrNextPageToken
      (\ s a -> s{_lgrNextPageToken = a})

-- | The groups that match the specified filters.
lgrGroup :: Lens' ListGroupsResponse [Group]
lgrGroup
  = lens _lgrGroup (\ s a -> s{_lgrGroup = a}) .
      _Default
      . _Coerce

instance FromJSON ListGroupsResponse where
        parseJSON
          = withObject "ListGroupsResponse"
              (\ o ->
                 ListGroupsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "group" .!= mempty))

instance ToJSON ListGroupsResponse where
        toJSON ListGroupsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lgrNextPageToken,
                  ("group" .=) <$> _lgrGroup])

-- | The ListMetricDescriptors response.
--
-- /See:/ 'listMetricDescriptorsResponse' smart constructor.
data ListMetricDescriptorsResponse = ListMetricDescriptorsResponse'
    { _lmdrMetricDescriptors :: !(Maybe [MetricDescriptor])
    , _lmdrNextPageToken :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListMetricDescriptorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmdrMetricDescriptors'
--
-- * 'lmdrNextPageToken'
listMetricDescriptorsResponse
    :: ListMetricDescriptorsResponse
listMetricDescriptorsResponse = 
    ListMetricDescriptorsResponse'
    { _lmdrMetricDescriptors = Nothing
    , _lmdrNextPageToken = Nothing
    }

-- | The metric descriptors that are available to the project and that match
-- the value of filter, if present.
lmdrMetricDescriptors :: Lens' ListMetricDescriptorsResponse [MetricDescriptor]
lmdrMetricDescriptors
  = lens _lmdrMetricDescriptors
      (\ s a -> s{_lmdrMetricDescriptors = a})
      . _Default
      . _Coerce

-- | If there are more results than have been returned, then this field is
-- set to a non-empty value. To see the additional results, use that value
-- as pageToken in the next call to this method.
lmdrNextPageToken :: Lens' ListMetricDescriptorsResponse (Maybe Text)
lmdrNextPageToken
  = lens _lmdrNextPageToken
      (\ s a -> s{_lmdrNextPageToken = a})

instance FromJSON ListMetricDescriptorsResponse where
        parseJSON
          = withObject "ListMetricDescriptorsResponse"
              (\ o ->
                 ListMetricDescriptorsResponse' <$>
                   (o .:? "metricDescriptors" .!= mempty) <*>
                     (o .:? "nextPageToken"))

instance ToJSON ListMetricDescriptorsResponse where
        toJSON ListMetricDescriptorsResponse'{..}
          = object
              (catMaybes
                 [("metricDescriptors" .=) <$> _lmdrMetricDescriptors,
                  ("nextPageToken" .=) <$> _lmdrNextPageToken])

-- | The option\'s value packed in an Any message. If the value is a
-- primitive, the corresponding wrapper type defined in
-- google\/protobuf\/wrappers.proto should be used. If the value is an
-- enum, it should be stored as an int32 value using the
-- google.protobuf.Int32Value type.
--
-- /See:/ 'optionValue' smart constructor.
newtype OptionValue = OptionValue'
    { _ovAddtional :: HashMap Text JSONValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OptionValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ovAddtional'
optionValue
    :: HashMap Text JSONValue -- ^ 'ovAddtional'
    -> OptionValue
optionValue pOvAddtional_ = 
    OptionValue'
    { _ovAddtional = _Coerce # pOvAddtional_
    }

-- | Properties of the object. Contains field \'type with type URL.
ovAddtional :: Lens' OptionValue (HashMap Text JSONValue)
ovAddtional
  = lens _ovAddtional (\ s a -> s{_ovAddtional = a}) .
      _Coerce

instance FromJSON OptionValue where
        parseJSON
          = withObject "OptionValue"
              (\ o -> OptionValue' <$> (parseJSONObject o))

instance ToJSON OptionValue where
        toJSON = toJSON . _ovAddtional

-- | The CreateTimeSeries request.
--
-- /See:/ 'createTimeSeriesRequest' smart constructor.
newtype CreateTimeSeriesRequest = CreateTimeSeriesRequest'
    { _ctsrTimeSeries :: Maybe [TimeSeries]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateTimeSeriesRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctsrTimeSeries'
createTimeSeriesRequest
    :: CreateTimeSeriesRequest
createTimeSeriesRequest = 
    CreateTimeSeriesRequest'
    { _ctsrTimeSeries = Nothing
    }

-- | The new data to be added to a list of time series. Adds at most one data
-- point to each of several time series. The new data point must be more
-- recent than any other point in its time series. Each TimeSeries value
-- must fully specify a unique time series by supplying all label values
-- for the metric and the monitored resource.
ctsrTimeSeries :: Lens' CreateTimeSeriesRequest [TimeSeries]
ctsrTimeSeries
  = lens _ctsrTimeSeries
      (\ s a -> s{_ctsrTimeSeries = a})
      . _Default
      . _Coerce

instance FromJSON CreateTimeSeriesRequest where
        parseJSON
          = withObject "CreateTimeSeriesRequest"
              (\ o ->
                 CreateTimeSeriesRequest' <$>
                   (o .:? "timeSeries" .!= mempty))

instance ToJSON CreateTimeSeriesRequest where
        toJSON CreateTimeSeriesRequest'{..}
          = object
              (catMaybes [("timeSeries" .=) <$> _ctsrTimeSeries])

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

-- | Nimbus InternalCheckers.
--
-- /See:/ 'internalChecker' smart constructor.
data InternalChecker = InternalChecker'
    { _icNetwork :: !(Maybe Text)
    , _icCheckerId :: !(Maybe Text)
    , _icGcpZone :: !(Maybe Text)
    , _icDisplayName :: !(Maybe Text)
    , _icProjectId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'InternalChecker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icNetwork'
--
-- * 'icCheckerId'
--
-- * 'icGcpZone'
--
-- * 'icDisplayName'
--
-- * 'icProjectId'
internalChecker
    :: InternalChecker
internalChecker = 
    InternalChecker'
    { _icNetwork = Nothing
    , _icCheckerId = Nothing
    , _icGcpZone = Nothing
    , _icDisplayName = Nothing
    , _icProjectId = Nothing
    }

-- | The internal network to perform this uptime check on.
icNetwork :: Lens' InternalChecker (Maybe Text)
icNetwork
  = lens _icNetwork (\ s a -> s{_icNetwork = a})

-- | The checker ID.
icCheckerId :: Lens' InternalChecker (Maybe Text)
icCheckerId
  = lens _icCheckerId (\ s a -> s{_icCheckerId = a})

-- | The GCP zone the uptime check should egress from. Only respected for
-- internal uptime checks, where internal_network is specified.
icGcpZone :: Lens' InternalChecker (Maybe Text)
icGcpZone
  = lens _icGcpZone (\ s a -> s{_icGcpZone = a})

-- | The checker\'s human-readable name.
icDisplayName :: Lens' InternalChecker (Maybe Text)
icDisplayName
  = lens _icDisplayName
      (\ s a -> s{_icDisplayName = a})

-- | The GCP project ID. Not necessarily the same as the project_id for the
-- config.
icProjectId :: Lens' InternalChecker (Maybe Text)
icProjectId
  = lens _icProjectId (\ s a -> s{_icProjectId = a})

instance FromJSON InternalChecker where
        parseJSON
          = withObject "InternalChecker"
              (\ o ->
                 InternalChecker' <$>
                   (o .:? "network") <*> (o .:? "checkerId") <*>
                     (o .:? "gcpZone")
                     <*> (o .:? "displayName")
                     <*> (o .:? "projectId"))

instance ToJSON InternalChecker where
        toJSON InternalChecker'{..}
          = object
              (catMaybes
                 [("network" .=) <$> _icNetwork,
                  ("checkerId" .=) <$> _icCheckerId,
                  ("gcpZone" .=) <$> _icGcpZone,
                  ("displayName" .=) <$> _icDisplayName,
                  ("projectId" .=) <$> _icProjectId])

-- | The ListMonitoredResourceDescriptors response.
--
-- /See:/ 'listMonitoredResourceDescriptorsResponse' smart constructor.
data ListMonitoredResourceDescriptorsResponse = ListMonitoredResourceDescriptorsResponse'
    { _lmrdrNextPageToken :: !(Maybe Text)
    , _lmrdrResourceDescriptors :: !(Maybe [MonitoredResourceDescriptor])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListMonitoredResourceDescriptorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmrdrNextPageToken'
--
-- * 'lmrdrResourceDescriptors'
listMonitoredResourceDescriptorsResponse
    :: ListMonitoredResourceDescriptorsResponse
listMonitoredResourceDescriptorsResponse = 
    ListMonitoredResourceDescriptorsResponse'
    { _lmrdrNextPageToken = Nothing
    , _lmrdrResourceDescriptors = Nothing
    }

-- | If there are more results than have been returned, then this field is
-- set to a non-empty value. To see the additional results, use that value
-- as pageToken in the next call to this method.
lmrdrNextPageToken :: Lens' ListMonitoredResourceDescriptorsResponse (Maybe Text)
lmrdrNextPageToken
  = lens _lmrdrNextPageToken
      (\ s a -> s{_lmrdrNextPageToken = a})

-- | The monitored resource descriptors that are available to this project
-- and that match filter, if present.
lmrdrResourceDescriptors :: Lens' ListMonitoredResourceDescriptorsResponse [MonitoredResourceDescriptor]
lmrdrResourceDescriptors
  = lens _lmrdrResourceDescriptors
      (\ s a -> s{_lmrdrResourceDescriptors = a})
      . _Default
      . _Coerce

instance FromJSON
         ListMonitoredResourceDescriptorsResponse where
        parseJSON
          = withObject
              "ListMonitoredResourceDescriptorsResponse"
              (\ o ->
                 ListMonitoredResourceDescriptorsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "resourceDescriptors" .!= mempty))

instance ToJSON
         ListMonitoredResourceDescriptorsResponse where
        toJSON ListMonitoredResourceDescriptorsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lmrdrNextPageToken,
                  ("resourceDescriptors" .=) <$>
                    _lmrdrResourceDescriptors])

-- | Specifies a set of buckets with arbitrary widths.There are size(bounds)
-- + 1 (= N) buckets. Bucket i has the following boundaries:Upper bound (0
-- \<= i \< N-1): boundsi Lower bound (1 \<= i \< N); boundsi - 1The bounds
-- field must contain at least one element. If bounds has only one element,
-- then there are no finite buckets, and that single element is the common
-- boundary of the overflow and underflow buckets.
--
-- /See:/ 'explicit' smart constructor.
newtype Explicit = Explicit'
    { _eBounds :: Maybe [Textual Double]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Explicit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eBounds'
explicit
    :: Explicit
explicit = 
    Explicit'
    { _eBounds = Nothing
    }

-- | The values must be monotonically increasing.
eBounds :: Lens' Explicit [Double]
eBounds
  = lens _eBounds (\ s a -> s{_eBounds = a}) . _Default
      . _Coerce

instance FromJSON Explicit where
        parseJSON
          = withObject "Explicit"
              (\ o -> Explicit' <$> (o .:? "bounds" .!= mempty))

instance ToJSON Explicit where
        toJSON Explicit'{..}
          = object (catMaybes [("bounds" .=) <$> _eBounds])

-- | The set of label values that uniquely identify this metric. All labels
-- listed in the MetricDescriptor must be assigned values.
--
-- /See:/ 'metricLabels' smart constructor.
newtype MetricLabels = MetricLabels'
    { _mlAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricLabels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mlAddtional'
metricLabels
    :: HashMap Text Text -- ^ 'mlAddtional'
    -> MetricLabels
metricLabels pMlAddtional_ = 
    MetricLabels'
    { _mlAddtional = _Coerce # pMlAddtional_
    }

mlAddtional :: Lens' MetricLabels (HashMap Text Text)
mlAddtional
  = lens _mlAddtional (\ s a -> s{_mlAddtional = a}) .
      _Coerce

instance FromJSON MetricLabels where
        parseJSON
          = withObject "MetricLabels"
              (\ o -> MetricLabels' <$> (parseJSONObject o))

instance ToJSON MetricLabels where
        toJSON = toJSON . _mlAddtional

-- | The measurement metadata. Example: \"process_id\" -> 12345
--
-- /See:/ 'collectdPayloadMetadata' smart constructor.
newtype CollectdPayloadMetadata = CollectdPayloadMetadata'
    { _cpmAddtional :: HashMap Text TypedValue
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CollectdPayloadMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpmAddtional'
collectdPayloadMetadata
    :: HashMap Text TypedValue -- ^ 'cpmAddtional'
    -> CollectdPayloadMetadata
collectdPayloadMetadata pCpmAddtional_ = 
    CollectdPayloadMetadata'
    { _cpmAddtional = _Coerce # pCpmAddtional_
    }

cpmAddtional :: Lens' CollectdPayloadMetadata (HashMap Text TypedValue)
cpmAddtional
  = lens _cpmAddtional (\ s a -> s{_cpmAddtional = a})
      . _Coerce

instance FromJSON CollectdPayloadMetadata where
        parseJSON
          = withObject "CollectdPayloadMetadata"
              (\ o ->
                 CollectdPayloadMetadata' <$> (parseJSONObject o))

instance ToJSON CollectdPayloadMetadata where
        toJSON = toJSON . _cpmAddtional

-- | A single data point from a collectd-based plugin.
--
-- /See:/ 'collectdValue' smart constructor.
data CollectdValue = CollectdValue'
    { _cvDataSourceName :: !(Maybe Text)
    , _cvDataSourceType :: !(Maybe CollectdValueDataSourceType)
    , _cvValue :: !(Maybe TypedValue)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CollectdValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvDataSourceName'
--
-- * 'cvDataSourceType'
--
-- * 'cvValue'
collectdValue
    :: CollectdValue
collectdValue = 
    CollectdValue'
    { _cvDataSourceName = Nothing
    , _cvDataSourceType = Nothing
    , _cvValue = Nothing
    }

-- | The data source for the collectd value. For example there are two data
-- sources for network measurements: \"rx\" and \"tx\".
cvDataSourceName :: Lens' CollectdValue (Maybe Text)
cvDataSourceName
  = lens _cvDataSourceName
      (\ s a -> s{_cvDataSourceName = a})

-- | The type of measurement.
cvDataSourceType :: Lens' CollectdValue (Maybe CollectdValueDataSourceType)
cvDataSourceType
  = lens _cvDataSourceType
      (\ s a -> s{_cvDataSourceType = a})

-- | The measurement value.
cvValue :: Lens' CollectdValue (Maybe TypedValue)
cvValue = lens _cvValue (\ s a -> s{_cvValue = a})

instance FromJSON CollectdValue where
        parseJSON
          = withObject "CollectdValue"
              (\ o ->
                 CollectdValue' <$>
                   (o .:? "dataSourceName") <*> (o .:? "dataSourceType")
                     <*> (o .:? "value"))

instance ToJSON CollectdValue where
        toJSON CollectdValue'{..}
          = object
              (catMaybes
                 [("dataSourceName" .=) <$> _cvDataSourceName,
                  ("dataSourceType" .=) <$> _cvDataSourceType,
                  ("value" .=) <$> _cvValue])

-- | The CreateCollectdTimeSeries request.
--
-- /See:/ 'createCollectdTimeSeriesRequest' smart constructor.
data CreateCollectdTimeSeriesRequest = CreateCollectdTimeSeriesRequest'
    { _cctsrCollectdPayloads :: !(Maybe [CollectdPayload])
    , _cctsrResource :: !(Maybe MonitoredResource)
    , _cctsrCollectdVersion :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCollectdTimeSeriesRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cctsrCollectdPayloads'
--
-- * 'cctsrResource'
--
-- * 'cctsrCollectdVersion'
createCollectdTimeSeriesRequest
    :: CreateCollectdTimeSeriesRequest
createCollectdTimeSeriesRequest = 
    CreateCollectdTimeSeriesRequest'
    { _cctsrCollectdPayloads = Nothing
    , _cctsrResource = Nothing
    , _cctsrCollectdVersion = Nothing
    }

-- | The collectd payloads representing the time series data. You must not
-- include more than a single point for each time series, so no two
-- payloads can have the same values for all of the fields plugin,
-- plugin_instance, type, and type_instance.
cctsrCollectdPayloads :: Lens' CreateCollectdTimeSeriesRequest [CollectdPayload]
cctsrCollectdPayloads
  = lens _cctsrCollectdPayloads
      (\ s a -> s{_cctsrCollectdPayloads = a})
      . _Default
      . _Coerce

-- | The monitored resource associated with the time series.
cctsrResource :: Lens' CreateCollectdTimeSeriesRequest (Maybe MonitoredResource)
cctsrResource
  = lens _cctsrResource
      (\ s a -> s{_cctsrResource = a})

-- | The version of collectd that collected the data. Example:
-- \"5.3.0-192.el6\".
cctsrCollectdVersion :: Lens' CreateCollectdTimeSeriesRequest (Maybe Text)
cctsrCollectdVersion
  = lens _cctsrCollectdVersion
      (\ s a -> s{_cctsrCollectdVersion = a})

instance FromJSON CreateCollectdTimeSeriesRequest
         where
        parseJSON
          = withObject "CreateCollectdTimeSeriesRequest"
              (\ o ->
                 CreateCollectdTimeSeriesRequest' <$>
                   (o .:? "collectdPayloads" .!= mempty) <*>
                     (o .:? "resource")
                     <*> (o .:? "collectdVersion"))

instance ToJSON CreateCollectdTimeSeriesRequest where
        toJSON CreateCollectdTimeSeriesRequest'{..}
          = object
              (catMaybes
                 [("collectdPayloads" .=) <$> _cctsrCollectdPayloads,
                  ("resource" .=) <$> _cctsrResource,
                  ("collectdVersion" .=) <$> _cctsrCollectdVersion])

-- | This message configures which resources and services to monitor for
-- availability.
--
-- /See:/ 'uptimeCheckConfig' smart constructor.
data UptimeCheckConfig = UptimeCheckConfig'
    { _uccInternalCheckers :: !(Maybe [InternalChecker])
    , _uccPeriod :: !(Maybe Duration)
    , _uccContentMatchers :: !(Maybe [ContentMatcher])
    , _uccName :: !(Maybe Text)
    , _uccMonitoredResource :: !(Maybe MonitoredResource)
    , _uccSelectedRegions :: !(Maybe [Text])
    , _uccIsInternal :: !(Maybe Bool)
    , _uccDisplayName :: !(Maybe Text)
    , _uccResourceGroup :: !(Maybe ResourceGroup)
    , _uccTimeout :: !(Maybe Duration)
    , _uccHTTPCheck :: !(Maybe HTTPCheck)
    , _uccTCPCheck :: !(Maybe TCPCheck)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UptimeCheckConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uccInternalCheckers'
--
-- * 'uccPeriod'
--
-- * 'uccContentMatchers'
--
-- * 'uccName'
--
-- * 'uccMonitoredResource'
--
-- * 'uccSelectedRegions'
--
-- * 'uccIsInternal'
--
-- * 'uccDisplayName'
--
-- * 'uccResourceGroup'
--
-- * 'uccTimeout'
--
-- * 'uccHTTPCheck'
--
-- * 'uccTCPCheck'
uptimeCheckConfig
    :: UptimeCheckConfig
uptimeCheckConfig = 
    UptimeCheckConfig'
    { _uccInternalCheckers = Nothing
    , _uccPeriod = Nothing
    , _uccContentMatchers = Nothing
    , _uccName = Nothing
    , _uccMonitoredResource = Nothing
    , _uccSelectedRegions = Nothing
    , _uccIsInternal = Nothing
    , _uccDisplayName = Nothing
    , _uccResourceGroup = Nothing
    , _uccTimeout = Nothing
    , _uccHTTPCheck = Nothing
    , _uccTCPCheck = Nothing
    }

-- | The internal checkers that this check will egress from. If is_internal
-- is true and this list is empty, the check will egress from all
-- InternalCheckers configured for the project that owns this CheckConfig.
uccInternalCheckers :: Lens' UptimeCheckConfig [InternalChecker]
uccInternalCheckers
  = lens _uccInternalCheckers
      (\ s a -> s{_uccInternalCheckers = a})
      . _Default
      . _Coerce

-- | How often the uptime check is performed. Currently, only 1, 5, 10, and
-- 15 minutes are supported. Required.
uccPeriod :: Lens' UptimeCheckConfig (Maybe Scientific)
uccPeriod
  = lens _uccPeriod (\ s a -> s{_uccPeriod = a}) .
      mapping _Duration

-- | The expected content on the page the check is run against. Currently,
-- only the first entry in the list is supported, and other entries will be
-- ignored. The server will look for an exact match of the string in the
-- page response\'s content. This field is optional and should only be
-- specified if a content match is required.
uccContentMatchers :: Lens' UptimeCheckConfig [ContentMatcher]
uccContentMatchers
  = lens _uccContentMatchers
      (\ s a -> s{_uccContentMatchers = a})
      . _Default
      . _Coerce

-- | A unique resource name for this UptimeCheckConfig. The format
-- is:projects\/[PROJECT_ID]\/uptimeCheckConfigs\/[UPTIME_CHECK_ID].This
-- field should be omitted when creating the uptime check configuration; on
-- create, the resource name is assigned by the server and included in the
-- response.
uccName :: Lens' UptimeCheckConfig (Maybe Text)
uccName = lens _uccName (\ s a -> s{_uccName = a})

-- | The monitored resource associated with the configuration.
uccMonitoredResource :: Lens' UptimeCheckConfig (Maybe MonitoredResource)
uccMonitoredResource
  = lens _uccMonitoredResource
      (\ s a -> s{_uccMonitoredResource = a})

-- | The list of regions from which the check will be run. If this field is
-- specified, enough regions to include a minimum of 3 locations must be
-- provided, or an error message is returned. Not specifying this field
-- will result in uptime checks running from all regions.
uccSelectedRegions :: Lens' UptimeCheckConfig [Text]
uccSelectedRegions
  = lens _uccSelectedRegions
      (\ s a -> s{_uccSelectedRegions = a})
      . _Default
      . _Coerce

-- | Denotes whether this check is a check that egresses from
-- InternalCheckers.
uccIsInternal :: Lens' UptimeCheckConfig (Maybe Bool)
uccIsInternal
  = lens _uccIsInternal
      (\ s a -> s{_uccIsInternal = a})

-- | A human-friendly name for the uptime check configuration. The display
-- name should be unique within a Stackdriver Account in order to make it
-- easier to identify; however, uniqueness is not enforced. Required.
uccDisplayName :: Lens' UptimeCheckConfig (Maybe Text)
uccDisplayName
  = lens _uccDisplayName
      (\ s a -> s{_uccDisplayName = a})

-- | The group resource associated with the configuration.
uccResourceGroup :: Lens' UptimeCheckConfig (Maybe ResourceGroup)
uccResourceGroup
  = lens _uccResourceGroup
      (\ s a -> s{_uccResourceGroup = a})

-- | The maximum amount of time to wait for the request to complete (must be
-- between 1 and 60 seconds). Required.
uccTimeout :: Lens' UptimeCheckConfig (Maybe Scientific)
uccTimeout
  = lens _uccTimeout (\ s a -> s{_uccTimeout = a}) .
      mapping _Duration

-- | Contains information needed to make an HTTP or HTTPS check.
uccHTTPCheck :: Lens' UptimeCheckConfig (Maybe HTTPCheck)
uccHTTPCheck
  = lens _uccHTTPCheck (\ s a -> s{_uccHTTPCheck = a})

-- | Contains information needed to make a TCP check.
uccTCPCheck :: Lens' UptimeCheckConfig (Maybe TCPCheck)
uccTCPCheck
  = lens _uccTCPCheck (\ s a -> s{_uccTCPCheck = a})

instance FromJSON UptimeCheckConfig where
        parseJSON
          = withObject "UptimeCheckConfig"
              (\ o ->
                 UptimeCheckConfig' <$>
                   (o .:? "internalCheckers" .!= mempty) <*>
                     (o .:? "period")
                     <*> (o .:? "contentMatchers" .!= mempty)
                     <*> (o .:? "name")
                     <*> (o .:? "monitoredResource")
                     <*> (o .:? "selectedRegions" .!= mempty)
                     <*> (o .:? "isInternal")
                     <*> (o .:? "displayName")
                     <*> (o .:? "resourceGroup")
                     <*> (o .:? "timeout")
                     <*> (o .:? "httpCheck")
                     <*> (o .:? "tcpCheck"))

instance ToJSON UptimeCheckConfig where
        toJSON UptimeCheckConfig'{..}
          = object
              (catMaybes
                 [("internalCheckers" .=) <$> _uccInternalCheckers,
                  ("period" .=) <$> _uccPeriod,
                  ("contentMatchers" .=) <$> _uccContentMatchers,
                  ("name" .=) <$> _uccName,
                  ("monitoredResource" .=) <$> _uccMonitoredResource,
                  ("selectedRegions" .=) <$> _uccSelectedRegions,
                  ("isInternal" .=) <$> _uccIsInternal,
                  ("displayName" .=) <$> _uccDisplayName,
                  ("resourceGroup" .=) <$> _uccResourceGroup,
                  ("timeout" .=) <$> _uccTimeout,
                  ("httpCheck" .=) <$> _uccHTTPCheck,
                  ("tcpCheck" .=) <$> _uccTCPCheck])

-- | A single data point in a time series.
--
-- /See:/ 'point' smart constructor.
data Point = Point'
    { _pValue :: !(Maybe TypedValue)
    , _pInterval :: !(Maybe TimeInterval)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Point' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pValue'
--
-- * 'pInterval'
point
    :: Point
point = 
    Point'
    { _pValue = Nothing
    , _pInterval = Nothing
    }

-- | The value of the data point.
pValue :: Lens' Point (Maybe TypedValue)
pValue = lens _pValue (\ s a -> s{_pValue = a})

-- | The time interval to which the data point applies. For GAUGE metrics,
-- only the end time of the interval is used. For DELTA metrics, the start
-- and end time should specify a non-zero interval, with subsequent points
-- specifying contiguous and non-overlapping intervals. For CUMULATIVE
-- metrics, the start and end time should specify a non-zero interval, with
-- subsequent points specifying the same start time and increasing end
-- times, until an event resets the cumulative value to zero and sets a new
-- start time for the following points.
pInterval :: Lens' Point (Maybe TimeInterval)
pInterval
  = lens _pInterval (\ s a -> s{_pInterval = a})

instance FromJSON Point where
        parseJSON
          = withObject "Point"
              (\ o ->
                 Point' <$> (o .:? "value") <*> (o .:? "interval"))

instance ToJSON Point where
        toJSON Point'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _pValue,
                  ("interval" .=) <$> _pInterval])

-- | A collection of data points sent from a collectd-based plugin. See the
-- collectd documentation for more information.
--
-- /See:/ 'collectdPayload' smart constructor.
data CollectdPayload = CollectdPayload'
    { _cpStartTime :: !(Maybe DateTime')
    , _cpPluginInstance :: !(Maybe Text)
    , _cpValues :: !(Maybe [CollectdValue])
    , _cpTypeInstance :: !(Maybe Text)
    , _cpEndTime :: !(Maybe DateTime')
    , _cpMetadata :: !(Maybe CollectdPayloadMetadata)
    , _cpType :: !(Maybe Text)
    , _cpPlugin :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CollectdPayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpStartTime'
--
-- * 'cpPluginInstance'
--
-- * 'cpValues'
--
-- * 'cpTypeInstance'
--
-- * 'cpEndTime'
--
-- * 'cpMetadata'
--
-- * 'cpType'
--
-- * 'cpPlugin'
collectdPayload
    :: CollectdPayload
collectdPayload = 
    CollectdPayload'
    { _cpStartTime = Nothing
    , _cpPluginInstance = Nothing
    , _cpValues = Nothing
    , _cpTypeInstance = Nothing
    , _cpEndTime = Nothing
    , _cpMetadata = Nothing
    , _cpType = Nothing
    , _cpPlugin = Nothing
    }

-- | The start time of the interval.
cpStartTime :: Lens' CollectdPayload (Maybe UTCTime)
cpStartTime
  = lens _cpStartTime (\ s a -> s{_cpStartTime = a}) .
      mapping _DateTime

-- | The instance name of the plugin Example: \"hdcl\".
cpPluginInstance :: Lens' CollectdPayload (Maybe Text)
cpPluginInstance
  = lens _cpPluginInstance
      (\ s a -> s{_cpPluginInstance = a})

-- | The measured values during this time interval. Each value must have a
-- different dataSourceName.
cpValues :: Lens' CollectdPayload [CollectdValue]
cpValues
  = lens _cpValues (\ s a -> s{_cpValues = a}) .
      _Default
      . _Coerce

-- | The measurement type instance. Example: \"used\".
cpTypeInstance :: Lens' CollectdPayload (Maybe Text)
cpTypeInstance
  = lens _cpTypeInstance
      (\ s a -> s{_cpTypeInstance = a})

-- | The end time of the interval.
cpEndTime :: Lens' CollectdPayload (Maybe UTCTime)
cpEndTime
  = lens _cpEndTime (\ s a -> s{_cpEndTime = a}) .
      mapping _DateTime

-- | The measurement metadata. Example: \"process_id\" -> 12345
cpMetadata :: Lens' CollectdPayload (Maybe CollectdPayloadMetadata)
cpMetadata
  = lens _cpMetadata (\ s a -> s{_cpMetadata = a})

-- | The measurement type. Example: \"memory\".
cpType :: Lens' CollectdPayload (Maybe Text)
cpType = lens _cpType (\ s a -> s{_cpType = a})

-- | The name of the plugin. Example: \"disk\".
cpPlugin :: Lens' CollectdPayload (Maybe Text)
cpPlugin = lens _cpPlugin (\ s a -> s{_cpPlugin = a})

instance FromJSON CollectdPayload where
        parseJSON
          = withObject "CollectdPayload"
              (\ o ->
                 CollectdPayload' <$>
                   (o .:? "startTime") <*> (o .:? "pluginInstance") <*>
                     (o .:? "values" .!= mempty)
                     <*> (o .:? "typeInstance")
                     <*> (o .:? "endTime")
                     <*> (o .:? "metadata")
                     <*> (o .:? "type")
                     <*> (o .:? "plugin"))

instance ToJSON CollectdPayload where
        toJSON CollectdPayload'{..}
          = object
              (catMaybes
                 [("startTime" .=) <$> _cpStartTime,
                  ("pluginInstance" .=) <$> _cpPluginInstance,
                  ("values" .=) <$> _cpValues,
                  ("typeInstance" .=) <$> _cpTypeInstance,
                  ("endTime" .=) <$> _cpEndTime,
                  ("metadata" .=) <$> _cpMetadata,
                  ("type" .=) <$> _cpType,
                  ("plugin" .=) <$> _cpPlugin])

-- | A specific metric, identified by specifying values for all of the labels
-- of a MetricDescriptor.
--
-- /See:/ 'metric' smart constructor.
data Metric = Metric'
    { _mLabels :: !(Maybe MetricLabels)
    , _mType :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Metric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mLabels'
--
-- * 'mType'
metric
    :: Metric
metric = 
    Metric'
    { _mLabels = Nothing
    , _mType = Nothing
    }

-- | The set of label values that uniquely identify this metric. All labels
-- listed in the MetricDescriptor must be assigned values.
mLabels :: Lens' Metric (Maybe MetricLabels)
mLabels = lens _mLabels (\ s a -> s{_mLabels = a})

-- | An existing metric type, see google.api.MetricDescriptor. For example,
-- custom.googleapis.com\/invoice\/paid\/amount.
mType :: Lens' Metric (Maybe Text)
mType = lens _mType (\ s a -> s{_mType = a})

instance FromJSON Metric where
        parseJSON
          = withObject "Metric"
              (\ o ->
                 Metric' <$> (o .:? "labels") <*> (o .:? "type"))

instance ToJSON Metric where
        toJSON Metric'{..}
          = object
              (catMaybes
                 [("labels" .=) <$> _mLabels, ("type" .=) <$> _mType])

-- | Describes the error status for payloads that were not written.
--
-- /See:/ 'collectdPayloadError' smart constructor.
data CollectdPayloadError = CollectdPayloadError'
    { _cpeError :: !(Maybe Status)
    , _cpeValueErrors :: !(Maybe [CollectdValueError])
    , _cpeIndex :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CollectdPayloadError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpeError'
--
-- * 'cpeValueErrors'
--
-- * 'cpeIndex'
collectdPayloadError
    :: CollectdPayloadError
collectdPayloadError = 
    CollectdPayloadError'
    { _cpeError = Nothing
    , _cpeValueErrors = Nothing
    , _cpeIndex = Nothing
    }

-- | Records the error status for the payload. If this field is present, the
-- partial errors for nested values won\'t be populated.
cpeError :: Lens' CollectdPayloadError (Maybe Status)
cpeError = lens _cpeError (\ s a -> s{_cpeError = a})

-- | Records the error status for values that were not written due to an
-- error.Failed payloads for which nothing is written will not include
-- partial value errors.
cpeValueErrors :: Lens' CollectdPayloadError [CollectdValueError]
cpeValueErrors
  = lens _cpeValueErrors
      (\ s a -> s{_cpeValueErrors = a})
      . _Default
      . _Coerce

-- | The zero-based index in
-- CreateCollectdTimeSeriesRequest.collectd_payloads.
cpeIndex :: Lens' CollectdPayloadError (Maybe Int32)
cpeIndex
  = lens _cpeIndex (\ s a -> s{_cpeIndex = a}) .
      mapping _Coerce

instance FromJSON CollectdPayloadError where
        parseJSON
          = withObject "CollectdPayloadError"
              (\ o ->
                 CollectdPayloadError' <$>
                   (o .:? "error") <*> (o .:? "valueErrors" .!= mempty)
                     <*> (o .:? "index"))

instance ToJSON CollectdPayloadError where
        toJSON CollectdPayloadError'{..}
          = object
              (catMaybes
                 [("error" .=) <$> _cpeError,
                  ("valueErrors" .=) <$> _cpeValueErrors,
                  ("index" .=) <$> _cpeIndex])

-- | Specifies an exponential sequence of buckets that have a width that is
-- proportional to the value of the lower bound. Each bucket represents a
-- constant relative uncertainty on a specific value in the bucket.There
-- are num_finite_buckets + 2 (= N) buckets. Bucket i has the following
-- boundaries:Upper bound (0 \<= i \< N-1): scale * (growth_factor ^ i).
-- Lower bound (1 \<= i \< N): scale * (growth_factor ^ (i - 1)).
--
-- /See:/ 'exponential' smart constructor.
data Exponential = Exponential'
    { _eGrowthFactor :: !(Maybe (Textual Double))
    , _eScale :: !(Maybe (Textual Double))
    , _eNumFiniteBuckets :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Exponential' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eGrowthFactor'
--
-- * 'eScale'
--
-- * 'eNumFiniteBuckets'
exponential
    :: Exponential
exponential = 
    Exponential'
    { _eGrowthFactor = Nothing
    , _eScale = Nothing
    , _eNumFiniteBuckets = Nothing
    }

-- | Must be greater than 1.
eGrowthFactor :: Lens' Exponential (Maybe Double)
eGrowthFactor
  = lens _eGrowthFactor
      (\ s a -> s{_eGrowthFactor = a})
      . mapping _Coerce

-- | Must be greater than 0.
eScale :: Lens' Exponential (Maybe Double)
eScale
  = lens _eScale (\ s a -> s{_eScale = a}) .
      mapping _Coerce

-- | Must be greater than 0.
eNumFiniteBuckets :: Lens' Exponential (Maybe Int32)
eNumFiniteBuckets
  = lens _eNumFiniteBuckets
      (\ s a -> s{_eNumFiniteBuckets = a})
      . mapping _Coerce

instance FromJSON Exponential where
        parseJSON
          = withObject "Exponential"
              (\ o ->
                 Exponential' <$>
                   (o .:? "growthFactor") <*> (o .:? "scale") <*>
                     (o .:? "numFiniteBuckets"))

instance ToJSON Exponential where
        toJSON Exponential'{..}
          = object
              (catMaybes
                 [("growthFactor" .=) <$> _eGrowthFactor,
                  ("scale" .=) <$> _eScale,
                  ("numFiniteBuckets" .=) <$> _eNumFiniteBuckets])

-- | The range of the population values.
--
-- /See:/ 'range' smart constructor.
data Range = Range'
    { _rMax :: !(Maybe (Textual Double))
    , _rMin :: !(Maybe (Textual Double))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Range' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rMax'
--
-- * 'rMin'
range
    :: Range
range = 
    Range'
    { _rMax = Nothing
    , _rMin = Nothing
    }

-- | The maximum of the population values.
rMax :: Lens' Range (Maybe Double)
rMax
  = lens _rMax (\ s a -> s{_rMax = a}) .
      mapping _Coerce

-- | The minimum of the population values.
rMin :: Lens' Range (Maybe Double)
rMin
  = lens _rMin (\ s a -> s{_rMin = a}) .
      mapping _Coerce

instance FromJSON Range where
        parseJSON
          = withObject "Range"
              (\ o -> Range' <$> (o .:? "max") <*> (o .:? "min"))

instance ToJSON Range where
        toJSON Range'{..}
          = object
              (catMaybes
                 [("max" .=) <$> _rMax, ("min" .=) <$> _rMin])

-- | An object representing a resource that can be used for monitoring,
-- logging, billing, or other purposes. Examples include virtual machine
-- instances, databases, and storage devices such as disks. The type field
-- identifies a MonitoredResourceDescriptor object that describes the
-- resource\'s schema. Information in the labels field identifies the
-- actual resource and its attributes according to the schema. For example,
-- a particular Compute Engine VM instance could be represented by the
-- following object, because the MonitoredResourceDescriptor for
-- \"gce_instance\" has labels \"instance_id\" and \"zone\": { \"type\":
-- \"gce_instance\", \"labels\": { \"instance_id\": \"12345678901234\",
-- \"zone\": \"us-central1-a\" }}
--
-- /See:/ 'monitoredResource' smart constructor.
data MonitoredResource = MonitoredResource'
    { _mrLabels :: !(Maybe MonitoredResourceLabels)
    , _mrType :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'MonitoredResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrLabels'
--
-- * 'mrType'
monitoredResource
    :: MonitoredResource
monitoredResource = 
    MonitoredResource'
    { _mrLabels = Nothing
    , _mrType = Nothing
    }

-- | Required. Values for all of the labels listed in the associated
-- monitored resource descriptor. For example, Compute Engine VM instances
-- use the labels \"project_id\", \"instance_id\", and \"zone\".
mrLabels :: Lens' MonitoredResource (Maybe MonitoredResourceLabels)
mrLabels = lens _mrLabels (\ s a -> s{_mrLabels = a})

-- | Required. The monitored resource type. This field must match the type
-- field of a MonitoredResourceDescriptor object. For example, the type of
-- a Compute Engine VM instance is gce_instance.
mrType :: Lens' MonitoredResource (Maybe Text)
mrType = lens _mrType (\ s a -> s{_mrType = a})

instance FromJSON MonitoredResource where
        parseJSON
          = withObject "MonitoredResource"
              (\ o ->
                 MonitoredResource' <$>
                   (o .:? "labels") <*> (o .:? "type"))

instance ToJSON MonitoredResource where
        toJSON MonitoredResource'{..}
          = object
              (catMaybes
                 [("labels" .=) <$> _mrLabels,
                  ("type" .=) <$> _mrType])

-- | Contains the region, location, and list of IP addresses where checkers
-- in the location run from.
--
-- /See:/ 'uptimeCheckIP' smart constructor.
data UptimeCheckIP = UptimeCheckIP'
    { _uciIPAddress :: !(Maybe Text)
    , _uciLocation :: !(Maybe Text)
    , _uciRegion :: !(Maybe UptimeCheckIPRegion)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UptimeCheckIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uciIPAddress'
--
-- * 'uciLocation'
--
-- * 'uciRegion'
uptimeCheckIP
    :: UptimeCheckIP
uptimeCheckIP = 
    UptimeCheckIP'
    { _uciIPAddress = Nothing
    , _uciLocation = Nothing
    , _uciRegion = Nothing
    }

-- | The IP address from which the uptime check originates. This is a full IP
-- address (not an IP address range). Most IP addresses, as of this
-- publication, are in IPv4 format; however, one should not rely on the IP
-- addresses being in IPv4 format indefinitely and should support
-- interpreting this field in either IPv4 or IPv6 format.
uciIPAddress :: Lens' UptimeCheckIP (Maybe Text)
uciIPAddress
  = lens _uciIPAddress (\ s a -> s{_uciIPAddress = a})

-- | A more specific location within the region that typically encodes a
-- particular city\/town\/metro (and its containing state\/province or
-- country) within the broader umbrella region category.
uciLocation :: Lens' UptimeCheckIP (Maybe Text)
uciLocation
  = lens _uciLocation (\ s a -> s{_uciLocation = a})

-- | A broad region category in which the IP address is located.
uciRegion :: Lens' UptimeCheckIP (Maybe UptimeCheckIPRegion)
uciRegion
  = lens _uciRegion (\ s a -> s{_uciRegion = a})

instance FromJSON UptimeCheckIP where
        parseJSON
          = withObject "UptimeCheckIP"
              (\ o ->
                 UptimeCheckIP' <$>
                   (o .:? "ipAddress") <*> (o .:? "location") <*>
                     (o .:? "region"))

instance ToJSON UptimeCheckIP where
        toJSON UptimeCheckIP'{..}
          = object
              (catMaybes
                 [("ipAddress" .=) <$> _uciIPAddress,
                  ("location" .=) <$> _uciLocation,
                  ("region" .=) <$> _uciRegion])

-- | A time interval extending just after a start time through an end time.
-- If the start time is the same as the end time, then the interval
-- represents a single point in time.
--
-- /See:/ 'timeInterval' smart constructor.
data TimeInterval = TimeInterval'
    { _tiStartTime :: !(Maybe DateTime')
    , _tiEndTime :: !(Maybe DateTime')
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TimeInterval' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiStartTime'
--
-- * 'tiEndTime'
timeInterval
    :: TimeInterval
timeInterval = 
    TimeInterval'
    { _tiStartTime = Nothing
    , _tiEndTime = Nothing
    }

-- | Optional. The beginning of the time interval. The default value for the
-- start time is the end time. The start time must not be later than the
-- end time.
tiStartTime :: Lens' TimeInterval (Maybe UTCTime)
tiStartTime
  = lens _tiStartTime (\ s a -> s{_tiStartTime = a}) .
      mapping _DateTime

-- | Required. The end of the time interval.
tiEndTime :: Lens' TimeInterval (Maybe UTCTime)
tiEndTime
  = lens _tiEndTime (\ s a -> s{_tiEndTime = a}) .
      mapping _DateTime

instance FromJSON TimeInterval where
        parseJSON
          = withObject "TimeInterval"
              (\ o ->
                 TimeInterval' <$>
                   (o .:? "startTime") <*> (o .:? "endTime"))

instance ToJSON TimeInterval where
        toJSON TimeInterval'{..}
          = object
              (catMaybes
                 [("startTime" .=) <$> _tiStartTime,
                  ("endTime" .=) <$> _tiEndTime])

-- | The list of headers to send as part of the uptime check request. If two
-- headers have the same key and different values, they should be entered
-- as a single header, with the value being a comma-separated list of all
-- the desired values as described at
-- https:\/\/www.w3.org\/Protocols\/rfc2616\/rfc2616.txt (page 31).
-- Entering two separate headers with the same key in a Create call will
-- cause the first to be overwritten by the second.
--
-- /See:/ 'hTTPCheckHeaders' smart constructor.
newtype HTTPCheckHeaders = HTTPCheckHeaders'
    { _httpchAddtional :: HashMap Text Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'HTTPCheckHeaders' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpchAddtional'
hTTPCheckHeaders
    :: HashMap Text Text -- ^ 'httpchAddtional'
    -> HTTPCheckHeaders
hTTPCheckHeaders pHttpchAddtional_ = 
    HTTPCheckHeaders'
    { _httpchAddtional = _Coerce # pHttpchAddtional_
    }

httpchAddtional :: Lens' HTTPCheckHeaders (HashMap Text Text)
httpchAddtional
  = lens _httpchAddtional
      (\ s a -> s{_httpchAddtional = a})
      . _Coerce

instance FromJSON HTTPCheckHeaders where
        parseJSON
          = withObject "HTTPCheckHeaders"
              (\ o -> HTTPCheckHeaders' <$> (parseJSONObject o))

instance ToJSON HTTPCheckHeaders where
        toJSON = toJSON . _httpchAddtional

-- | Used to perform string matching. Currently, this matches on the exact
-- content. In the future, it can be expanded to allow for regular
-- expressions and more complex matching.
--
-- /See:/ 'contentMatcher' smart constructor.
newtype ContentMatcher = ContentMatcher'
    { _cmContent :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContentMatcher' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmContent'
contentMatcher
    :: ContentMatcher
contentMatcher = 
    ContentMatcher'
    { _cmContent = Nothing
    }

-- | String content to match
cmContent :: Lens' ContentMatcher (Maybe Text)
cmContent
  = lens _cmContent (\ s a -> s{_cmContent = a})

instance FromJSON ContentMatcher where
        parseJSON
          = withObject "ContentMatcher"
              (\ o -> ContentMatcher' <$> (o .:? "content"))

instance ToJSON ContentMatcher where
        toJSON ContentMatcher'{..}
          = object (catMaybes [("content" .=) <$> _cmContent])

-- | The ListGroupMembers response.
--
-- /See:/ 'listGroupMembersResponse' smart constructor.
data ListGroupMembersResponse = ListGroupMembersResponse'
    { _lgmrNextPageToken :: !(Maybe Text)
    , _lgmrMembers :: !(Maybe [MonitoredResource])
    , _lgmrTotalSize :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGroupMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgmrNextPageToken'
--
-- * 'lgmrMembers'
--
-- * 'lgmrTotalSize'
listGroupMembersResponse
    :: ListGroupMembersResponse
listGroupMembersResponse = 
    ListGroupMembersResponse'
    { _lgmrNextPageToken = Nothing
    , _lgmrMembers = Nothing
    , _lgmrTotalSize = Nothing
    }

-- | If there are more results than have been returned, then this field is
-- set to a non-empty value. To see the additional results, use that value
-- as pageToken in the next call to this method.
lgmrNextPageToken :: Lens' ListGroupMembersResponse (Maybe Text)
lgmrNextPageToken
  = lens _lgmrNextPageToken
      (\ s a -> s{_lgmrNextPageToken = a})

-- | A set of monitored resources in the group.
lgmrMembers :: Lens' ListGroupMembersResponse [MonitoredResource]
lgmrMembers
  = lens _lgmrMembers (\ s a -> s{_lgmrMembers = a}) .
      _Default
      . _Coerce

-- | The total number of elements matching this request.
lgmrTotalSize :: Lens' ListGroupMembersResponse (Maybe Int32)
lgmrTotalSize
  = lens _lgmrTotalSize
      (\ s a -> s{_lgmrTotalSize = a})
      . mapping _Coerce

instance FromJSON ListGroupMembersResponse where
        parseJSON
          = withObject "ListGroupMembersResponse"
              (\ o ->
                 ListGroupMembersResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "members" .!= mempty)
                     <*> (o .:? "totalSize"))

instance ToJSON ListGroupMembersResponse where
        toJSON ListGroupMembersResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lgmrNextPageToken,
                  ("members" .=) <$> _lgmrMembers,
                  ("totalSize" .=) <$> _lgmrTotalSize])

-- | A description of a label.
--
-- /See:/ 'labelDescriptor' smart constructor.
data LabelDescriptor = LabelDescriptor'
    { _ldKey :: !(Maybe Text)
    , _ldValueType :: !(Maybe LabelDescriptorValueType)
    , _ldDescription :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'LabelDescriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldKey'
--
-- * 'ldValueType'
--
-- * 'ldDescription'
labelDescriptor
    :: LabelDescriptor
labelDescriptor = 
    LabelDescriptor'
    { _ldKey = Nothing
    , _ldValueType = Nothing
    , _ldDescription = Nothing
    }

-- | The label key.
ldKey :: Lens' LabelDescriptor (Maybe Text)
ldKey = lens _ldKey (\ s a -> s{_ldKey = a})

-- | The type of data that can be assigned to the label.
ldValueType :: Lens' LabelDescriptor (Maybe LabelDescriptorValueType)
ldValueType
  = lens _ldValueType (\ s a -> s{_ldValueType = a})

-- | A human-readable description for the label.
ldDescription :: Lens' LabelDescriptor (Maybe Text)
ldDescription
  = lens _ldDescription
      (\ s a -> s{_ldDescription = a})

instance FromJSON LabelDescriptor where
        parseJSON
          = withObject "LabelDescriptor"
              (\ o ->
                 LabelDescriptor' <$>
                   (o .:? "key") <*> (o .:? "valueType") <*>
                     (o .:? "description"))

instance ToJSON LabelDescriptor where
        toJSON LabelDescriptor'{..}
          = object
              (catMaybes
                 [("key" .=) <$> _ldKey,
                  ("valueType" .=) <$> _ldValueType,
                  ("description" .=) <$> _ldDescription])

-- | Specifies a linear sequence of buckets that all have the same width
-- (except overflow and underflow). Each bucket represents a constant
-- absolute uncertainty on the specific value in the bucket.There are
-- num_finite_buckets + 2 (= N) buckets. Bucket i has the following
-- boundaries:Upper bound (0 \<= i \< N-1): offset + (width * i). Lower
-- bound (1 \<= i \< N): offset + (width * (i - 1)).
--
-- /See:/ 'linear' smart constructor.
data Linear = Linear'
    { _lOffSet :: !(Maybe (Textual Double))
    , _lWidth :: !(Maybe (Textual Double))
    , _lNumFiniteBuckets :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Linear' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lOffSet'
--
-- * 'lWidth'
--
-- * 'lNumFiniteBuckets'
linear
    :: Linear
linear = 
    Linear'
    { _lOffSet = Nothing
    , _lWidth = Nothing
    , _lNumFiniteBuckets = Nothing
    }

-- | Lower bound of the first bucket.
lOffSet :: Lens' Linear (Maybe Double)
lOffSet
  = lens _lOffSet (\ s a -> s{_lOffSet = a}) .
      mapping _Coerce

-- | Must be greater than 0.
lWidth :: Lens' Linear (Maybe Double)
lWidth
  = lens _lWidth (\ s a -> s{_lWidth = a}) .
      mapping _Coerce

-- | Must be greater than 0.
lNumFiniteBuckets :: Lens' Linear (Maybe Int32)
lNumFiniteBuckets
  = lens _lNumFiniteBuckets
      (\ s a -> s{_lNumFiniteBuckets = a})
      . mapping _Coerce

instance FromJSON Linear where
        parseJSON
          = withObject "Linear"
              (\ o ->
                 Linear' <$>
                   (o .:? "offset") <*> (o .:? "width") <*>
                     (o .:? "numFiniteBuckets"))

instance ToJSON Linear where
        toJSON Linear'{..}
          = object
              (catMaybes
                 [("offset" .=) <$> _lOffSet,
                  ("width" .=) <$> _lWidth,
                  ("numFiniteBuckets" .=) <$> _lNumFiniteBuckets])

-- | The protocol for the ListUptimeCheckIps response.
--
-- /See:/ 'listUptimeCheckIPsResponse' smart constructor.
data ListUptimeCheckIPsResponse = ListUptimeCheckIPsResponse'
    { _lucirNextPageToken :: !(Maybe Text)
    , _lucirUptimeCheckIPs :: !(Maybe [UptimeCheckIP])
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListUptimeCheckIPsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lucirNextPageToken'
--
-- * 'lucirUptimeCheckIPs'
listUptimeCheckIPsResponse
    :: ListUptimeCheckIPsResponse
listUptimeCheckIPsResponse = 
    ListUptimeCheckIPsResponse'
    { _lucirNextPageToken = Nothing
    , _lucirUptimeCheckIPs = Nothing
    }

-- | This field represents the pagination token to retrieve the next page of
-- results. If the value is empty, it means no further results for the
-- request. To retrieve the next page of results, the value of the
-- next_page_token is passed to the subsequent List method call (in the
-- request message\'s page_token field). NOTE: this field is not yet
-- implemented
lucirNextPageToken :: Lens' ListUptimeCheckIPsResponse (Maybe Text)
lucirNextPageToken
  = lens _lucirNextPageToken
      (\ s a -> s{_lucirNextPageToken = a})

-- | The returned list of IP addresses (including region and location) that
-- the checkers run from.
lucirUptimeCheckIPs :: Lens' ListUptimeCheckIPsResponse [UptimeCheckIP]
lucirUptimeCheckIPs
  = lens _lucirUptimeCheckIPs
      (\ s a -> s{_lucirUptimeCheckIPs = a})
      . _Default
      . _Coerce

instance FromJSON ListUptimeCheckIPsResponse where
        parseJSON
          = withObject "ListUptimeCheckIPsResponse"
              (\ o ->
                 ListUptimeCheckIPsResponse' <$>
                   (o .:? "nextPageToken") <*>
                     (o .:? "uptimeCheckIps" .!= mempty))

instance ToJSON ListUptimeCheckIPsResponse where
        toJSON ListUptimeCheckIPsResponse'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lucirNextPageToken,
                  ("uptimeCheckIps" .=) <$> _lucirUptimeCheckIPs])

-- | The resource submessage for group checks. It can be used instead of a
-- monitored resource, when multiple resources are being monitored.
--
-- /See:/ 'resourceGroup' smart constructor.
data ResourceGroup = ResourceGroup'
    { _rgResourceType :: !(Maybe ResourceGroupResourceType)
    , _rgGroupId :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgResourceType'
--
-- * 'rgGroupId'
resourceGroup
    :: ResourceGroup
resourceGroup = 
    ResourceGroup'
    { _rgResourceType = Nothing
    , _rgGroupId = Nothing
    }

-- | The resource type of the group members.
rgResourceType :: Lens' ResourceGroup (Maybe ResourceGroupResourceType)
rgResourceType
  = lens _rgResourceType
      (\ s a -> s{_rgResourceType = a})

-- | The group of resources being monitored. Should be only the group_id, not
-- projects\/\/groups\/.
rgGroupId :: Lens' ResourceGroup (Maybe Text)
rgGroupId
  = lens _rgGroupId (\ s a -> s{_rgGroupId = a})

instance FromJSON ResourceGroup where
        parseJSON
          = withObject "ResourceGroup"
              (\ o ->
                 ResourceGroup' <$>
                   (o .:? "resourceType") <*> (o .:? "groupId"))

instance ToJSON ResourceGroup where
        toJSON ResourceGroup'{..}
          = object
              (catMaybes
                 [("resourceType" .=) <$> _rgResourceType,
                  ("groupId" .=) <$> _rgGroupId])

-- | A protocol buffer message type.
--
-- /See:/ 'type'' smart constructor.
data Type = Type'
    { _tSourceContext :: !(Maybe SourceContext)
    , _tOneofs :: !(Maybe [Text])
    , _tName :: !(Maybe Text)
    , _tOptions :: !(Maybe [Option])
    , _tFields :: !(Maybe [Field])
    , _tSyntax :: !(Maybe TypeSyntax)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Type' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tSourceContext'
--
-- * 'tOneofs'
--
-- * 'tName'
--
-- * 'tOptions'
--
-- * 'tFields'
--
-- * 'tSyntax'
type'
    :: Type
type' = 
    Type'
    { _tSourceContext = Nothing
    , _tOneofs = Nothing
    , _tName = Nothing
    , _tOptions = Nothing
    , _tFields = Nothing
    , _tSyntax = Nothing
    }

-- | The source context.
tSourceContext :: Lens' Type (Maybe SourceContext)
tSourceContext
  = lens _tSourceContext
      (\ s a -> s{_tSourceContext = a})

-- | The list of types appearing in oneof definitions in this type.
tOneofs :: Lens' Type [Text]
tOneofs
  = lens _tOneofs (\ s a -> s{_tOneofs = a}) . _Default
      . _Coerce

-- | The fully qualified message name.
tName :: Lens' Type (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a})

-- | The protocol buffer options.
tOptions :: Lens' Type [Option]
tOptions
  = lens _tOptions (\ s a -> s{_tOptions = a}) .
      _Default
      . _Coerce

-- | The list of fields.
tFields :: Lens' Type [Field]
tFields
  = lens _tFields (\ s a -> s{_tFields = a}) . _Default
      . _Coerce

-- | The source syntax.
tSyntax :: Lens' Type (Maybe TypeSyntax)
tSyntax = lens _tSyntax (\ s a -> s{_tSyntax = a})

instance FromJSON Type where
        parseJSON
          = withObject "Type"
              (\ o ->
                 Type' <$>
                   (o .:? "sourceContext") <*>
                     (o .:? "oneofs" .!= mempty)
                     <*> (o .:? "name")
                     <*> (o .:? "options" .!= mempty)
                     <*> (o .:? "fields" .!= mempty)
                     <*> (o .:? "syntax"))

instance ToJSON Type where
        toJSON Type'{..}
          = object
              (catMaybes
                 [("sourceContext" .=) <$> _tSourceContext,
                  ("oneofs" .=) <$> _tOneofs, ("name" .=) <$> _tName,
                  ("options" .=) <$> _tOptions,
                  ("fields" .=) <$> _tFields,
                  ("syntax" .=) <$> _tSyntax])

-- | The CreateCollectdTimeSeries response.
--
-- /See:/ 'createCollectdTimeSeriesResponse' smart constructor.
newtype CreateCollectdTimeSeriesResponse = CreateCollectdTimeSeriesResponse'
    { _cctsrPayloadErrors :: Maybe [CollectdPayloadError]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCollectdTimeSeriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cctsrPayloadErrors'
createCollectdTimeSeriesResponse
    :: CreateCollectdTimeSeriesResponse
createCollectdTimeSeriesResponse = 
    CreateCollectdTimeSeriesResponse'
    { _cctsrPayloadErrors = Nothing
    }

-- | Records the error status for points that were not written due to an
-- error.Failed requests for which nothing is written will return an error
-- response instead.
cctsrPayloadErrors :: Lens' CreateCollectdTimeSeriesResponse [CollectdPayloadError]
cctsrPayloadErrors
  = lens _cctsrPayloadErrors
      (\ s a -> s{_cctsrPayloadErrors = a})
      . _Default
      . _Coerce

instance FromJSON CreateCollectdTimeSeriesResponse
         where
        parseJSON
          = withObject "CreateCollectdTimeSeriesResponse"
              (\ o ->
                 CreateCollectdTimeSeriesResponse' <$>
                   (o .:? "payloadErrors" .!= mempty))

instance ToJSON CreateCollectdTimeSeriesResponse
         where
        toJSON CreateCollectdTimeSeriesResponse'{..}
          = object
              (catMaybes
                 [("payloadErrors" .=) <$> _cctsrPayloadErrors])

-- | A protocol buffer option, which can be attached to a message, field,
-- enumeration, etc.
--
-- /See:/ 'option' smart constructor.
data Option = Option'
    { _oValue :: !(Maybe OptionValue)
    , _oName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Option' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oValue'
--
-- * 'oName'
option
    :: Option
option = 
    Option'
    { _oValue = Nothing
    , _oName = Nothing
    }

-- | The option\'s value packed in an Any message. If the value is a
-- primitive, the corresponding wrapper type defined in
-- google\/protobuf\/wrappers.proto should be used. If the value is an
-- enum, it should be stored as an int32 value using the
-- google.protobuf.Int32Value type.
oValue :: Lens' Option (Maybe OptionValue)
oValue = lens _oValue (\ s a -> s{_oValue = a})

-- | The option\'s name. For protobuf built-in options (options defined in
-- descriptor.proto), this is the short name. For example, \"map_entry\".
-- For custom options, it should be the fully-qualified name. For example,
-- \"google.api.http\".
oName :: Lens' Option (Maybe Text)
oName = lens _oName (\ s a -> s{_oName = a})

instance FromJSON Option where
        parseJSON
          = withObject "Option"
              (\ o ->
                 Option' <$> (o .:? "value") <*> (o .:? "name"))

instance ToJSON Option where
        toJSON Option'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _oValue, ("name" .=) <$> _oName])

-- | BucketOptions describes the bucket boundaries used to create a histogram
-- for the distribution. The buckets can be in a linear sequence, an
-- exponential sequence, or each bucket can be specified explicitly.
-- BucketOptions does not include the number of values in each bucket.A
-- bucket has an inclusive lower bound and exclusive upper bound for the
-- values that are counted for that bucket. The upper bound of a bucket
-- must be strictly greater than the lower bound. The sequence of N buckets
-- for a distribution consists of an underflow bucket (number 0), zero or
-- more finite buckets (number 1 through N - 2) and an overflow bucket
-- (number N - 1). The buckets are contiguous: the lower bound of bucket i
-- (i > 0) is the same as the upper bound of bucket i - 1. The buckets span
-- the whole range of finite values: lower bound of the underflow bucket is
-- -infinity and the upper bound of the overflow bucket is +infinity. The
-- finite buckets are so-called because both bounds are finite.
--
-- /See:/ 'bucketOptions' smart constructor.
data BucketOptions = BucketOptions'
    { _boExponentialBuckets :: !(Maybe Exponential)
    , _boLinearBuckets :: !(Maybe Linear)
    , _boExplicitBuckets :: !(Maybe Explicit)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'BucketOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'boExponentialBuckets'
--
-- * 'boLinearBuckets'
--
-- * 'boExplicitBuckets'
bucketOptions
    :: BucketOptions
bucketOptions = 
    BucketOptions'
    { _boExponentialBuckets = Nothing
    , _boLinearBuckets = Nothing
    , _boExplicitBuckets = Nothing
    }

-- | The exponential buckets.
boExponentialBuckets :: Lens' BucketOptions (Maybe Exponential)
boExponentialBuckets
  = lens _boExponentialBuckets
      (\ s a -> s{_boExponentialBuckets = a})

-- | The linear bucket.
boLinearBuckets :: Lens' BucketOptions (Maybe Linear)
boLinearBuckets
  = lens _boLinearBuckets
      (\ s a -> s{_boLinearBuckets = a})

-- | The explicit buckets.
boExplicitBuckets :: Lens' BucketOptions (Maybe Explicit)
boExplicitBuckets
  = lens _boExplicitBuckets
      (\ s a -> s{_boExplicitBuckets = a})

instance FromJSON BucketOptions where
        parseJSON
          = withObject "BucketOptions"
              (\ o ->
                 BucketOptions' <$>
                   (o .:? "exponentialBuckets") <*>
                     (o .:? "linearBuckets")
                     <*> (o .:? "explicitBuckets"))

instance ToJSON BucketOptions where
        toJSON BucketOptions'{..}
          = object
              (catMaybes
                 [("exponentialBuckets" .=) <$> _boExponentialBuckets,
                  ("linearBuckets" .=) <$> _boLinearBuckets,
                  ("explicitBuckets" .=) <$> _boExplicitBuckets])

-- | The protocol for the ListUptimeCheckConfigs response.
--
-- /See:/ 'listUptimeCheckConfigsResponse' smart constructor.
data ListUptimeCheckConfigsResponse = ListUptimeCheckConfigsResponse'
    { _luccrUptimeCheckConfigs :: !(Maybe [UptimeCheckConfig])
    , _luccrNextPageToken :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListUptimeCheckConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luccrUptimeCheckConfigs'
--
-- * 'luccrNextPageToken'
listUptimeCheckConfigsResponse
    :: ListUptimeCheckConfigsResponse
listUptimeCheckConfigsResponse = 
    ListUptimeCheckConfigsResponse'
    { _luccrUptimeCheckConfigs = Nothing
    , _luccrNextPageToken = Nothing
    }

-- | The returned uptime check configurations.
luccrUptimeCheckConfigs :: Lens' ListUptimeCheckConfigsResponse [UptimeCheckConfig]
luccrUptimeCheckConfigs
  = lens _luccrUptimeCheckConfigs
      (\ s a -> s{_luccrUptimeCheckConfigs = a})
      . _Default
      . _Coerce

-- | This field represents the pagination token to retrieve the next page of
-- results. If the value is empty, it means no further results for the
-- request. To retrieve the next page of results, the value of the
-- next_page_token is passed to the subsequent List method call (in the
-- request message\'s page_token field).
luccrNextPageToken :: Lens' ListUptimeCheckConfigsResponse (Maybe Text)
luccrNextPageToken
  = lens _luccrNextPageToken
      (\ s a -> s{_luccrNextPageToken = a})

instance FromJSON ListUptimeCheckConfigsResponse
         where
        parseJSON
          = withObject "ListUptimeCheckConfigsResponse"
              (\ o ->
                 ListUptimeCheckConfigsResponse' <$>
                   (o .:? "uptimeCheckConfigs" .!= mempty) <*>
                     (o .:? "nextPageToken"))

instance ToJSON ListUptimeCheckConfigsResponse where
        toJSON ListUptimeCheckConfigsResponse'{..}
          = object
              (catMaybes
                 [("uptimeCheckConfigs" .=) <$>
                    _luccrUptimeCheckConfigs,
                  ("nextPageToken" .=) <$> _luccrNextPageToken])

-- | Information involved in an HTTP\/HTTPS uptime check request.
--
-- /See:/ 'hTTPCheck' smart constructor.
data HTTPCheck = HTTPCheck'
    { _httpcUseSSL :: !(Maybe Bool)
    , _httpcPath :: !(Maybe Text)
    , _httpcMaskHeaders :: !(Maybe Bool)
    , _httpcHeaders :: !(Maybe HTTPCheckHeaders)
    , _httpcAuthInfo :: !(Maybe BasicAuthentication)
    , _httpcPort :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'HTTPCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpcUseSSL'
--
-- * 'httpcPath'
--
-- * 'httpcMaskHeaders'
--
-- * 'httpcHeaders'
--
-- * 'httpcAuthInfo'
--
-- * 'httpcPort'
hTTPCheck
    :: HTTPCheck
hTTPCheck = 
    HTTPCheck'
    { _httpcUseSSL = Nothing
    , _httpcPath = Nothing
    , _httpcMaskHeaders = Nothing
    , _httpcHeaders = Nothing
    , _httpcAuthInfo = Nothing
    , _httpcPort = Nothing
    }

-- | If true, use HTTPS instead of HTTP to run the check.
httpcUseSSL :: Lens' HTTPCheck (Maybe Bool)
httpcUseSSL
  = lens _httpcUseSSL (\ s a -> s{_httpcUseSSL = a})

-- | The path to the page to run the check against. Will be combined with the
-- host (specified within the MonitoredResource) and port to construct the
-- full URL. Optional (defaults to \"\/\").
httpcPath :: Lens' HTTPCheck (Maybe Text)
httpcPath
  = lens _httpcPath (\ s a -> s{_httpcPath = a})

-- | Boolean specifiying whether to encrypt the header information.
-- Encryption should be specified for any headers related to authentication
-- that you do not wish to be seen when retrieving the configuration. The
-- server will be responsible for encrypting the headers. On Get\/List
-- calls, if mask_headers is set to True then the headers will be obscured
-- with ******.
httpcMaskHeaders :: Lens' HTTPCheck (Maybe Bool)
httpcMaskHeaders
  = lens _httpcMaskHeaders
      (\ s a -> s{_httpcMaskHeaders = a})

-- | The list of headers to send as part of the uptime check request. If two
-- headers have the same key and different values, they should be entered
-- as a single header, with the value being a comma-separated list of all
-- the desired values as described at
-- https:\/\/www.w3.org\/Protocols\/rfc2616\/rfc2616.txt (page 31).
-- Entering two separate headers with the same key in a Create call will
-- cause the first to be overwritten by the second.
httpcHeaders :: Lens' HTTPCheck (Maybe HTTPCheckHeaders)
httpcHeaders
  = lens _httpcHeaders (\ s a -> s{_httpcHeaders = a})

-- | The authentication information. Optional when creating an HTTP check;
-- defaults to empty.
httpcAuthInfo :: Lens' HTTPCheck (Maybe BasicAuthentication)
httpcAuthInfo
  = lens _httpcAuthInfo
      (\ s a -> s{_httpcAuthInfo = a})

-- | The port to the page to run the check against. Will be combined with
-- host (specified within the MonitoredResource) and path to construct the
-- full URL. Optional (defaults to 80 without SSL, or 443 with SSL).
httpcPort :: Lens' HTTPCheck (Maybe Int32)
httpcPort
  = lens _httpcPort (\ s a -> s{_httpcPort = a}) .
      mapping _Coerce

instance FromJSON HTTPCheck where
        parseJSON
          = withObject "HTTPCheck"
              (\ o ->
                 HTTPCheck' <$>
                   (o .:? "useSsl") <*> (o .:? "path") <*>
                     (o .:? "maskHeaders")
                     <*> (o .:? "headers")
                     <*> (o .:? "authInfo")
                     <*> (o .:? "port"))

instance ToJSON HTTPCheck where
        toJSON HTTPCheck'{..}
          = object
              (catMaybes
                 [("useSsl" .=) <$> _httpcUseSSL,
                  ("path" .=) <$> _httpcPath,
                  ("maskHeaders" .=) <$> _httpcMaskHeaders,
                  ("headers" .=) <$> _httpcHeaders,
                  ("authInfo" .=) <$> _httpcAuthInfo,
                  ("port" .=) <$> _httpcPort])

-- | A collection of data points that describes the time-varying values of a
-- metric. A time series is identified by a combination of a
-- fully-specified monitored resource and a fully-specified metric. This
-- type is used for both listing and creating time series.
--
-- /See:/ 'timeSeries' smart constructor.
data TimeSeries = TimeSeries'
    { _tsPoints :: !(Maybe [Point])
    , _tsMetricKind :: !(Maybe TimeSeriesMetricKind)
    , _tsMetric :: !(Maybe Metric)
    , _tsResource :: !(Maybe MonitoredResource)
    , _tsValueType :: !(Maybe TimeSeriesValueType)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TimeSeries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsPoints'
--
-- * 'tsMetricKind'
--
-- * 'tsMetric'
--
-- * 'tsResource'
--
-- * 'tsValueType'
timeSeries
    :: TimeSeries
timeSeries = 
    TimeSeries'
    { _tsPoints = Nothing
    , _tsMetricKind = Nothing
    , _tsMetric = Nothing
    , _tsResource = Nothing
    , _tsValueType = Nothing
    }

-- | The data points of this time series. When listing time series, the order
-- of the points is specified by the list method.When creating a time
-- series, this field must contain exactly one point and the point\'s type
-- must be the same as the value type of the associated metric. If the
-- associated metric\'s descriptor must be auto-created, then the value
-- type of the descriptor is determined by the point\'s type, which must be
-- BOOL, INT64, DOUBLE, or DISTRIBUTION.
tsPoints :: Lens' TimeSeries [Point]
tsPoints
  = lens _tsPoints (\ s a -> s{_tsPoints = a}) .
      _Default
      . _Coerce

-- | The metric kind of the time series. When listing time series, this
-- metric kind might be different from the metric kind of the associated
-- metric if this time series is an alignment or reduction of other time
-- series.When creating a time series, this field is optional. If present,
-- it must be the same as the metric kind of the associated metric. If the
-- associated metric\'s descriptor must be auto-created, then this field
-- specifies the metric kind of the new descriptor and must be either GAUGE
-- (the default) or CUMULATIVE.
tsMetricKind :: Lens' TimeSeries (Maybe TimeSeriesMetricKind)
tsMetricKind
  = lens _tsMetricKind (\ s a -> s{_tsMetricKind = a})

-- | The associated metric. A fully-specified metric used to identify the
-- time series.
tsMetric :: Lens' TimeSeries (Maybe Metric)
tsMetric = lens _tsMetric (\ s a -> s{_tsMetric = a})

-- | The associated monitored resource. Custom metrics can use only certain
-- monitored resource types in their time series data.
tsResource :: Lens' TimeSeries (Maybe MonitoredResource)
tsResource
  = lens _tsResource (\ s a -> s{_tsResource = a})

-- | The value type of the time series. When listing time series, this value
-- type might be different from the value type of the associated metric if
-- this time series is an alignment or reduction of other time series.When
-- creating a time series, this field is optional. If present, it must be
-- the same as the type of the data in the points field.
tsValueType :: Lens' TimeSeries (Maybe TimeSeriesValueType)
tsValueType
  = lens _tsValueType (\ s a -> s{_tsValueType = a})

instance FromJSON TimeSeries where
        parseJSON
          = withObject "TimeSeries"
              (\ o ->
                 TimeSeries' <$>
                   (o .:? "points" .!= mempty) <*> (o .:? "metricKind")
                     <*> (o .:? "metric")
                     <*> (o .:? "resource")
                     <*> (o .:? "valueType"))

instance ToJSON TimeSeries where
        toJSON TimeSeries'{..}
          = object
              (catMaybes
                 [("points" .=) <$> _tsPoints,
                  ("metricKind" .=) <$> _tsMetricKind,
                  ("metric" .=) <$> _tsMetric,
                  ("resource" .=) <$> _tsResource,
                  ("valueType" .=) <$> _tsValueType])

-- | Information required for a TCP uptime check request.
--
-- /See:/ 'tcpCheck' smart constructor.
newtype TCPCheck = TCPCheck'
    { _tcPort :: Maybe (Textual Int32)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TCPCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcPort'
tcpCheck
    :: TCPCheck
tcpCheck = 
    TCPCheck'
    { _tcPort = Nothing
    }

-- | The port to the page to run the check against. Will be combined with
-- host (specified within the MonitoredResource) to construct the full URL.
-- Required.
tcPort :: Lens' TCPCheck (Maybe Int32)
tcPort
  = lens _tcPort (\ s a -> s{_tcPort = a}) .
      mapping _Coerce

instance FromJSON TCPCheck where
        parseJSON
          = withObject "TCPCheck"
              (\ o -> TCPCheck' <$> (o .:? "port"))

instance ToJSON TCPCheck where
        toJSON TCPCheck'{..}
          = object (catMaybes [("port" .=) <$> _tcPort])

-- | Describes the error status for values that were not written.
--
-- /See:/ 'collectdValueError' smart constructor.
data CollectdValueError = CollectdValueError'
    { _cveError :: !(Maybe Status)
    , _cveIndex :: !(Maybe (Textual Int32))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CollectdValueError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cveError'
--
-- * 'cveIndex'
collectdValueError
    :: CollectdValueError
collectdValueError = 
    CollectdValueError'
    { _cveError = Nothing
    , _cveIndex = Nothing
    }

-- | Records the error status for the value.
cveError :: Lens' CollectdValueError (Maybe Status)
cveError = lens _cveError (\ s a -> s{_cveError = a})

-- | The zero-based index in CollectdPayload.values within the parent
-- CreateCollectdTimeSeriesRequest.collectd_payloads.
cveIndex :: Lens' CollectdValueError (Maybe Int32)
cveIndex
  = lens _cveIndex (\ s a -> s{_cveIndex = a}) .
      mapping _Coerce

instance FromJSON CollectdValueError where
        parseJSON
          = withObject "CollectdValueError"
              (\ o ->
                 CollectdValueError' <$>
                   (o .:? "error") <*> (o .:? "index"))

instance ToJSON CollectdValueError where
        toJSON CollectdValueError'{..}
          = object
              (catMaybes
                 [("error" .=) <$> _cveError,
                  ("index" .=) <$> _cveIndex])
