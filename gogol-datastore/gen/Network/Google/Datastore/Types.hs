{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Datastore.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Datastore.Types
    (
    -- * Service Configuration
      datastoreService

    -- * OAuth Scopes
    , cloudPlatformScope
    , datastoreScope

    -- * LatLng
    , LatLng
    , latLng
    , llLatitude
    , llLongitude

    -- * TransactionOptions
    , TransactionOptions
    , transactionOptions
    , toReadWrite
    , toReadOnly

    -- * PropertyOrderDirection
    , PropertyOrderDirection (..)

    -- * Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- * GoogleLongrunningOperationMetadata
    , GoogleLongrunningOperationMetadata
    , googleLongrunningOperationMetadata
    , glomAddtional

    -- * ReadWrite
    , ReadWrite
    , readWrite
    , rwPreviousTransaction

    -- * GoogleDatastoreAdminV1beta1ExportEntitiesResponse
    , GoogleDatastoreAdminV1beta1ExportEntitiesResponse
    , googleDatastoreAdminV1beta1ExportEntitiesResponse
    , gdaveerOutputURL

    -- * RollbackRequest
    , RollbackRequest
    , rollbackRequest
    , rrTransaction

    -- * ReserveIdsRequest
    , ReserveIdsRequest
    , reserveIdsRequest
    , rirKeys
    , rirDatabaseId

    -- * PartitionId
    , PartitionId
    , partitionId
    , piNamespaceId
    , piProjectId

    -- * QueryResultBatch
    , QueryResultBatch
    , queryResultBatch
    , qrbSkippedResults
    , qrbSkippedCursor
    , qrbEntityResultType
    , qrbSnapshotVersion
    , qrbEntityResults
    , qrbMoreResults
    , qrbEndCursor

    -- * CompositeFilterOp
    , CompositeFilterOp (..)

    -- * EntityProperties
    , EntityProperties
    , entityProperties
    , epAddtional

    -- * BeginTransactionRequest
    , BeginTransactionRequest
    , beginTransactionRequest
    , btrTransactionOptions

    -- * RunQueryRequest
    , RunQueryRequest
    , runQueryRequest
    , rqrPartitionId
    , rqrGqlQuery
    , rqrQuery
    , rqrReadOptions

    -- * AllocateIdsRequest
    , AllocateIdsRequest
    , allocateIdsRequest
    , airKeys

    -- * QueryResultBatchEntityResultType
    , QueryResultBatchEntityResultType (..)

    -- * GoogleDatastoreAdminV1beta1CommonMetadata
    , GoogleDatastoreAdminV1beta1CommonMetadata
    , googleDatastoreAdminV1beta1CommonMetadata
    , gdavcmState
    , gdavcmStartTime
    , gdavcmEndTime
    , gdavcmLabels
    , gdavcmOperationType

    -- * Empty
    , Empty
    , empty

    -- * CompositeFilter
    , CompositeFilter
    , compositeFilter
    , cfOp
    , cfFilters

    -- * GoogleDatastoreAdminV1beta1CommonMetadataOperationType
    , GoogleDatastoreAdminV1beta1CommonMetadataOperationType (..)

    -- * QueryResultBatchMoreResults
    , QueryResultBatchMoreResults (..)

    -- * GoogleDatastoreAdminV1beta1ImportEntitiesMetadata
    , GoogleDatastoreAdminV1beta1ImportEntitiesMetadata
    , googleDatastoreAdminV1beta1ImportEntitiesMetadata
    , gdaviemProgressBytes
    , gdaviemProgressEntities
    , gdaviemEntityFilter
    , gdaviemInputURL
    , gdaviemCommon

    -- * GoogleDatastoreAdminV1beta1Progress
    , GoogleDatastoreAdminV1beta1Progress
    , googleDatastoreAdminV1beta1Progress
    , gdavpWorkCompleted
    , gdavpWorkEstimated

    -- * BeginTransactionResponse
    , BeginTransactionResponse
    , beginTransactionResponse
    , btrTransaction

    -- * MutationResult
    , MutationResult
    , mutationResult
    , mrConflictDetected
    , mrKey
    , mrVersion

    -- * AllocateIdsResponse
    , AllocateIdsResponse
    , allocateIdsResponse
    , aKeys

    -- * GqlQuery
    , GqlQuery
    , gqlQuery
    , gqPositionalBindings
    , gqNamedBindings
    , gqQueryString
    , gqAllowLiterals

    -- * RunQueryResponse
    , RunQueryResponse
    , runQueryResponse
    , rBatch
    , rQuery

    -- * Value
    , Value
    , value
    , vKeyValue
    , vGeoPointValue
    , vIntegerValue
    , vTimestampValue
    , vEntityValue
    , vExcludeFromIndexes
    , vDoubleValue
    , vStringValue
    , vBooleanValue
    , vMeaning
    , vArrayValue
    , vNullValue
    , vBlobValue

    -- * ValueNullValue
    , ValueNullValue (..)

    -- * StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- * LookupRequest
    , LookupRequest
    , lookupRequest
    , lrKeys
    , lrReadOptions

    -- * ReadOptionsReadConsistency
    , ReadOptionsReadConsistency (..)

    -- * Mutation
    , Mutation
    , mutation
    , mBaseVersion
    , mInsert
    , mUpsert
    , mDelete
    , mUpdate

    -- * GqlQueryNamedBindings
    , GqlQueryNamedBindings
    , gqlQueryNamedBindings
    , gqnbAddtional

    -- * PropertyReference
    , PropertyReference
    , propertyReference
    , prName

    -- * Key
    , Key
    , key
    , kPartitionId
    , kPath

    -- * PropertyFilter
    , PropertyFilter
    , propertyFilter
    , pfProperty
    , pfOp
    , pfValue

    -- * Query
    , Query
    , query
    , qStartCursor
    , qOffSet
    , qKind
    , qDistinctOn
    , qEndCursor
    , qLimit
    , qProjection
    , qFilter
    , qOrder

    -- * ArrayValue
    , ArrayValue
    , arrayValue
    , avValues

    -- * EntityResult
    , EntityResult
    , entityResult
    , erCursor
    , erVersion
    , erEntity

    -- * Xgafv
    , Xgafv (..)

    -- * CommitResponse
    , CommitResponse
    , commitResponse
    , crIndexUpdates
    , crMutationResults

    -- * KindExpression
    , KindExpression
    , kindExpression
    , keName

    -- * GoogleLongrunningOperationResponse
    , GoogleLongrunningOperationResponse
    , googleLongrunningOperationResponse
    , glorAddtional

    -- * ReadOptions
    , ReadOptions
    , readOptions
    , roReadConsistency
    , roTransaction

    -- * GoogleDatastoreAdminV1beta1CommonMetadataState
    , GoogleDatastoreAdminV1beta1CommonMetadataState (..)

    -- * RollbackResponse
    , RollbackResponse
    , rollbackResponse

    -- * Projection
    , Projection
    , projection
    , pProperty

    -- * ReserveIdsResponse
    , ReserveIdsResponse
    , reserveIdsResponse

    -- * Filter
    , Filter
    , filter'
    , fCompositeFilter
    , fPropertyFilter

    -- * GoogleDatastoreAdminV1beta1CommonMetadataLabels
    , GoogleDatastoreAdminV1beta1CommonMetadataLabels
    , googleDatastoreAdminV1beta1CommonMetadataLabels
    , gdavcmlAddtional

    -- * PropertyFilterOp
    , PropertyFilterOp (..)

    -- * CommitRequest
    , CommitRequest
    , commitRequest
    , crMutations
    , crMode
    , crTransaction

    -- * CommitRequestMode
    , CommitRequestMode (..)

    -- * GoogleLongrunningListOperationsResponse
    , GoogleLongrunningListOperationsResponse
    , googleLongrunningListOperationsResponse
    , gllorNextPageToken
    , gllorOperations

    -- * PathElement
    , PathElement
    , pathElement
    , peKind
    , peName
    , peId

    -- * Entity
    , Entity
    , entity
    , eKey
    , eProperties

    -- * GoogleDatastoreAdminV1beta1EntityFilter
    , GoogleDatastoreAdminV1beta1EntityFilter
    , googleDatastoreAdminV1beta1EntityFilter
    , gdavefNamespaceIds
    , gdavefKinds

    -- * ReadOnly
    , ReadOnly
    , readOnly

    -- * LookupResponse
    , LookupResponse
    , lookupResponse
    , lrDeferred
    , lrFound
    , lrMissing

    -- * GoogleLongrunningOperation
    , GoogleLongrunningOperation
    , googleLongrunningOperation
    , gloDone
    , gloError
    , gloResponse
    , gloName
    , gloMetadata

    -- * PropertyOrder
    , PropertyOrder
    , propertyOrder
    , poProperty
    , poDirection

    -- * GoogleDatastoreAdminV1beta1ExportEntitiesMetadata
    , GoogleDatastoreAdminV1beta1ExportEntitiesMetadata
    , googleDatastoreAdminV1beta1ExportEntitiesMetadata
    , gdaveemProgressBytes
    , gdaveemOutputURLPrefix
    , gdaveemProgressEntities
    , gdaveemEntityFilter
    , gdaveemCommon

    -- * GqlQueryParameter
    , GqlQueryParameter
    , gqlQueryParameter
    , gqpCursor
    , gqpValue
    ) where

import Network.Google.Datastore.Types.Product
import Network.Google.Datastore.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v1' of the Google Cloud Datastore API. This contains the host and root path used as a starting point for constructing service requests.
datastoreService :: ServiceConfig
datastoreService
  = defaultService (ServiceId "datastore:v1")
      "datastore.googleapis.com"

-- | View and manage your data across Google Cloud Platform services
cloudPlatformScope :: Proxy '["https://www.googleapis.com/auth/cloud-platform"]
cloudPlatformScope = Proxy;

-- | View and manage your Google Cloud Datastore data
datastoreScope :: Proxy '["https://www.googleapis.com/auth/datastore"]
datastoreScope = Proxy;
