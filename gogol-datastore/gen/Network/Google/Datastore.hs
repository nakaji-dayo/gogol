{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Datastore
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accesses the schemaless NoSQL database to provide fully managed, robust,
-- scalable storage for your application.
--
-- /See:/ <https://cloud.google.com/datastore/ Google Cloud Datastore API Reference>
module Network.Google.Datastore
    (
    -- * Service Configuration
      datastoreService

    -- * OAuth Scopes
    , cloudPlatformScope
    , datastoreScope

    -- * API Declaration
    , DatastoreAPI

    -- * Resources

    -- ** datastore.projects.allocateIds
    , module Network.Google.Resource.Datastore.Projects.AllocateIds

    -- ** datastore.projects.beginTransaction
    , module Network.Google.Resource.Datastore.Projects.BeginTransaction

    -- ** datastore.projects.commit
    , module Network.Google.Resource.Datastore.Projects.Commit

    -- ** datastore.projects.lookup
    , module Network.Google.Resource.Datastore.Projects.Lookup

    -- ** datastore.projects.operations.cancel
    , module Network.Google.Resource.Datastore.Projects.Operations.Cancel

    -- ** datastore.projects.operations.delete
    , module Network.Google.Resource.Datastore.Projects.Operations.Delete

    -- ** datastore.projects.operations.get
    , module Network.Google.Resource.Datastore.Projects.Operations.Get

    -- ** datastore.projects.operations.list
    , module Network.Google.Resource.Datastore.Projects.Operations.List

    -- ** datastore.projects.reserveIds
    , module Network.Google.Resource.Datastore.Projects.ReserveIds

    -- ** datastore.projects.rollback
    , module Network.Google.Resource.Datastore.Projects.Rollback

    -- ** datastore.projects.runQuery
    , module Network.Google.Resource.Datastore.Projects.RunQuery

    -- * Types

    -- ** LatLng
    , LatLng
    , latLng
    , llLatitude
    , llLongitude

    -- ** TransactionOptions
    , TransactionOptions
    , transactionOptions
    , toReadWrite
    , toReadOnly

    -- ** PropertyOrderDirection
    , PropertyOrderDirection (..)

    -- ** Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- ** GoogleLongrunningOperationMetadata
    , GoogleLongrunningOperationMetadata
    , googleLongrunningOperationMetadata
    , glomAddtional

    -- ** ReadWrite
    , ReadWrite
    , readWrite
    , rwPreviousTransaction

    -- ** GoogleDatastoreAdminV1beta1ExportEntitiesResponse
    , GoogleDatastoreAdminV1beta1ExportEntitiesResponse
    , googleDatastoreAdminV1beta1ExportEntitiesResponse
    , gdaveerOutputURL

    -- ** RollbackRequest
    , RollbackRequest
    , rollbackRequest
    , rrTransaction

    -- ** ReserveIdsRequest
    , ReserveIdsRequest
    , reserveIdsRequest
    , rirKeys
    , rirDatabaseId

    -- ** PartitionId
    , PartitionId
    , partitionId
    , piNamespaceId
    , piProjectId

    -- ** QueryResultBatch
    , QueryResultBatch
    , queryResultBatch
    , qrbSkippedResults
    , qrbSkippedCursor
    , qrbEntityResultType
    , qrbSnapshotVersion
    , qrbEntityResults
    , qrbMoreResults
    , qrbEndCursor

    -- ** CompositeFilterOp
    , CompositeFilterOp (..)

    -- ** EntityProperties
    , EntityProperties
    , entityProperties
    , epAddtional

    -- ** BeginTransactionRequest
    , BeginTransactionRequest
    , beginTransactionRequest
    , btrTransactionOptions

    -- ** RunQueryRequest
    , RunQueryRequest
    , runQueryRequest
    , rqrPartitionId
    , rqrGqlQuery
    , rqrQuery
    , rqrReadOptions

    -- ** AllocateIdsRequest
    , AllocateIdsRequest
    , allocateIdsRequest
    , airKeys

    -- ** QueryResultBatchEntityResultType
    , QueryResultBatchEntityResultType (..)

    -- ** GoogleDatastoreAdminV1beta1CommonMetadata
    , GoogleDatastoreAdminV1beta1CommonMetadata
    , googleDatastoreAdminV1beta1CommonMetadata
    , gdavcmState
    , gdavcmStartTime
    , gdavcmEndTime
    , gdavcmLabels
    , gdavcmOperationType

    -- ** Empty
    , Empty
    , empty

    -- ** CompositeFilter
    , CompositeFilter
    , compositeFilter
    , cfOp
    , cfFilters

    -- ** GoogleDatastoreAdminV1beta1CommonMetadataOperationType
    , GoogleDatastoreAdminV1beta1CommonMetadataOperationType (..)

    -- ** QueryResultBatchMoreResults
    , QueryResultBatchMoreResults (..)

    -- ** GoogleDatastoreAdminV1beta1ImportEntitiesMetadata
    , GoogleDatastoreAdminV1beta1ImportEntitiesMetadata
    , googleDatastoreAdminV1beta1ImportEntitiesMetadata
    , gdaviemProgressBytes
    , gdaviemProgressEntities
    , gdaviemEntityFilter
    , gdaviemInputURL
    , gdaviemCommon

    -- ** GoogleDatastoreAdminV1beta1Progress
    , GoogleDatastoreAdminV1beta1Progress
    , googleDatastoreAdminV1beta1Progress
    , gdavpWorkCompleted
    , gdavpWorkEstimated

    -- ** BeginTransactionResponse
    , BeginTransactionResponse
    , beginTransactionResponse
    , btrTransaction

    -- ** MutationResult
    , MutationResult
    , mutationResult
    , mrConflictDetected
    , mrKey
    , mrVersion

    -- ** AllocateIdsResponse
    , AllocateIdsResponse
    , allocateIdsResponse
    , aKeys

    -- ** GqlQuery
    , GqlQuery
    , gqlQuery
    , gqPositionalBindings
    , gqNamedBindings
    , gqQueryString
    , gqAllowLiterals

    -- ** RunQueryResponse
    , RunQueryResponse
    , runQueryResponse
    , rBatch
    , rQuery

    -- ** Value
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

    -- ** ValueNullValue
    , ValueNullValue (..)

    -- ** StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- ** LookupRequest
    , LookupRequest
    , lookupRequest
    , lrKeys
    , lrReadOptions

    -- ** ReadOptionsReadConsistency
    , ReadOptionsReadConsistency (..)

    -- ** Mutation
    , Mutation
    , mutation
    , mBaseVersion
    , mInsert
    , mUpsert
    , mDelete
    , mUpdate

    -- ** GqlQueryNamedBindings
    , GqlQueryNamedBindings
    , gqlQueryNamedBindings
    , gqnbAddtional

    -- ** PropertyReference
    , PropertyReference
    , propertyReference
    , prName

    -- ** Key
    , Key
    , key
    , kPartitionId
    , kPath

    -- ** PropertyFilter
    , PropertyFilter
    , propertyFilter
    , pfProperty
    , pfOp
    , pfValue

    -- ** Query
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

    -- ** ArrayValue
    , ArrayValue
    , arrayValue
    , avValues

    -- ** EntityResult
    , EntityResult
    , entityResult
    , erCursor
    , erVersion
    , erEntity

    -- ** Xgafv
    , Xgafv (..)

    -- ** CommitResponse
    , CommitResponse
    , commitResponse
    , crIndexUpdates
    , crMutationResults

    -- ** KindExpression
    , KindExpression
    , kindExpression
    , keName

    -- ** GoogleLongrunningOperationResponse
    , GoogleLongrunningOperationResponse
    , googleLongrunningOperationResponse
    , glorAddtional

    -- ** ReadOptions
    , ReadOptions
    , readOptions
    , roReadConsistency
    , roTransaction

    -- ** GoogleDatastoreAdminV1beta1CommonMetadataState
    , GoogleDatastoreAdminV1beta1CommonMetadataState (..)

    -- ** RollbackResponse
    , RollbackResponse
    , rollbackResponse

    -- ** Projection
    , Projection
    , projection
    , pProperty

    -- ** ReserveIdsResponse
    , ReserveIdsResponse
    , reserveIdsResponse

    -- ** Filter
    , Filter
    , filter'
    , fCompositeFilter
    , fPropertyFilter

    -- ** GoogleDatastoreAdminV1beta1CommonMetadataLabels
    , GoogleDatastoreAdminV1beta1CommonMetadataLabels
    , googleDatastoreAdminV1beta1CommonMetadataLabels
    , gdavcmlAddtional

    -- ** PropertyFilterOp
    , PropertyFilterOp (..)

    -- ** CommitRequest
    , CommitRequest
    , commitRequest
    , crMutations
    , crMode
    , crTransaction

    -- ** CommitRequestMode
    , CommitRequestMode (..)

    -- ** GoogleLongrunningListOperationsResponse
    , GoogleLongrunningListOperationsResponse
    , googleLongrunningListOperationsResponse
    , gllorNextPageToken
    , gllorOperations

    -- ** PathElement
    , PathElement
    , pathElement
    , peKind
    , peName
    , peId

    -- ** Entity
    , Entity
    , entity
    , eKey
    , eProperties

    -- ** GoogleDatastoreAdminV1beta1EntityFilter
    , GoogleDatastoreAdminV1beta1EntityFilter
    , googleDatastoreAdminV1beta1EntityFilter
    , gdavefNamespaceIds
    , gdavefKinds

    -- ** ReadOnly
    , ReadOnly
    , readOnly

    -- ** LookupResponse
    , LookupResponse
    , lookupResponse
    , lrDeferred
    , lrFound
    , lrMissing

    -- ** GoogleLongrunningOperation
    , GoogleLongrunningOperation
    , googleLongrunningOperation
    , gloDone
    , gloError
    , gloResponse
    , gloName
    , gloMetadata

    -- ** PropertyOrder
    , PropertyOrder
    , propertyOrder
    , poProperty
    , poDirection

    -- ** GoogleDatastoreAdminV1beta1ExportEntitiesMetadata
    , GoogleDatastoreAdminV1beta1ExportEntitiesMetadata
    , googleDatastoreAdminV1beta1ExportEntitiesMetadata
    , gdaveemProgressBytes
    , gdaveemOutputURLPrefix
    , gdaveemProgressEntities
    , gdaveemEntityFilter
    , gdaveemCommon

    -- ** GqlQueryParameter
    , GqlQueryParameter
    , gqlQueryParameter
    , gqpCursor
    , gqpValue
    ) where

import Network.Google.Prelude
import Network.Google.Datastore.Types
import Network.Google.Resource.Datastore.Projects.AllocateIds
import Network.Google.Resource.Datastore.Projects.BeginTransaction
import Network.Google.Resource.Datastore.Projects.Commit
import Network.Google.Resource.Datastore.Projects.Lookup
import Network.Google.Resource.Datastore.Projects.Operations.Cancel
import Network.Google.Resource.Datastore.Projects.Operations.Delete
import Network.Google.Resource.Datastore.Projects.Operations.Get
import Network.Google.Resource.Datastore.Projects.Operations.List
import Network.Google.Resource.Datastore.Projects.ReserveIds
import Network.Google.Resource.Datastore.Projects.Rollback
import Network.Google.Resource.Datastore.Projects.RunQuery

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Cloud Datastore API service.
type DatastoreAPI =
     ProjectsOperationsListResource :<|>
       ProjectsOperationsGetResource
       :<|> ProjectsOperationsCancelResource
       :<|> ProjectsOperationsDeleteResource
       :<|> ProjectsBeginTransactionResource
       :<|> ProjectsAllocateIdsResource
       :<|> ProjectsRunQueryResource
       :<|> ProjectsRollbackResource
       :<|> ProjectsReserveIdsResource
       :<|> ProjectsLookupResource
       :<|> ProjectsCommitResource
