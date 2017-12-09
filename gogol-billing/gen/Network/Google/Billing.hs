{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Billing
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows developers to manage billing for their Google Cloud Platform
-- projects programmatically.
--
-- /See:/ <https://cloud.google.com/billing/ Google Cloud Billing API Reference>
module Network.Google.Billing
    (
    -- * Service Configuration
      billingService

    -- * OAuth Scopes
    , cloudPlatformScope

    -- * API Declaration
    , BillingAPI

    -- * Resources

    -- ** cloudbilling.billingAccounts.get
    , module Network.Google.Resource.CloudBilling.BillingAccounts.Get

    -- ** cloudbilling.billingAccounts.list
    , module Network.Google.Resource.CloudBilling.BillingAccounts.List

    -- ** cloudbilling.billingAccounts.projects.list
    , module Network.Google.Resource.CloudBilling.BillingAccounts.Projects.List

    -- ** cloudbilling.projects.getBillingInfo
    , module Network.Google.Resource.CloudBilling.Projects.GetBillingInfo

    -- ** cloudbilling.projects.updateBillingInfo
    , module Network.Google.Resource.CloudBilling.Projects.UpdateBillingInfo

    -- ** cloudbilling.services.list
    , module Network.Google.Resource.CloudBilling.Services.List

    -- ** cloudbilling.services.skus.list
    , module Network.Google.Resource.CloudBilling.Services.SKUs.List

    -- * Types

    -- ** ListServicesResponse
    , ListServicesResponse
    , listServicesResponse
    , lsrNextPageToken
    , lsrServices

    -- ** BillingAccount
    , BillingAccount
    , billingAccount
    , baOpen
    , baName
    , baDisplayName

    -- ** Service
    , Service
    , service
    , sName
    , sDisplayName
    , sServiceId

    -- ** ProjectBillingInfo
    , ProjectBillingInfo
    , projectBillingInfo
    , pbiName
    , pbiBillingAccountName
    , pbiProjectId
    , pbiBillingEnabled

    -- ** TierRate
    , TierRate
    , tierRate
    , trUnitPrice
    , trStartUsageAmount

    -- ** Money
    , Money
    , money
    , mCurrencyCode
    , mNanos
    , mUnits

    -- ** Category
    , Category
    , category
    , cResourceFamily
    , cUsageType
    , cServiceDisplayName
    , cResourceGroup

    -- ** AggregationInfoAggregationLevel
    , AggregationInfoAggregationLevel (..)

    -- ** ListProjectBillingInfoResponse
    , ListProjectBillingInfoResponse
    , listProjectBillingInfoResponse
    , lpbirNextPageToken
    , lpbirProjectBillingInfo

    -- ** ListSKUsResponse
    , ListSKUsResponse
    , listSKUsResponse
    , lskurNextPageToken
    , lskurSKUs

    -- ** PricingExpression
    , PricingExpression
    , pricingExpression
    , peUsageUnitDescription
    , peBaseUnit
    , peBaseUnitConversionFactor
    , peDisplayQuantity
    , peTieredRates
    , peBaseUnitDescription
    , peUsageUnit

    -- ** Xgafv
    , Xgafv (..)

    -- ** AggregationInfo
    , AggregationInfo
    , aggregationInfo
    , aiAggregationInterval
    , aiAggregationCount
    , aiAggregationLevel

    -- ** SKU
    , SKU
    , sKU
    , skukCategory
    , skukSKUId
    , skukServiceProviderName
    , skukServiceRegions
    , skukName
    , skukPricingInfo
    , skukDescription

    -- ** ListBillingAccountsResponse
    , ListBillingAccountsResponse
    , listBillingAccountsResponse
    , lbarNextPageToken
    , lbarBillingAccounts

    -- ** PricingInfo
    , PricingInfo
    , pricingInfo
    , piSummary
    , piAggregationInfo
    , piPricingExpression
    , piCurrencyConversionRate
    , piEffectiveTime

    -- ** AggregationInfoAggregationInterval
    , AggregationInfoAggregationInterval (..)
    ) where

import Network.Google.Prelude
import Network.Google.Billing.Types
import Network.Google.Resource.CloudBilling.BillingAccounts.Get
import Network.Google.Resource.CloudBilling.BillingAccounts.List
import Network.Google.Resource.CloudBilling.BillingAccounts.Projects.List
import Network.Google.Resource.CloudBilling.Projects.GetBillingInfo
import Network.Google.Resource.CloudBilling.Projects.UpdateBillingInfo
import Network.Google.Resource.CloudBilling.Services.List
import Network.Google.Resource.CloudBilling.Services.SKUs.List

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Cloud Billing API service.
type BillingAPI =
     ProjectsGetBillingInfoResource :<|>
       ProjectsUpdateBillingInfoResource
       :<|> ServicesSKUsListResource
       :<|> ServicesListResource
       :<|> BillingAccountsProjectsListResource
       :<|> BillingAccountsListResource
       :<|> BillingAccountsGetResource
