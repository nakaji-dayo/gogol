{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.ShoppingContent.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.ShoppingContent.Types
    (
    -- * Service Configuration
      shoppingContentService

    -- * OAuth Scopes
    , contentScope

    -- * OrdersAcknowledgeRequest
    , OrdersAcknowledgeRequest
    , ordersAcknowledgeRequest
    , oarOperationId

    -- * AccountTax
    , AccountTax
    , accountTax
    , atRules
    , atKind
    , atAccountId

    -- * OrdersUpdateMerchantOrderIdRequest
    , OrdersUpdateMerchantOrderIdRequest
    , ordersUpdateMerchantOrderIdRequest
    , oumoirMerchantOrderId
    , oumoirOperationId

    -- * OrdersAdvanceTestOrderResponse
    , OrdersAdvanceTestOrderResponse
    , ordersAdvanceTestOrderResponse
    , oatorKind

    -- * ProductsCustomBatchResponse
    , ProductsCustomBatchResponse
    , productsCustomBatchResponse
    , pcbrEntries
    , pcbrKind

    -- * OrdersGettestOrdertemplateTemplateName
    , OrdersGettestOrdertemplateTemplateName (..)

    -- * TestOrderCustomer
    , TestOrderCustomer
    , testOrderCustomer
    , tocFullName
    , tocEmail
    , tocExplicitMarketingPreference

    -- * DatafeedstatusesCustomBatchResponse
    , DatafeedstatusesCustomBatchResponse
    , datafeedstatusesCustomBatchResponse
    , dcbrEntries
    , dcbrKind

    -- * OrderReturn
    , OrderReturn
    , orderReturn
    , orQuantity
    , orActor
    , orReason
    , orCreationDate
    , orReasonText

    -- * AccounttaxCustomBatchResponseEntry
    , AccounttaxCustomBatchResponseEntry
    , accounttaxCustomBatchResponseEntry
    , acbreAccountTax
    , acbreKind
    , acbreErrors
    , acbreBatchId

    -- * InventoryCustomBatchRequest
    , InventoryCustomBatchRequest
    , inventoryCustomBatchRequest
    , icbrEntries

    -- * AccountsAuthInfoResponse
    , AccountsAuthInfoResponse
    , accountsAuthInfoResponse
    , aairKind
    , aairAccountIdentifiers

    -- * ProductStatusDestinationStatus
    , ProductStatusDestinationStatus
    , productStatusDestinationStatus
    , psdsDestination
    , psdsIntention
    , psdsApprovalStatus

    -- * AccountTaxTaxRule
    , AccountTaxTaxRule
    , accountTaxTaxRule
    , attrUseGlobalRate
    , attrCountry
    , attrShippingTaxed
    , attrLocationId
    , attrRatePercent

    -- * PostalCodeGroup
    , PostalCodeGroup
    , postalCodeGroup
    , pcgCountry
    , pcgPostalCodeRanges
    , pcgName

    -- * ProductDestination
    , ProductDestination
    , productDestination
    , pdIntention
    , pdDestinationName

    -- * DatafeedsCustomBatchRequest
    , DatafeedsCustomBatchRequest
    , datafeedsCustomBatchRequest
    , dEntries

    -- * OrdersCustomBatchRequestEntry
    , OrdersCustomBatchRequestEntry
    , ordersCustomBatchRequestEntry
    , ocbreMerchantId
    , ocbreCancelLineItem
    , ocbreRefund
    , ocbreUpdateShipment
    , ocbreReturnLineItem
    , ocbreMerchantOrderId
    , ocbreMethod
    , ocbreShipLineItems
    , ocbreOperationId
    , ocbreOrderId
    , ocbreCancel
    , ocbreBatchId

    -- * OrdersRefundRequest
    , OrdersRefundRequest
    , ordersRefundRequest
    , orrAmount
    , orrReason
    , orrOperationId
    , orrAmountPretax
    , orrAmountTax
    , orrReasonText

    -- * OrdersCustomBatchRequestEntryCancelLineItem
    , OrdersCustomBatchRequestEntryCancelLineItem
    , ordersCustomBatchRequestEntryCancelLineItem
    , ocbrecliAmount
    , ocbrecliQuantity
    , ocbrecliLineItemId
    , ocbrecliReason
    , ocbrecliAmountPretax
    , ocbrecliProductId
    , ocbrecliAmountTax
    , ocbrecliReasonText

    -- * OrderLineItemShippingDetailsMethod
    , OrderLineItemShippingDetailsMethod
    , orderLineItemShippingDetailsMethod
    , olisdmCarrier
    , olisdmMethodName
    , olisdmMaxDaysInTransit
    , olisdmMinDaysInTransit

    -- * Datafeed
    , Datafeed
    , datafeed
    , dKind
    , dFormat
    , dAttributeLanguage
    , dTargetCountry
    , dFetchSchedule
    , dName
    , dIntendedDestinations
    , dTargets
    , dId
    , dContentLanguage
    , dContentType
    , dFileName

    -- * OrdersCreateTestOrderResponse
    , OrdersCreateTestOrderResponse
    , ordersCreateTestOrderResponse
    , octorKind
    , octorOrderId

    -- * AccountsCustomBatchResponseEntry
    , AccountsCustomBatchResponseEntry
    , accountsCustomBatchResponseEntry
    , aKind
    , aAccount
    , aErrors
    , aBatchId

    -- * OrdersListOrderBy
    , OrdersListOrderBy (..)

    -- * AccountIdentifier
    , AccountIdentifier
    , accountIdentifier
    , aiMerchantId
    , aiAggregatorId

    -- * TestOrderPaymentMethod
    , TestOrderPaymentMethod
    , testOrderPaymentMethod
    , topmExpirationMonth
    , topmExpirationYear
    , topmLastFourDigits
    , topmType
    , topmPredefinedBillingAddress

    -- * OrderLineItem
    , OrderLineItem
    , orderLineItem
    , oliQuantityOrdered
    , oliReturnInfo
    , oliQuantityDelivered
    , oliShippingDetails
    , oliQuantityPending
    , oliCancellations
    , oliQuantityCanceled
    , oliId
    , oliTax
    , oliPrice
    , oliQuantityShipped
    , oliQuantityReturned
    , oliProduct
    , oliReturns

    -- * Service
    , Service
    , service
    , sDeliveryCountry
    , sRateGroups
    , sDeliveryTime
    , sActive
    , sName
    , sCurrency
    , sMinimumOrderValue

    -- * ProductstatusesCustomBatchResponse
    , ProductstatusesCustomBatchResponse
    , productstatusesCustomBatchResponse
    , pEntries
    , pKind

    -- * ProductUnitPricingMeasure
    , ProductUnitPricingMeasure
    , productUnitPricingMeasure
    , pupmValue
    , pupmUnit

    -- * OrdersUpdateShipmentRequest
    , OrdersUpdateShipmentRequest
    , ordersUpdateShipmentRequest
    , ousrCarrier
    , ousrStatus
    , ousrTrackingId
    , ousrShipmentId
    , ousrOperationId

    -- * OrderShipmentLineItemShipment
    , OrderShipmentLineItemShipment
    , orderShipmentLineItemShipment
    , oslisQuantity
    , oslisLineItemId
    , oslisProductId

    -- * OrdersListStatuses
    , OrdersListStatuses (..)

    -- * LoyaltyPoints
    , LoyaltyPoints
    , loyaltyPoints
    , lpRatio
    , lpPointsValue
    , lpName

    -- * OrdersCustomBatchRequestEntryShipLineItems
    , OrdersCustomBatchRequestEntryShipLineItems
    , ordersCustomBatchRequestEntryShipLineItems
    , ocbresliCarrier
    , ocbresliTrackingId
    , ocbresliShipmentId
    , ocbresliShipmentInfos
    , ocbresliLineItems

    -- * AccountStatus
    , AccountStatus
    , accountStatus
    , asDataQualityIssues
    , asAccountLevelIssues
    , asKind
    , asAccountId
    , asWebsiteClaimed

    -- * OrdersReturnLineItemRequest
    , OrdersReturnLineItemRequest
    , ordersReturnLineItemRequest
    , orlirQuantity
    , orlirLineItemId
    , orlirReason
    , orlirOperationId
    , orlirProductId
    , orlirReasonText

    -- * ShippingSettingsCustomBatchRequestEntry
    , ShippingSettingsCustomBatchRequestEntry
    , shippingSettingsCustomBatchRequestEntry
    , sscbreMerchantId
    , sscbreAccountId
    , sscbreMethod
    , sscbreShippingSettings
    , sscbreBatchId

    -- * AccountstatusesCustomBatchRequest
    , AccountstatusesCustomBatchRequest
    , accountstatusesCustomBatchRequest
    , acbrEntries

    -- * AccounttaxListResponse
    , AccounttaxListResponse
    , accounttaxListResponse
    , alrNextPageToken
    , alrKind
    , alrResources

    -- * OrdersGetTestOrderTemplateResponse
    , OrdersGetTestOrderTemplateResponse
    , ordersGetTestOrderTemplateResponse
    , ogtotrKind
    , ogtotrTemplate

    -- * AccountsCustomBatchRequestEntry
    , AccountsCustomBatchRequestEntry
    , accountsCustomBatchRequestEntry
    , accMerchantId
    , accForce
    , accAccount
    , accAccountId
    , accMethod
    , accOverwrite
    , accBatchId

    -- * Weight
    , Weight
    , weight
    , wValue
    , wUnit

    -- * Error'
    , Error'
    , error'
    , eDomain
    , eReason
    , eMessage

    -- * ProductstatusesListResponse
    , ProductstatusesListResponse
    , productstatusesListResponse
    , plrNextPageToken
    , plrKind
    , plrResources

    -- * OrdersRefundResponse
    , OrdersRefundResponse
    , ordersRefundResponse
    , orrKind
    , orrExecutionStatus

    -- * OrdersCreateTestOrderRequest
    , OrdersCreateTestOrderRequest
    , ordersCreateTestOrderRequest
    , octorTemplateName
    , octorTestOrder

    -- * AccountUser
    , AccountUser
    , accountUser
    , auAdmin
    , auEmailAddress

    -- * AccountStatusExampleItem
    , AccountStatusExampleItem
    , accountStatusExampleItem
    , aseiSubmittedValue
    , aseiLink
    , aseiItemId
    , aseiTitle
    , aseiValueOnLandingPage

    -- * DatafeedsCustomBatchRequestEntry
    , DatafeedsCustomBatchRequestEntry
    , datafeedsCustomBatchRequestEntry
    , dcbreMerchantId
    , dcbreDatafeed
    , dcbreMethod
    , dcbreDatafeedId
    , dcbreBatchId

    -- * AccountStatusAccountLevelIssue
    , AccountStatusAccountLevelIssue
    , accountStatusAccountLevelIssue
    , asaliCountry
    , asaliSeverity
    , asaliId
    , asaliTitle
    , asaliDetail

    -- * Value
    , Value
    , value
    , vPricePercentage
    , vCarrierRateName
    , vFlatRate
    , vSubtableName
    , vNoShipping

    -- * Installment
    , Installment
    , installment
    , iAmount
    , iMonths

    -- * DatafeedFetchSchedule
    , DatafeedFetchSchedule
    , datafeedFetchSchedule
    , dfsFetchURL
    , dfsUsername
    , dfsMinuteOfHour
    , dfsPassword
    , dfsDayOfMonth
    , dfsHour
    , dfsWeekday
    , dfsTimeZone
    , dfsPaused

    -- * OrdersCustomBatchRequest
    , OrdersCustomBatchRequest
    , ordersCustomBatchRequest
    , ocbrEntries

    -- * ShippingSettingsGetSupportedCarriersResponse
    , ShippingSettingsGetSupportedCarriersResponse
    , shippingSettingsGetSupportedCarriersResponse
    , ssgscrKind
    , ssgscrCarriers

    -- * AccountsListResponse
    , AccountsListResponse
    , accountsListResponse
    , accNextPageToken
    , accKind
    , accResources

    -- * ProductStatusDataQualityIssue
    , ProductStatusDataQualityIssue
    , productStatusDataQualityIssue
    , psdqiLocation
    , psdqiFetchStatus
    , psdqiSeverity
    , psdqiValueProvided
    , psdqiId
    , psdqiValueOnLandingPage
    , psdqiTimestamp
    , psdqiDetail

    -- * CarriersCarrier
    , CarriersCarrier
    , carriersCarrier
    , ccCountry
    , ccName
    , ccServices

    -- * CarrierRate
    , CarrierRate
    , carrierRate
    , crOriginPostalCode
    , crFlatAdjustment
    , crCarrierService
    , crName
    , crPercentageAdjustment
    , crCarrierName

    -- * ShippingSettingsListResponse
    , ShippingSettingsListResponse
    , shippingSettingsListResponse
    , sslrNextPageToken
    , sslrKind
    , sslrResources

    -- * OrdersShipLineItemsRequest
    , OrdersShipLineItemsRequest
    , ordersShipLineItemsRequest
    , oslirCarrier
    , oslirTrackingId
    , oslirShipmentId
    , oslirShipmentInfos
    , oslirLineItems
    , oslirOperationId

    -- * AccountsCustomBatchResponse
    , AccountsCustomBatchResponse
    , accountsCustomBatchResponse
    , acbrcEntries
    , acbrcKind

    -- * ProductTax
    , ProductTax
    , productTax
    , ptTaxShip
    , ptCountry
    , ptPostalCode
    , ptRate
    , ptRegion
    , ptLocationId

    -- * OrderShipment
    , OrderShipment
    , orderShipment
    , osCarrier
    , osStatus
    , osTrackingId
    , osLineItems
    , osId
    , osCreationDate
    , osDeliveryDate

    -- * OrderLineItemReturnInfo
    , OrderLineItemReturnInfo
    , orderLineItemReturnInfo
    , oliriIsReturnable
    , oliriPolicyURL
    , oliriDaysToReturn

    -- * Account
    , Account
    , account
    , aaUsers
    , aaYouTubeChannelLinks
    , aaKind
    , aaSellerId
    , aaName
    , aaReviewsURL
    , aaId
    , aaWebsiteURL
    , aaAdwordsLinks
    , aaGoogleMyBusinessLink
    , aaAdultContent

    -- * InventorySetRequest
    , InventorySetRequest
    , inventorySetRequest
    , isrLoyaltyPoints
    , isrQuantity
    , isrInstallment
    , isrSalePrice
    , isrAvailability
    , isrPickup
    , isrSalePriceEffectiveDate
    , isrSellOnGoogleQuantity
    , isrPrice

    -- * OrdersCancelLineItemRequest
    , OrdersCancelLineItemRequest
    , ordersCancelLineItemRequest
    , oclirAmount
    , oclirQuantity
    , oclirLineItemId
    , oclirReason
    , oclirOperationId
    , oclirAmountPretax
    , oclirProductId
    , oclirAmountTax
    , oclirReasonText

    -- * ProductShippingWeight
    , ProductShippingWeight
    , productShippingWeight
    , pswValue
    , pswUnit

    -- * AccountstatusesCustomBatchRequestEntry
    , AccountstatusesCustomBatchRequestEntry
    , accountstatusesCustomBatchRequestEntry
    , acbrecMerchantId
    , acbrecAccountId
    , acbrecMethod
    , acbrecBatchId

    -- * DeliveryTime
    , DeliveryTime
    , deliveryTime
    , dtMinTransitTimeInDays
    , dtMaxTransitTimeInDays

    -- * ProductstatusesCustomBatchResponseEntry
    , ProductstatusesCustomBatchResponseEntry
    , productstatusesCustomBatchResponseEntry
    , pcbreKind
    , pcbreProductStatus
    , pcbreErrors
    , pcbreBatchId

    -- * OrdersCustomBatchRequestEntryCancel
    , OrdersCustomBatchRequestEntryCancel
    , ordersCustomBatchRequestEntryCancel
    , ocbrecReason
    , ocbrecReasonText

    -- * DatafeedFormat
    , DatafeedFormat
    , datafeedFormat
    , dfQuotingMode
    , dfFileEncoding
    , dfColumnDelimiter

    -- * ProductShipping
    , ProductShipping
    , productShipping
    , psService
    , psLocationGroupName
    , psCountry
    , psPostalCode
    , psPrice
    , psRegion
    , psLocationId

    -- * ShippingSettingsCustomBatchRequest
    , ShippingSettingsCustomBatchRequest
    , shippingSettingsCustomBatchRequest
    , sscbrEntries

    -- * AccountsCustomBatchRequest
    , AccountsCustomBatchRequest
    , accountsCustomBatchRequest
    , aEntries

    -- * ProductCustomAttribute
    , ProductCustomAttribute
    , productCustomAttribute
    , pcaValue
    , pcaName
    , pcaType
    , pcaUnit

    -- * OrdersListResponse
    , OrdersListResponse
    , ordersListResponse
    , olrNextPageToken
    , olrKind
    , olrResources

    -- * Headers
    , Headers
    , headers
    , hNumberOfItems
    , hPostalCodeGroupNames
    , hPrices
    , hWeights
    , hLocations

    -- * OrdersShipLineItemsResponse
    , OrdersShipLineItemsResponse
    , ordersShipLineItemsResponse
    , oslirKind
    , oslirExecutionStatus

    -- * ShippingSettings
    , ShippingSettings
    , shippingSettings
    , ssPostalCodeGroups
    , ssAccountId
    , ssServices

    -- * PostalCodeRange
    , PostalCodeRange
    , postalCodeRange
    , pcrPostalCodeRangeBegin
    , pcrPostalCodeRangeEnd

    -- * OrdersUpdateShipmentResponse
    , OrdersUpdateShipmentResponse
    , ordersUpdateShipmentResponse
    , ousrKind
    , ousrExecutionStatus

    -- * ProductstatusesCustomBatchRequest
    , ProductstatusesCustomBatchRequest
    , productstatusesCustomBatchRequest
    , proEntries

    -- * AccountYouTubeChannelLink
    , AccountYouTubeChannelLink
    , accountYouTubeChannelLink
    , aytclStatus
    , aytclChannelId

    -- * OrdersReturnLineItemResponse
    , OrdersReturnLineItemResponse
    , ordersReturnLineItemResponse
    , orlirKind
    , orlirExecutionStatus

    -- * ProductCustomGroup
    , ProductCustomGroup
    , productCustomGroup
    , pName
    , pAttributes

    -- * AccountstatusesCustomBatchResponse
    , AccountstatusesCustomBatchResponse
    , accountstatusesCustomBatchResponse
    , acccEntries
    , acccKind

    -- * ShippingSettingsCustomBatchResponseEntry
    , ShippingSettingsCustomBatchResponseEntry
    , shippingSettingsCustomBatchResponseEntry
    , sKind
    , sShippingSettings
    , sErrors
    , sBatchId

    -- * ProductStatus
    , ProductStatus
    , productStatus
    , psDataQualityIssues
    , psKind
    , psLink
    , psDestinationStatuses
    , psLastUpdateDate
    , psCreationDate
    , psTitle
    , psProduct
    , psGoogleExpirationDate
    , psProductId

    -- * AccountstatusesListResponse
    , AccountstatusesListResponse
    , accountstatusesListResponse
    , alrlNextPageToken
    , alrlKind
    , alrlResources

    -- * AccounttaxCustomBatchRequest
    , AccounttaxCustomBatchRequest
    , accounttaxCustomBatchRequest
    , accEntries

    -- * ProductsCustomBatchRequestEntry
    , ProductsCustomBatchRequestEntry
    , productsCustomBatchRequestEntry
    , pMerchantId
    , pMethod
    , pProduct
    , pProductId
    , pBatchId

    -- * AccountGoogleMyBusinessLink
    , AccountGoogleMyBusinessLink
    , accountGoogleMyBusinessLink
    , agmblGmbEmail
    , agmblStatus

    -- * DatafeedstatusesCustomBatchRequestEntry
    , DatafeedstatusesCustomBatchRequestEntry
    , datafeedstatusesCustomBatchRequestEntry
    , dMerchantId
    , dCountry
    , dMethod
    , dDatafeedId
    , dLanguage
    , dBatchId

    -- * OrderCustomer
    , OrderCustomer
    , orderCustomer
    , ocFullName
    , ocEmail
    , ocExplicitMarketingPreference

    -- * InventoryCustomBatchResponseEntry
    , InventoryCustomBatchResponseEntry
    , inventoryCustomBatchResponseEntry
    , icbreKind
    , icbreErrors
    , icbreBatchId

    -- * LocationIdSet
    , LocationIdSet
    , locationIdSet
    , lisLocationIds

    -- * Row
    , Row
    , row
    , rCells

    -- * Inventory
    , Inventory
    , inventory
    , iLoyaltyPoints
    , iKind
    , iQuantity
    , iInstallment
    , iSalePrice
    , iAvailability
    , iPickup
    , iSalePriceEffectiveDate
    , iSellOnGoogleQuantity
    , iPrice

    -- * OrdersGetByMerchantOrderIdResponse
    , OrdersGetByMerchantOrderIdResponse
    , ordersGetByMerchantOrderIdResponse
    , ogbmoirKind
    , ogbmoirOrder

    -- * OrderPromotionBenefit
    , OrderPromotionBenefit
    , orderPromotionBenefit
    , opbTaxImpact
    , opbDiscount
    , opbOfferIds
    , opbSubType
    , opbType

    -- * OrdersCancelRequest
    , OrdersCancelRequest
    , ordersCancelRequest
    , ocrReason
    , ocrOperationId
    , ocrReasonText

    -- * OrderLineItemProductVariantAttribute
    , OrderLineItemProductVariantAttribute
    , orderLineItemProductVariantAttribute
    , olipvaDimension
    , olipvaValue

    -- * OrdersCustomBatchResponseEntry
    , OrdersCustomBatchResponseEntry
    , ordersCustomBatchResponseEntry
    , oKind
    , oExecutionStatus
    , oErrors
    , oOrder
    , oBatchId

    -- * RateGroup
    , RateGroup
    , rateGroup
    , rgCarrierRates
    , rgApplicableShippingLabels
    , rgMainTable
    , rgSingleValue
    , rgSubtables

    -- * OrderPromotion
    , OrderPromotion
    , orderPromotion
    , opEffectiveDates
    , opGenericRedemptionCode
    , opRedemptionChannel
    , opBenefits
    , opLongTitle
    , opId
    , opProductApplicability

    -- * Price
    , Price
    , price
    , pValue
    , pCurrency

    -- * OrderLineItemShippingDetails
    , OrderLineItemShippingDetails
    , orderLineItemShippingDetails
    , olisdShipByDate
    , olisdMethod
    , olisdDeliverByDate

    -- * DatafeedsCustomBatchResponse
    , DatafeedsCustomBatchResponse
    , datafeedsCustomBatchResponse
    , datEntries
    , datKind

    -- * OrderDeliveryDetails
    , OrderDeliveryDetails
    , orderDeliveryDetails
    , oddAddress
    , oddPhoneNumber

    -- * OrdersCancelResponse
    , OrdersCancelResponse
    , ordersCancelResponse
    , ocrKind
    , ocrExecutionStatus

    -- * TestOrder
    , TestOrder
    , testOrder
    , toKind
    , toLineItems
    , toShippingOption
    , toPredefinedDeliveryAddress
    , toShippingCostTax
    , toCustomer
    , toPaymentMethod
    , toPromotions
    , toNotificationMode
    , toShippingCost

    -- * DatafeedstatusesCustomBatchResponseEntry
    , DatafeedstatusesCustomBatchResponseEntry
    , datafeedstatusesCustomBatchResponseEntry
    , datErrors
    , datDatafeedStatus
    , datBatchId

    -- * OrderRefund
    , OrderRefund
    , orderRefund
    , oAmount
    , oActor
    , oReason
    , oCreationDate
    , oReasonText

    -- * TestOrderLineItemProduct
    , TestOrderLineItemProduct
    , testOrderLineItemProduct
    , tolipImageLink
    , tolipChannel
    , tolipBrand
    , tolipTargetCountry
    , tolipGtin
    , tolipItemGroupId
    , tolipOfferId
    , tolipPrice
    , tolipVariantAttributes
    , tolipTitle
    , tolipContentLanguage
    , tolipMpn
    , tolipCondition

    -- * AccounttaxCustomBatchResponse
    , AccounttaxCustomBatchResponse
    , accounttaxCustomBatchResponse
    , acbr1Entries
    , acbr1Kind

    -- * InventoryCustomBatchRequestEntry
    , InventoryCustomBatchRequestEntry
    , inventoryCustomBatchRequestEntry
    , iMerchantId
    , iStoreCode
    , iInventory
    , iProductId
    , iBatchId

    -- * AccountsClaimWebsiteResponse
    , AccountsClaimWebsiteResponse
    , accountsClaimWebsiteResponse
    , acwrKind

    -- * OrderAddress
    , OrderAddress
    , orderAddress
    , oaRecipientName
    , oaStreetAddress
    , oaCountry
    , oaPostalCode
    , oaLocality
    , oaIsPostOfficeBox
    , oaFullAddress
    , oaRegion

    -- * ProductUnitPricingBaseMeasure
    , ProductUnitPricingBaseMeasure
    , productUnitPricingBaseMeasure
    , pupbmValue
    , pupbmUnit

    -- * DatafeedsListResponse
    , DatafeedsListResponse
    , datafeedsListResponse
    , dlrNextPageToken
    , dlrKind
    , dlrResources

    -- * ProductsCustomBatchResponseEntry
    , ProductsCustomBatchResponseEntry
    , productsCustomBatchResponseEntry
    , proKind
    , proProduct
    , proErrors
    , proBatchId

    -- * OrderPaymentMethod
    , OrderPaymentMethod
    , orderPaymentMethod
    , opmExpirationMonth
    , opmExpirationYear
    , opmPhoneNumber
    , opmBillingAddress
    , opmLastFourDigits
    , opmType

    -- * Product
    , Product
    , product
    , ppDisplayAdsLink
    , ppCustomLabel1
    , ppShippingWidth
    , ppCustomGroups
    , ppImageLink
    , ppDisplayAdsValue
    , ppLoyaltyPoints
    , ppAdditionalImageLinks
    , ppValidatedDestinations
    , ppColor
    , ppCustomLabel0
    , ppKind
    , ppMinHandlingTime
    , ppMultipack
    , ppPattern
    , ppLink
    , ppSizeSystem
    , ppUnitPricingBaseMeasure
    , ppTaxes
    , ppMaterial
    , ppInstallment
    , ppChannel
    , ppProductType
    , ppIdentifierExists
    , ppOnlineOnly
    , ppBrand
    , ppUnitPricingMeasure
    , ppSalePrice
    , ppShippingLength
    , ppCustomLabel3
    , ppMaxHandlingTime
    , ppWarnings
    , ppAdditionalProductTypes
    , ppAvailability
    , ppTargetCountry
    , ppShippingLabel
    , ppCustomAttributes
    , ppGtin
    , ppAgeGroup
    , ppDisplayAdsTitle
    , ppGender
    , ppDestinations
    , ppExpirationDate
    , ppItemGroupId
    , ppAdwordsGrouping
    , ppSalePriceEffectiveDate
    , ppCustomLabel2
    , ppGoogleProductCategory
    , ppShipping
    , ppAdwordsRedirect
    , ppShippingWeight
    , ppSellOnGoogleQuantity
    , ppShippingHeight
    , ppAvailabilityDate
    , ppOfferId
    , ppId
    , ppAdwordsLabels
    , ppPrice
    , ppPromotionIds
    , ppSizeType
    , ppMobileLink
    , ppTitle
    , ppAdult
    , ppContentLanguage
    , ppAspects
    , ppEnergyEfficiencyClass
    , ppDisplayAdsSimilarIds
    , ppMpn
    , ppCondition
    , ppSizes
    , ppIsBundle
    , ppDescription
    , ppCustomLabel4
    , ppDisplayAdsId

    -- * Errors
    , Errors
    , errors
    , errCode
    , errMessage
    , errErrors

    -- * AccountstatusesCustomBatchResponseEntry
    , AccountstatusesCustomBatchResponseEntry
    , accountstatusesCustomBatchResponseEntry
    , aaAccountStatus
    , aaErrors
    , aaBatchId

    -- * InventorySetResponse
    , InventorySetResponse
    , inventorySetResponse
    , isrKind

    -- * OrdersCancelLineItemResponse
    , OrdersCancelLineItemResponse
    , ordersCancelLineItemResponse
    , oclirKind
    , oclirExecutionStatus

    -- * TestOrderLineItem
    , TestOrderLineItem
    , testOrderLineItem
    , toliQuantityOrdered
    , toliReturnInfo
    , toliShippingDetails
    , toliProduct
    , toliUnitTax

    -- * ProductstatusesCustomBatchRequestEntry
    , ProductstatusesCustomBatchRequestEntry
    , productstatusesCustomBatchRequestEntry
    , pcbrecMerchantId
    , pcbrecMethod
    , pcbrecIncludeAttributes
    , pcbrecProductId
    , pcbrecBatchId

    -- * ShippingSettingsCustomBatchResponse
    , ShippingSettingsCustomBatchResponse
    , shippingSettingsCustomBatchResponse
    , shiEntries
    , shiKind

    -- * OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo
    , OrdersCustomBatchRequestEntryShipLineItemsShipmentInfo
    , ordersCustomBatchRequestEntryShipLineItemsShipmentInfo
    , ocbreslisiCarrier
    , ocbreslisiTrackingId
    , ocbreslisiShipmentId

    -- * ProductAspect
    , ProductAspect
    , productAspect
    , paIntention
    , paAspectName
    , paDestinationName

    -- * DatafeedTarget
    , DatafeedTarget
    , datafeedTarget
    , dtIncludedDestinations
    , dtExcludedDestinations
    , dtCountry
    , dtLanguage

    -- * OrdersUpdateMerchantOrderIdResponse
    , OrdersUpdateMerchantOrderIdResponse
    , ordersUpdateMerchantOrderIdResponse
    , oumoirKind
    , oumoirExecutionStatus

    -- * InventoryPickup
    , InventoryPickup
    , inventoryPickup
    , ipPickupSla
    , ipPickupMethod

    -- * DatafeedStatusExample
    , DatafeedStatusExample
    , datafeedStatusExample
    , dseLineNumber
    , dseItemId
    , dseValue

    -- * OrdersAcknowledgeResponse
    , OrdersAcknowledgeResponse
    , ordersAcknowledgeResponse
    , oarKind
    , oarExecutionStatus

    -- * Table
    , Table
    , table
    , tRows
    , tName
    , tColumnHeaders
    , tRowHeaders

    -- * Order
    , Order
    , order
    , ooStatus
    , ooMerchantId
    , ooRefunds
    , ooKind
    , ooLineItems
    , ooShipments
    , ooNetAmount
    , ooPlacedDate
    , ooDeliveryDetails
    , ooShippingOption
    , ooMerchantOrderId
    , ooAcknowledged
    , ooShippingCostTax
    , ooCustomer
    , ooId
    , ooPaymentMethod
    , ooPromotions
    , ooChannelType
    , ooPaymentStatus
    , ooShippingCost

    -- * InventoryCustomBatchResponse
    , InventoryCustomBatchResponse
    , inventoryCustomBatchResponse
    , invEntries
    , invKind

    -- * OrderLineItemProduct
    , OrderLineItemProduct
    , orderLineItemProduct
    , olipImageLink
    , olipShownImage
    , olipChannel
    , olipBrand
    , olipTargetCountry
    , olipGtin
    , olipItemGroupId
    , olipOfferId
    , olipId
    , olipPrice
    , olipVariantAttributes
    , olipTitle
    , olipContentLanguage
    , olipMpn
    , olipCondition

    -- * AccounttaxCustomBatchRequestEntry
    , AccounttaxCustomBatchRequestEntry
    , accounttaxCustomBatchRequestEntry
    , acccAccountTax
    , acccMerchantId
    , acccAccountId
    , acccMethod
    , acccBatchId

    -- * DatafeedStatusError
    , DatafeedStatusError
    , datafeedStatusError
    , dseCount
    , dseCode
    , dseMessage
    , dseExamples

    -- * ProductsCustomBatchRequest
    , ProductsCustomBatchRequest
    , productsCustomBatchRequest
    , pcbrcEntries

    -- * OrdersCustomBatchRequestEntryReturnLineItem
    , OrdersCustomBatchRequestEntryReturnLineItem
    , ordersCustomBatchRequestEntryReturnLineItem
    , ocbrerliQuantity
    , ocbrerliLineItemId
    , ocbrerliReason
    , ocbrerliProductId
    , ocbrerliReasonText

    -- * OrdersCustomBatchRequestEntryUpdateShipment
    , OrdersCustomBatchRequestEntryUpdateShipment
    , ordersCustomBatchRequestEntryUpdateShipment
    , ocbreusCarrier
    , ocbreusStatus
    , ocbreusTrackingId
    , ocbreusShipmentId

    -- * DatafeedStatus
    , DatafeedStatus
    , datafeedStatus
    , dsItemsTotal
    , dsCountry
    , dsKind
    , dsWarnings
    , dsDatafeedId
    , dsProcessingStatus
    , dsLanguage
    , dsLastUploadDate
    , dsItemsValid
    , dsErrors

    -- * DatafeedstatusesCustomBatchRequest
    , DatafeedstatusesCustomBatchRequest
    , datafeedstatusesCustomBatchRequest
    , dcbrcEntries

    -- * AccountStatusDataQualityIssue
    , AccountStatusDataQualityIssue
    , accountStatusDataQualityIssue
    , asdqiSubmittedValue
    , asdqiLocation
    , asdqiCountry
    , asdqiDisplayedValue
    , asdqiNumItems
    , asdqiSeverity
    , asdqiExampleItems
    , asdqiLastChecked
    , asdqiId
    , asdqiDetail

    -- * ProductShippingDimension
    , ProductShippingDimension
    , productShippingDimension
    , psdValue
    , psdUnit

    -- * DatafeedsCustomBatchResponseEntry
    , DatafeedsCustomBatchResponseEntry
    , datafeedsCustomBatchResponseEntry
    , dcbrecDatafeed
    , dcbrecErrors
    , dcbrecBatchId

    -- * OrdersCustomBatchRequestEntryRefund
    , OrdersCustomBatchRequestEntryRefund
    , ordersCustomBatchRequestEntryRefund
    , ocbrerAmount
    , ocbrerReason
    , ocbrerAmountPretax
    , ocbrerAmountTax
    , ocbrerReasonText

    -- * DatafeedstatusesListResponse
    , DatafeedstatusesListResponse
    , datafeedstatusesListResponse
    , dlrlNextPageToken
    , dlrlKind
    , dlrlResources

    -- * ProductsListResponse
    , ProductsListResponse
    , productsListResponse
    , plrlNextPageToken
    , plrlKind
    , plrlResources

    -- * AccountAdwordsLink
    , AccountAdwordsLink
    , accountAdwordsLink
    , aalStatus
    , aalAdwordsId

    -- * OrderCancellation
    , OrderCancellation
    , orderCancellation
    , ocQuantity
    , ocActor
    , ocReason
    , ocCreationDate
    , ocReasonText

    -- * OrdersCustomBatchResponse
    , OrdersCustomBatchResponse
    , ordersCustomBatchResponse
    , ordEntries
    , ordKind
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types.Product
import Network.Google.ShoppingContent.Types.Sum

-- | Default request referring to version 'v2' of the Content API for Shopping. This contains the host and root path used as a starting point for constructing service requests.
shoppingContentService :: ServiceConfig
shoppingContentService
  = defaultService (ServiceId "content:v2")
      "www.googleapis.com"

-- | Manage your product listings and accounts for Google Shopping
contentScope :: Proxy '["https://www.googleapis.com/auth/content"]
contentScope = Proxy;
