{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.FirebaseDynamicLinks.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.FirebaseDynamicLinks.Types
    (
    -- * Service Configuration
      firebaseDynamicLinksService

    -- * OAuth Scopes
    , firebaseScope

    -- * NavigationInfo
    , NavigationInfo
    , navigationInfo
    , niEnableForcedRedirect

    -- * DesktopInfo
    , DesktopInfo
    , desktopInfo
    , diDesktopFallbackLink

    -- * DynamicLinkEventStatPlatform
    , DynamicLinkEventStatPlatform (..)

    -- * Suffix
    , Suffix
    , suffix
    , sOption

    -- * DynamicLinkWarning
    , DynamicLinkWarning
    , dynamicLinkWarning
    , dlwWarningCode
    , dlwWarningDocumentLink
    , dlwWarningMessage

    -- * CreateShortDynamicLinkRequest
    , CreateShortDynamicLinkRequest
    , createShortDynamicLinkRequest
    , csdlrLongDynamicLink
    , csdlrSuffix
    , csdlrDynamicLinkInfo

    -- * SocialMetaTagInfo
    , SocialMetaTagInfo
    , socialMetaTagInfo
    , smtiSocialImageLink
    , smtiSocialDescription
    , smtiSocialTitle

    -- * CreateShortDynamicLinkResponse
    , CreateShortDynamicLinkResponse
    , createShortDynamicLinkResponse
    , csdlrPreviewLink
    , csdlrWarning
    , csdlrShortLink

    -- * DynamicLinkEventStat
    , DynamicLinkEventStat
    , dynamicLinkEventStat
    , dlesEvent
    , dlesPlatform
    , dlesCount

    -- * IosInfo
    , IosInfo
    , iosInfo
    , iiIosBundleId
    , iiIosIPadBundleId
    , iiIosAppStoreId
    , iiIosIPadFallbackLink
    , iiIosCustomScheme
    , iiIosFallbackLink

    -- * DynamicLinkInfo
    , DynamicLinkInfo
    , dynamicLinkInfo
    , dliNavigationInfo
    , dliDesktopInfo
    , dliSocialMetaTagInfo
    , dliDynamicLinkDomain
    , dliLink
    , dliIosInfo
    , dliAndroidInfo
    , dliAnalyticsInfo

    -- * GetIosPostInstallAttributionRequestVisualStyle
    , GetIosPostInstallAttributionRequestVisualStyle (..)

    -- * DynamicLinkStats
    , DynamicLinkStats
    , dynamicLinkStats
    , dlsLinkEventStats

    -- * SuffixOption
    , SuffixOption (..)

    -- * DynamicLinkEventStatEvent
    , DynamicLinkEventStatEvent (..)

    -- * GetIosPostInstallAttributionRequest
    , GetIosPostInstallAttributionRequest
    , getIosPostInstallAttributionRequest
    , gipiarIosVersion
    , gipiarUniqueMatchLinkToCheck
    , gipiarAppInstallationTime
    , gipiarDevice
    , gipiarSdkVersion
    , gipiarBundleId
    , gipiarRetrievalMethod
    , gipiarVisualStyle

    -- * Xgafv
    , Xgafv (..)

    -- * GetIosPostInstallAttributionResponseAttributionConfidence
    , GetIosPostInstallAttributionResponseAttributionConfidence (..)

    -- * AndroidInfo
    , AndroidInfo
    , androidInfo
    , aiAndroidMinPackageVersionCode
    , aiAndroidFallbackLink
    , aiAndroidLink
    , aiAndroidPackageName

    -- * DynamicLinkWarningWarningCode
    , DynamicLinkWarningWarningCode (..)

    -- * AnalyticsInfo
    , AnalyticsInfo
    , analyticsInfo
    , aiItunesConnectAnalytics
    , aiGooglePlayAnalytics

    -- * ITunesConnectAnalytics
    , ITunesConnectAnalytics
    , iTunesConnectAnalytics
    , itcaAt
    , itcaMt
    , itcaPt
    , itcaCt

    -- * GetIosPostInstallAttributionResponse
    , GetIosPostInstallAttributionResponse
    , getIosPostInstallAttributionResponse
    , gipiarDeepLink
    , gipiarAppMinimumVersion
    , gipiarAttributionConfidence
    , gipiarExternalBrowserDestinationLink
    , gipiarResolvedLink
    , gipiarRequestedLink
    , gipiarUtmMedium
    , gipiarFallbackLink
    , gipiarInvitationId
    , gipiarIsStrongMatchExecutable
    , gipiarUtmCampaign
    , gipiarMatchMessage
    , gipiarUtmSource

    -- * GooglePlayAnalytics
    , GooglePlayAnalytics
    , googlePlayAnalytics
    , gpaUtmContent
    , gpaUtmMedium
    , gpaUtmTerm
    , gpaUtmCampaign
    , gpaGclid
    , gpaUtmSource

    -- * GetIosPostInstallAttributionRequestRetrievalMethod
    , GetIosPostInstallAttributionRequestRetrievalMethod (..)

    -- * DeviceInfo
    , DeviceInfo
    , deviceInfo
    , diScreenResolutionWidth
    , diLanguageCode
    , diDeviceModelName
    , diScreenResolutionHeight
    , diLanguageCodeRaw
    , diTimezone
    ) where

import Network.Google.FirebaseDynamicLinks.Types.Product
import Network.Google.FirebaseDynamicLinks.Types.Sum
import Network.Google.Prelude

-- | Default request referring to version 'v1' of the Firebase Dynamic Links API. This contains the host and root path used as a starting point for constructing service requests.
firebaseDynamicLinksService :: ServiceConfig
firebaseDynamicLinksService
  = defaultService
      (ServiceId "firebasedynamiclinks:v1")
      "firebasedynamiclinks-ipv6.googleapis.com"

-- | View and administer all your Firebase data and settings
firebaseScope :: Proxy '["https://www.googleapis.com/auth/firebase"]
firebaseScope = Proxy;
