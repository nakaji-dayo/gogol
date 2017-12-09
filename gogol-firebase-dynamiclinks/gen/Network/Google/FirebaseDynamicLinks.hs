{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.FirebaseDynamicLinks
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Programmatically creates and manages Firebase Dynamic Links.
--
-- /See:/ <https://firebase.google.com/docs/dynamic-links/ Firebase Dynamic Links API Reference>
module Network.Google.FirebaseDynamicLinks
    (
    -- * Service Configuration
      firebaseDynamicLinksService

    -- * OAuth Scopes
    , firebaseScope

    -- * API Declaration
    , FirebaseDynamicLinksAPI

    -- * Resources

    -- ** firebasedynamiclinks.getLinkStats
    , module Network.Google.Resource.FirebaseDynamicLinks.GetLinkStats

    -- ** firebasedynamiclinks.installAttribution
    , module Network.Google.Resource.FirebaseDynamicLinks.InstallAttribution

    -- ** firebasedynamiclinks.shortLinks.create
    , module Network.Google.Resource.FirebaseDynamicLinks.ShortLinks.Create

    -- * Types

    -- ** NavigationInfo
    , NavigationInfo
    , navigationInfo
    , niEnableForcedRedirect

    -- ** DesktopInfo
    , DesktopInfo
    , desktopInfo
    , diDesktopFallbackLink

    -- ** DynamicLinkEventStatPlatform
    , DynamicLinkEventStatPlatform (..)

    -- ** Suffix
    , Suffix
    , suffix
    , sOption

    -- ** DynamicLinkWarning
    , DynamicLinkWarning
    , dynamicLinkWarning
    , dlwWarningCode
    , dlwWarningDocumentLink
    , dlwWarningMessage

    -- ** CreateShortDynamicLinkRequest
    , CreateShortDynamicLinkRequest
    , createShortDynamicLinkRequest
    , csdlrLongDynamicLink
    , csdlrSuffix
    , csdlrDynamicLinkInfo

    -- ** SocialMetaTagInfo
    , SocialMetaTagInfo
    , socialMetaTagInfo
    , smtiSocialImageLink
    , smtiSocialDescription
    , smtiSocialTitle

    -- ** CreateShortDynamicLinkResponse
    , CreateShortDynamicLinkResponse
    , createShortDynamicLinkResponse
    , csdlrPreviewLink
    , csdlrWarning
    , csdlrShortLink

    -- ** DynamicLinkEventStat
    , DynamicLinkEventStat
    , dynamicLinkEventStat
    , dlesEvent
    , dlesPlatform
    , dlesCount

    -- ** IosInfo
    , IosInfo
    , iosInfo
    , iiIosBundleId
    , iiIosIPadBundleId
    , iiIosAppStoreId
    , iiIosIPadFallbackLink
    , iiIosCustomScheme
    , iiIosFallbackLink

    -- ** DynamicLinkInfo
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

    -- ** GetIosPostInstallAttributionRequestVisualStyle
    , GetIosPostInstallAttributionRequestVisualStyle (..)

    -- ** DynamicLinkStats
    , DynamicLinkStats
    , dynamicLinkStats
    , dlsLinkEventStats

    -- ** SuffixOption
    , SuffixOption (..)

    -- ** DynamicLinkEventStatEvent
    , DynamicLinkEventStatEvent (..)

    -- ** GetIosPostInstallAttributionRequest
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

    -- ** Xgafv
    , Xgafv (..)

    -- ** GetIosPostInstallAttributionResponseAttributionConfidence
    , GetIosPostInstallAttributionResponseAttributionConfidence (..)

    -- ** AndroidInfo
    , AndroidInfo
    , androidInfo
    , aiAndroidMinPackageVersionCode
    , aiAndroidFallbackLink
    , aiAndroidLink
    , aiAndroidPackageName

    -- ** DynamicLinkWarningWarningCode
    , DynamicLinkWarningWarningCode (..)

    -- ** AnalyticsInfo
    , AnalyticsInfo
    , analyticsInfo
    , aiItunesConnectAnalytics
    , aiGooglePlayAnalytics

    -- ** ITunesConnectAnalytics
    , ITunesConnectAnalytics
    , iTunesConnectAnalytics
    , itcaAt
    , itcaMt
    , itcaPt
    , itcaCt

    -- ** GetIosPostInstallAttributionResponse
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

    -- ** GooglePlayAnalytics
    , GooglePlayAnalytics
    , googlePlayAnalytics
    , gpaUtmContent
    , gpaUtmMedium
    , gpaUtmTerm
    , gpaUtmCampaign
    , gpaGclid
    , gpaUtmSource

    -- ** GetIosPostInstallAttributionRequestRetrievalMethod
    , GetIosPostInstallAttributionRequestRetrievalMethod (..)

    -- ** DeviceInfo
    , DeviceInfo
    , deviceInfo
    , diScreenResolutionWidth
    , diLanguageCode
    , diDeviceModelName
    , diScreenResolutionHeight
    , diLanguageCodeRaw
    , diTimezone
    ) where

import Network.Google.Prelude
import Network.Google.FirebaseDynamicLinks.Types
import Network.Google.Resource.FirebaseDynamicLinks.GetLinkStats
import Network.Google.Resource.FirebaseDynamicLinks.InstallAttribution
import Network.Google.Resource.FirebaseDynamicLinks.ShortLinks.Create

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Firebase Dynamic Links API service.
type FirebaseDynamicLinksAPI =
     InstallAttributionResource :<|> GetLinkStatsResource
       :<|> ShortLinksCreateResource
