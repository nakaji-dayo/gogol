{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.FirebaseDynamicLinks.Types.Product
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.FirebaseDynamicLinks.Types.Product where

import Network.Google.FirebaseDynamicLinks.Types.Sum
import Network.Google.Prelude

-- | Information of navigation behavior.
--
-- /See:/ 'navigationInfo' smart constructor.
newtype NavigationInfo = NavigationInfo'
    { _niEnableForcedRedirect :: Maybe Bool
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'NavigationInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niEnableForcedRedirect'
navigationInfo
    :: NavigationInfo
navigationInfo = 
    NavigationInfo'
    { _niEnableForcedRedirect = Nothing
    }

-- | If this option is on, FDL click will be forced to redirect rather than
-- show an interstitial page.
niEnableForcedRedirect :: Lens' NavigationInfo (Maybe Bool)
niEnableForcedRedirect
  = lens _niEnableForcedRedirect
      (\ s a -> s{_niEnableForcedRedirect = a})

instance FromJSON NavigationInfo where
        parseJSON
          = withObject "NavigationInfo"
              (\ o ->
                 NavigationInfo' <$> (o .:? "enableForcedRedirect"))

instance ToJSON NavigationInfo where
        toJSON NavigationInfo'{..}
          = object
              (catMaybes
                 [("enableForcedRedirect" .=) <$>
                    _niEnableForcedRedirect])

-- | Desktop related attributes to the Dynamic Link.
--
-- /See:/ 'desktopInfo' smart constructor.
newtype DesktopInfo = DesktopInfo'
    { _diDesktopFallbackLink :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DesktopInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diDesktopFallbackLink'
desktopInfo
    :: DesktopInfo
desktopInfo = 
    DesktopInfo'
    { _diDesktopFallbackLink = Nothing
    }

-- | Link to open on desktop.
diDesktopFallbackLink :: Lens' DesktopInfo (Maybe Text)
diDesktopFallbackLink
  = lens _diDesktopFallbackLink
      (\ s a -> s{_diDesktopFallbackLink = a})

instance FromJSON DesktopInfo where
        parseJSON
          = withObject "DesktopInfo"
              (\ o ->
                 DesktopInfo' <$> (o .:? "desktopFallbackLink"))

instance ToJSON DesktopInfo where
        toJSON DesktopInfo'{..}
          = object
              (catMaybes
                 [("desktopFallbackLink" .=) <$>
                    _diDesktopFallbackLink])

-- | Short Dynamic Link suffix.
--
-- /See:/ 'suffix' smart constructor.
newtype Suffix = Suffix'
    { _sOption :: Maybe SuffixOption
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'Suffix' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sOption'
suffix
    :: Suffix
suffix = 
    Suffix'
    { _sOption = Nothing
    }

-- | Suffix option.
sOption :: Lens' Suffix (Maybe SuffixOption)
sOption = lens _sOption (\ s a -> s{_sOption = a})

instance FromJSON Suffix where
        parseJSON
          = withObject "Suffix"
              (\ o -> Suffix' <$> (o .:? "option"))

instance ToJSON Suffix where
        toJSON Suffix'{..}
          = object (catMaybes [("option" .=) <$> _sOption])

-- | Dynamic Links warning messages.
--
-- /See:/ 'dynamicLinkWarning' smart constructor.
data DynamicLinkWarning = DynamicLinkWarning'
    { _dlwWarningCode :: !(Maybe DynamicLinkWarningWarningCode)
    , _dlwWarningDocumentLink :: !(Maybe Text)
    , _dlwWarningMessage :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DynamicLinkWarning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlwWarningCode'
--
-- * 'dlwWarningDocumentLink'
--
-- * 'dlwWarningMessage'
dynamicLinkWarning
    :: DynamicLinkWarning
dynamicLinkWarning = 
    DynamicLinkWarning'
    { _dlwWarningCode = Nothing
    , _dlwWarningDocumentLink = Nothing
    , _dlwWarningMessage = Nothing
    }

-- | The warning code.
dlwWarningCode :: Lens' DynamicLinkWarning (Maybe DynamicLinkWarningWarningCode)
dlwWarningCode
  = lens _dlwWarningCode
      (\ s a -> s{_dlwWarningCode = a})

-- | The document describing the warning, and helps resolve.
dlwWarningDocumentLink :: Lens' DynamicLinkWarning (Maybe Text)
dlwWarningDocumentLink
  = lens _dlwWarningDocumentLink
      (\ s a -> s{_dlwWarningDocumentLink = a})

-- | The warning message to help developers improve their requests.
dlwWarningMessage :: Lens' DynamicLinkWarning (Maybe Text)
dlwWarningMessage
  = lens _dlwWarningMessage
      (\ s a -> s{_dlwWarningMessage = a})

instance FromJSON DynamicLinkWarning where
        parseJSON
          = withObject "DynamicLinkWarning"
              (\ o ->
                 DynamicLinkWarning' <$>
                   (o .:? "warningCode") <*>
                     (o .:? "warningDocumentLink")
                     <*> (o .:? "warningMessage"))

instance ToJSON DynamicLinkWarning where
        toJSON DynamicLinkWarning'{..}
          = object
              (catMaybes
                 [("warningCode" .=) <$> _dlwWarningCode,
                  ("warningDocumentLink" .=) <$>
                    _dlwWarningDocumentLink,
                  ("warningMessage" .=) <$> _dlwWarningMessage])

-- | Request to create a short Dynamic Link.
--
-- /See:/ 'createShortDynamicLinkRequest' smart constructor.
data CreateShortDynamicLinkRequest = CreateShortDynamicLinkRequest'
    { _csdlrLongDynamicLink :: !(Maybe Text)
    , _csdlrSuffix :: !(Maybe Suffix)
    , _csdlrDynamicLinkInfo :: !(Maybe DynamicLinkInfo)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateShortDynamicLinkRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdlrLongDynamicLink'
--
-- * 'csdlrSuffix'
--
-- * 'csdlrDynamicLinkInfo'
createShortDynamicLinkRequest
    :: CreateShortDynamicLinkRequest
createShortDynamicLinkRequest = 
    CreateShortDynamicLinkRequest'
    { _csdlrLongDynamicLink = Nothing
    , _csdlrSuffix = Nothing
    , _csdlrDynamicLinkInfo = Nothing
    }

-- | Full long Dynamic Link URL with desired query parameters specified. For
-- example,
-- \"https:\/\/sample.app.goo.gl\/?link=http:\/\/www.google.com&apn=com.sample\",
-- [Learn
-- more](https:\/\/firebase.google.com\/docs\/dynamic-links\/android#create-a-dynamic-link-programmatically).
csdlrLongDynamicLink :: Lens' CreateShortDynamicLinkRequest (Maybe Text)
csdlrLongDynamicLink
  = lens _csdlrLongDynamicLink
      (\ s a -> s{_csdlrLongDynamicLink = a})

-- | Short Dynamic Link suffix. Optional.
csdlrSuffix :: Lens' CreateShortDynamicLinkRequest (Maybe Suffix)
csdlrSuffix
  = lens _csdlrSuffix (\ s a -> s{_csdlrSuffix = a})

-- | Information about the Dynamic Link to be shortened. [Learn
-- more](https:\/\/firebase.google.com\/docs\/dynamic-links\/android#create-a-dynamic-link-programmatically).
csdlrDynamicLinkInfo :: Lens' CreateShortDynamicLinkRequest (Maybe DynamicLinkInfo)
csdlrDynamicLinkInfo
  = lens _csdlrDynamicLinkInfo
      (\ s a -> s{_csdlrDynamicLinkInfo = a})

instance FromJSON CreateShortDynamicLinkRequest where
        parseJSON
          = withObject "CreateShortDynamicLinkRequest"
              (\ o ->
                 CreateShortDynamicLinkRequest' <$>
                   (o .:? "longDynamicLink") <*> (o .:? "suffix") <*>
                     (o .:? "dynamicLinkInfo"))

instance ToJSON CreateShortDynamicLinkRequest where
        toJSON CreateShortDynamicLinkRequest'{..}
          = object
              (catMaybes
                 [("longDynamicLink" .=) <$> _csdlrLongDynamicLink,
                  ("suffix" .=) <$> _csdlrSuffix,
                  ("dynamicLinkInfo" .=) <$> _csdlrDynamicLinkInfo])

-- | Parameters for social meta tag params. Used to set meta tag data for
-- link previews on social sites.
--
-- /See:/ 'socialMetaTagInfo' smart constructor.
data SocialMetaTagInfo = SocialMetaTagInfo'
    { _smtiSocialImageLink :: !(Maybe Text)
    , _smtiSocialDescription :: !(Maybe Text)
    , _smtiSocialTitle :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SocialMetaTagInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smtiSocialImageLink'
--
-- * 'smtiSocialDescription'
--
-- * 'smtiSocialTitle'
socialMetaTagInfo
    :: SocialMetaTagInfo
socialMetaTagInfo = 
    SocialMetaTagInfo'
    { _smtiSocialImageLink = Nothing
    , _smtiSocialDescription = Nothing
    , _smtiSocialTitle = Nothing
    }

-- | An image url string. Optional.
smtiSocialImageLink :: Lens' SocialMetaTagInfo (Maybe Text)
smtiSocialImageLink
  = lens _smtiSocialImageLink
      (\ s a -> s{_smtiSocialImageLink = a})

-- | A short description of the link. Optional.
smtiSocialDescription :: Lens' SocialMetaTagInfo (Maybe Text)
smtiSocialDescription
  = lens _smtiSocialDescription
      (\ s a -> s{_smtiSocialDescription = a})

-- | Title to be displayed. Optional.
smtiSocialTitle :: Lens' SocialMetaTagInfo (Maybe Text)
smtiSocialTitle
  = lens _smtiSocialTitle
      (\ s a -> s{_smtiSocialTitle = a})

instance FromJSON SocialMetaTagInfo where
        parseJSON
          = withObject "SocialMetaTagInfo"
              (\ o ->
                 SocialMetaTagInfo' <$>
                   (o .:? "socialImageLink") <*>
                     (o .:? "socialDescription")
                     <*> (o .:? "socialTitle"))

instance ToJSON SocialMetaTagInfo where
        toJSON SocialMetaTagInfo'{..}
          = object
              (catMaybes
                 [("socialImageLink" .=) <$> _smtiSocialImageLink,
                  ("socialDescription" .=) <$> _smtiSocialDescription,
                  ("socialTitle" .=) <$> _smtiSocialTitle])

-- | Response to create a short Dynamic Link.
--
-- /See:/ 'createShortDynamicLinkResponse' smart constructor.
data CreateShortDynamicLinkResponse = CreateShortDynamicLinkResponse'
    { _csdlrPreviewLink :: !(Maybe Text)
    , _csdlrWarning :: !(Maybe [DynamicLinkWarning])
    , _csdlrShortLink :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateShortDynamicLinkResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdlrPreviewLink'
--
-- * 'csdlrWarning'
--
-- * 'csdlrShortLink'
createShortDynamicLinkResponse
    :: CreateShortDynamicLinkResponse
createShortDynamicLinkResponse = 
    CreateShortDynamicLinkResponse'
    { _csdlrPreviewLink = Nothing
    , _csdlrWarning = Nothing
    , _csdlrShortLink = Nothing
    }

-- | Preivew link to show the link flow chart.
csdlrPreviewLink :: Lens' CreateShortDynamicLinkResponse (Maybe Text)
csdlrPreviewLink
  = lens _csdlrPreviewLink
      (\ s a -> s{_csdlrPreviewLink = a})

-- | Information about potential warnings on link creation.
csdlrWarning :: Lens' CreateShortDynamicLinkResponse [DynamicLinkWarning]
csdlrWarning
  = lens _csdlrWarning (\ s a -> s{_csdlrWarning = a})
      . _Default
      . _Coerce

-- | Short Dynamic Link value. e.g. https:\/\/abcd.app.goo.gl\/wxyz
csdlrShortLink :: Lens' CreateShortDynamicLinkResponse (Maybe Text)
csdlrShortLink
  = lens _csdlrShortLink
      (\ s a -> s{_csdlrShortLink = a})

instance FromJSON CreateShortDynamicLinkResponse
         where
        parseJSON
          = withObject "CreateShortDynamicLinkResponse"
              (\ o ->
                 CreateShortDynamicLinkResponse' <$>
                   (o .:? "previewLink") <*>
                     (o .:? "warning" .!= mempty)
                     <*> (o .:? "shortLink"))

instance ToJSON CreateShortDynamicLinkResponse where
        toJSON CreateShortDynamicLinkResponse'{..}
          = object
              (catMaybes
                 [("previewLink" .=) <$> _csdlrPreviewLink,
                  ("warning" .=) <$> _csdlrWarning,
                  ("shortLink" .=) <$> _csdlrShortLink])

-- | Dynamic Link event stat.
--
-- /See:/ 'dynamicLinkEventStat' smart constructor.
data DynamicLinkEventStat = DynamicLinkEventStat'
    { _dlesEvent :: !(Maybe DynamicLinkEventStatEvent)
    , _dlesPlatform :: !(Maybe DynamicLinkEventStatPlatform)
    , _dlesCount :: !(Maybe (Textual Int64))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DynamicLinkEventStat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlesEvent'
--
-- * 'dlesPlatform'
--
-- * 'dlesCount'
dynamicLinkEventStat
    :: DynamicLinkEventStat
dynamicLinkEventStat = 
    DynamicLinkEventStat'
    { _dlesEvent = Nothing
    , _dlesPlatform = Nothing
    , _dlesCount = Nothing
    }

-- | Link event.
dlesEvent :: Lens' DynamicLinkEventStat (Maybe DynamicLinkEventStatEvent)
dlesEvent
  = lens _dlesEvent (\ s a -> s{_dlesEvent = a})

-- | Requested platform.
dlesPlatform :: Lens' DynamicLinkEventStat (Maybe DynamicLinkEventStatPlatform)
dlesPlatform
  = lens _dlesPlatform (\ s a -> s{_dlesPlatform = a})

-- | The number of times this event occurred.
dlesCount :: Lens' DynamicLinkEventStat (Maybe Int64)
dlesCount
  = lens _dlesCount (\ s a -> s{_dlesCount = a}) .
      mapping _Coerce

instance FromJSON DynamicLinkEventStat where
        parseJSON
          = withObject "DynamicLinkEventStat"
              (\ o ->
                 DynamicLinkEventStat' <$>
                   (o .:? "event") <*> (o .:? "platform") <*>
                     (o .:? "count"))

instance ToJSON DynamicLinkEventStat where
        toJSON DynamicLinkEventStat'{..}
          = object
              (catMaybes
                 [("event" .=) <$> _dlesEvent,
                  ("platform" .=) <$> _dlesPlatform,
                  ("count" .=) <$> _dlesCount])

-- | iOS related attributes to the Dynamic Link..
--
-- /See:/ 'iosInfo' smart constructor.
data IosInfo = IosInfo'
    { _iiIosBundleId :: !(Maybe Text)
    , _iiIosIPadBundleId :: !(Maybe Text)
    , _iiIosAppStoreId :: !(Maybe Text)
    , _iiIosIPadFallbackLink :: !(Maybe Text)
    , _iiIosCustomScheme :: !(Maybe Text)
    , _iiIosFallbackLink :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'IosInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiIosBundleId'
--
-- * 'iiIosIPadBundleId'
--
-- * 'iiIosAppStoreId'
--
-- * 'iiIosIPadFallbackLink'
--
-- * 'iiIosCustomScheme'
--
-- * 'iiIosFallbackLink'
iosInfo
    :: IosInfo
iosInfo = 
    IosInfo'
    { _iiIosBundleId = Nothing
    , _iiIosIPadBundleId = Nothing
    , _iiIosAppStoreId = Nothing
    , _iiIosIPadFallbackLink = Nothing
    , _iiIosCustomScheme = Nothing
    , _iiIosFallbackLink = Nothing
    }

-- | iOS bundle ID of the app.
iiIosBundleId :: Lens' IosInfo (Maybe Text)
iiIosBundleId
  = lens _iiIosBundleId
      (\ s a -> s{_iiIosBundleId = a})

-- | iPad bundle ID of the app.
iiIosIPadBundleId :: Lens' IosInfo (Maybe Text)
iiIosIPadBundleId
  = lens _iiIosIPadBundleId
      (\ s a -> s{_iiIosIPadBundleId = a})

-- | iOS App Store ID.
iiIosAppStoreId :: Lens' IosInfo (Maybe Text)
iiIosAppStoreId
  = lens _iiIosAppStoreId
      (\ s a -> s{_iiIosAppStoreId = a})

-- | If specified, this overrides the ios_fallback_link value on iPads.
iiIosIPadFallbackLink :: Lens' IosInfo (Maybe Text)
iiIosIPadFallbackLink
  = lens _iiIosIPadFallbackLink
      (\ s a -> s{_iiIosIPadFallbackLink = a})

-- | Custom (destination) scheme to use for iOS. By default, we’ll use the
-- bundle ID as the custom scheme. Developer can override this behavior
-- using this param.
iiIosCustomScheme :: Lens' IosInfo (Maybe Text)
iiIosCustomScheme
  = lens _iiIosCustomScheme
      (\ s a -> s{_iiIosCustomScheme = a})

-- | Link to open on iOS if the app is not installed.
iiIosFallbackLink :: Lens' IosInfo (Maybe Text)
iiIosFallbackLink
  = lens _iiIosFallbackLink
      (\ s a -> s{_iiIosFallbackLink = a})

instance FromJSON IosInfo where
        parseJSON
          = withObject "IosInfo"
              (\ o ->
                 IosInfo' <$>
                   (o .:? "iosBundleId") <*> (o .:? "iosIpadBundleId")
                     <*> (o .:? "iosAppStoreId")
                     <*> (o .:? "iosIpadFallbackLink")
                     <*> (o .:? "iosCustomScheme")
                     <*> (o .:? "iosFallbackLink"))

instance ToJSON IosInfo where
        toJSON IosInfo'{..}
          = object
              (catMaybes
                 [("iosBundleId" .=) <$> _iiIosBundleId,
                  ("iosIpadBundleId" .=) <$> _iiIosIPadBundleId,
                  ("iosAppStoreId" .=) <$> _iiIosAppStoreId,
                  ("iosIpadFallbackLink" .=) <$>
                    _iiIosIPadFallbackLink,
                  ("iosCustomScheme" .=) <$> _iiIosCustomScheme,
                  ("iosFallbackLink" .=) <$> _iiIosFallbackLink])

-- | Information about a Dynamic Link.
--
-- /See:/ 'dynamicLinkInfo' smart constructor.
data DynamicLinkInfo = DynamicLinkInfo'
    { _dliNavigationInfo :: !(Maybe NavigationInfo)
    , _dliDesktopInfo :: !(Maybe DesktopInfo)
    , _dliSocialMetaTagInfo :: !(Maybe SocialMetaTagInfo)
    , _dliDynamicLinkDomain :: !(Maybe Text)
    , _dliLink :: !(Maybe Text)
    , _dliIosInfo :: !(Maybe IosInfo)
    , _dliAndroidInfo :: !(Maybe AndroidInfo)
    , _dliAnalyticsInfo :: !(Maybe AnalyticsInfo)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DynamicLinkInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dliNavigationInfo'
--
-- * 'dliDesktopInfo'
--
-- * 'dliSocialMetaTagInfo'
--
-- * 'dliDynamicLinkDomain'
--
-- * 'dliLink'
--
-- * 'dliIosInfo'
--
-- * 'dliAndroidInfo'
--
-- * 'dliAnalyticsInfo'
dynamicLinkInfo
    :: DynamicLinkInfo
dynamicLinkInfo = 
    DynamicLinkInfo'
    { _dliNavigationInfo = Nothing
    , _dliDesktopInfo = Nothing
    , _dliSocialMetaTagInfo = Nothing
    , _dliDynamicLinkDomain = Nothing
    , _dliLink = Nothing
    , _dliIosInfo = Nothing
    , _dliAndroidInfo = Nothing
    , _dliAnalyticsInfo = Nothing
    }

-- | Information of navigation behavior of a Firebase Dynamic Links.
dliNavigationInfo :: Lens' DynamicLinkInfo (Maybe NavigationInfo)
dliNavigationInfo
  = lens _dliNavigationInfo
      (\ s a -> s{_dliNavigationInfo = a})

-- | Desktop related information. See desktop related parameters in the
-- [documentation](https:\/\/firebase.google.com\/docs\/dynamic-links\/create-manually).
dliDesktopInfo :: Lens' DynamicLinkInfo (Maybe DesktopInfo)
dliDesktopInfo
  = lens _dliDesktopInfo
      (\ s a -> s{_dliDesktopInfo = a})

-- | Parameters for social meta tag params. Used to set meta tag data for
-- link previews on social sites.
dliSocialMetaTagInfo :: Lens' DynamicLinkInfo (Maybe SocialMetaTagInfo)
dliSocialMetaTagInfo
  = lens _dliSocialMetaTagInfo
      (\ s a -> s{_dliSocialMetaTagInfo = a})

-- | Dynamic Links domain that the project owns, e.g. abcd.app.goo.gl [Learn
-- more](https:\/\/firebase.google.com\/docs\/dynamic-links\/android\/receive)
-- on how to set up Dynamic Link domain associated with your Firebase
-- project. Required.
dliDynamicLinkDomain :: Lens' DynamicLinkInfo (Maybe Text)
dliDynamicLinkDomain
  = lens _dliDynamicLinkDomain
      (\ s a -> s{_dliDynamicLinkDomain = a})

-- | The link your app will open, You can specify any URL your app can
-- handle. This link must be a well-formatted URL, be properly URL-encoded,
-- and use the HTTP or HTTPS scheme. See \'link\' parameters in the
-- [documentation](https:\/\/firebase.google.com\/docs\/dynamic-links\/create-manually).
-- Required.
dliLink :: Lens' DynamicLinkInfo (Maybe Text)
dliLink = lens _dliLink (\ s a -> s{_dliLink = a})

-- | iOS related information. See iOS related parameters in the
-- [documentation](https:\/\/firebase.google.com\/docs\/dynamic-links\/create-manually).
dliIosInfo :: Lens' DynamicLinkInfo (Maybe IosInfo)
dliIosInfo
  = lens _dliIosInfo (\ s a -> s{_dliIosInfo = a})

-- | Android related information. See Android related parameters in the
-- [documentation](https:\/\/firebase.google.com\/docs\/dynamic-links\/create-manually).
dliAndroidInfo :: Lens' DynamicLinkInfo (Maybe AndroidInfo)
dliAndroidInfo
  = lens _dliAndroidInfo
      (\ s a -> s{_dliAndroidInfo = a})

-- | Parameters used for tracking. See all tracking parameters in the
-- [documentation](https:\/\/firebase.google.com\/docs\/dynamic-links\/create-manually).
dliAnalyticsInfo :: Lens' DynamicLinkInfo (Maybe AnalyticsInfo)
dliAnalyticsInfo
  = lens _dliAnalyticsInfo
      (\ s a -> s{_dliAnalyticsInfo = a})

instance FromJSON DynamicLinkInfo where
        parseJSON
          = withObject "DynamicLinkInfo"
              (\ o ->
                 DynamicLinkInfo' <$>
                   (o .:? "navigationInfo") <*> (o .:? "desktopInfo")
                     <*> (o .:? "socialMetaTagInfo")
                     <*> (o .:? "dynamicLinkDomain")
                     <*> (o .:? "link")
                     <*> (o .:? "iosInfo")
                     <*> (o .:? "androidInfo")
                     <*> (o .:? "analyticsInfo"))

instance ToJSON DynamicLinkInfo where
        toJSON DynamicLinkInfo'{..}
          = object
              (catMaybes
                 [("navigationInfo" .=) <$> _dliNavigationInfo,
                  ("desktopInfo" .=) <$> _dliDesktopInfo,
                  ("socialMetaTagInfo" .=) <$> _dliSocialMetaTagInfo,
                  ("dynamicLinkDomain" .=) <$> _dliDynamicLinkDomain,
                  ("link" .=) <$> _dliLink,
                  ("iosInfo" .=) <$> _dliIosInfo,
                  ("androidInfo" .=) <$> _dliAndroidInfo,
                  ("analyticsInfo" .=) <$> _dliAnalyticsInfo])

-- | Analytics stats of a Dynamic Link for a given timeframe.
--
-- /See:/ 'dynamicLinkStats' smart constructor.
newtype DynamicLinkStats = DynamicLinkStats'
    { _dlsLinkEventStats :: Maybe [DynamicLinkEventStat]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DynamicLinkStats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsLinkEventStats'
dynamicLinkStats
    :: DynamicLinkStats
dynamicLinkStats = 
    DynamicLinkStats'
    { _dlsLinkEventStats = Nothing
    }

-- | Dynamic Link event stats.
dlsLinkEventStats :: Lens' DynamicLinkStats [DynamicLinkEventStat]
dlsLinkEventStats
  = lens _dlsLinkEventStats
      (\ s a -> s{_dlsLinkEventStats = a})
      . _Default
      . _Coerce

instance FromJSON DynamicLinkStats where
        parseJSON
          = withObject "DynamicLinkStats"
              (\ o ->
                 DynamicLinkStats' <$>
                   (o .:? "linkEventStats" .!= mempty))

instance ToJSON DynamicLinkStats where
        toJSON DynamicLinkStats'{..}
          = object
              (catMaybes
                 [("linkEventStats" .=) <$> _dlsLinkEventStats])

-- | Request for iSDK to execute strong match flow for post-install
-- attribution. This is meant for iOS requests only. Requests from other
-- platforms will not be honored.
--
-- /See:/ 'getIosPostInstallAttributionRequest' smart constructor.
data GetIosPostInstallAttributionRequest = GetIosPostInstallAttributionRequest'
    { _gipiarIosVersion :: !(Maybe Text)
    , _gipiarUniqueMatchLinkToCheck :: !(Maybe Text)
    , _gipiarAppInstallationTime :: !(Maybe (Textual Int64))
    , _gipiarDevice :: !(Maybe DeviceInfo)
    , _gipiarSdkVersion :: !(Maybe Text)
    , _gipiarBundleId :: !(Maybe Text)
    , _gipiarRetrievalMethod :: !(Maybe GetIosPostInstallAttributionRequestRetrievalMethod)
    , _gipiarVisualStyle :: !(Maybe GetIosPostInstallAttributionRequestVisualStyle)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetIosPostInstallAttributionRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gipiarIosVersion'
--
-- * 'gipiarUniqueMatchLinkToCheck'
--
-- * 'gipiarAppInstallationTime'
--
-- * 'gipiarDevice'
--
-- * 'gipiarSdkVersion'
--
-- * 'gipiarBundleId'
--
-- * 'gipiarRetrievalMethod'
--
-- * 'gipiarVisualStyle'
getIosPostInstallAttributionRequest
    :: GetIosPostInstallAttributionRequest
getIosPostInstallAttributionRequest = 
    GetIosPostInstallAttributionRequest'
    { _gipiarIosVersion = Nothing
    , _gipiarUniqueMatchLinkToCheck = Nothing
    , _gipiarAppInstallationTime = Nothing
    , _gipiarDevice = Nothing
    , _gipiarSdkVersion = Nothing
    , _gipiarBundleId = Nothing
    , _gipiarRetrievalMethod = Nothing
    , _gipiarVisualStyle = Nothing
    }

-- | iOS version, ie: 9.3.5. Consider adding \"build\".
gipiarIosVersion :: Lens' GetIosPostInstallAttributionRequest (Maybe Text)
gipiarIosVersion
  = lens _gipiarIosVersion
      (\ s a -> s{_gipiarIosVersion = a})

-- | Possible unique matched link that server need to check before performing
-- fingerprint match. If passed link is short server need to expand the
-- link. If link is long server need to vslidate the link.
gipiarUniqueMatchLinkToCheck :: Lens' GetIosPostInstallAttributionRequest (Maybe Text)
gipiarUniqueMatchLinkToCheck
  = lens _gipiarUniqueMatchLinkToCheck
      (\ s a -> s{_gipiarUniqueMatchLinkToCheck = a})

-- | App installation epoch time
-- (https:\/\/en.wikipedia.org\/wiki\/Unix_time). This is a client signal
-- for a more accurate weak match.
gipiarAppInstallationTime :: Lens' GetIosPostInstallAttributionRequest (Maybe Int64)
gipiarAppInstallationTime
  = lens _gipiarAppInstallationTime
      (\ s a -> s{_gipiarAppInstallationTime = a})
      . mapping _Coerce

-- | Device information.
gipiarDevice :: Lens' GetIosPostInstallAttributionRequest (Maybe DeviceInfo)
gipiarDevice
  = lens _gipiarDevice (\ s a -> s{_gipiarDevice = a})

-- | Google SDK version.
gipiarSdkVersion :: Lens' GetIosPostInstallAttributionRequest (Maybe Text)
gipiarSdkVersion
  = lens _gipiarSdkVersion
      (\ s a -> s{_gipiarSdkVersion = a})

-- | APP bundle ID.
gipiarBundleId :: Lens' GetIosPostInstallAttributionRequest (Maybe Text)
gipiarBundleId
  = lens _gipiarBundleId
      (\ s a -> s{_gipiarBundleId = a})

-- | App post install attribution retrieval information. Disambiguates
-- mechanism (iSDK or developer invoked) to retrieve payload from clicked
-- link.
gipiarRetrievalMethod :: Lens' GetIosPostInstallAttributionRequest (Maybe GetIosPostInstallAttributionRequestRetrievalMethod)
gipiarRetrievalMethod
  = lens _gipiarRetrievalMethod
      (\ s a -> s{_gipiarRetrievalMethod = a})

-- | Strong match page information. Disambiguates between default UI and
-- custom page to present when strong match succeeds\/fails to find cookie.
gipiarVisualStyle :: Lens' GetIosPostInstallAttributionRequest (Maybe GetIosPostInstallAttributionRequestVisualStyle)
gipiarVisualStyle
  = lens _gipiarVisualStyle
      (\ s a -> s{_gipiarVisualStyle = a})

instance FromJSON GetIosPostInstallAttributionRequest
         where
        parseJSON
          = withObject "GetIosPostInstallAttributionRequest"
              (\ o ->
                 GetIosPostInstallAttributionRequest' <$>
                   (o .:? "iosVersion") <*>
                     (o .:? "uniqueMatchLinkToCheck")
                     <*> (o .:? "appInstallationTime")
                     <*> (o .:? "device")
                     <*> (o .:? "sdkVersion")
                     <*> (o .:? "bundleId")
                     <*> (o .:? "retrievalMethod")
                     <*> (o .:? "visualStyle"))

instance ToJSON GetIosPostInstallAttributionRequest
         where
        toJSON GetIosPostInstallAttributionRequest'{..}
          = object
              (catMaybes
                 [("iosVersion" .=) <$> _gipiarIosVersion,
                  ("uniqueMatchLinkToCheck" .=) <$>
                    _gipiarUniqueMatchLinkToCheck,
                  ("appInstallationTime" .=) <$>
                    _gipiarAppInstallationTime,
                  ("device" .=) <$> _gipiarDevice,
                  ("sdkVersion" .=) <$> _gipiarSdkVersion,
                  ("bundleId" .=) <$> _gipiarBundleId,
                  ("retrievalMethod" .=) <$> _gipiarRetrievalMethod,
                  ("visualStyle" .=) <$> _gipiarVisualStyle])

-- | Android related attributes to the Dynamic Link.
--
-- /See:/ 'androidInfo' smart constructor.
data AndroidInfo = AndroidInfo'
    { _aiAndroidMinPackageVersionCode :: !(Maybe Text)
    , _aiAndroidFallbackLink :: !(Maybe Text)
    , _aiAndroidLink :: !(Maybe Text)
    , _aiAndroidPackageName :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AndroidInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiAndroidMinPackageVersionCode'
--
-- * 'aiAndroidFallbackLink'
--
-- * 'aiAndroidLink'
--
-- * 'aiAndroidPackageName'
androidInfo
    :: AndroidInfo
androidInfo = 
    AndroidInfo'
    { _aiAndroidMinPackageVersionCode = Nothing
    , _aiAndroidFallbackLink = Nothing
    , _aiAndroidLink = Nothing
    , _aiAndroidPackageName = Nothing
    }

-- | Minimum version code for the Android app. If the installed app’s version
-- code is lower, then the user is taken to the Play Store.
aiAndroidMinPackageVersionCode :: Lens' AndroidInfo (Maybe Text)
aiAndroidMinPackageVersionCode
  = lens _aiAndroidMinPackageVersionCode
      (\ s a -> s{_aiAndroidMinPackageVersionCode = a})

-- | Link to open on Android if the app is not installed.
aiAndroidFallbackLink :: Lens' AndroidInfo (Maybe Text)
aiAndroidFallbackLink
  = lens _aiAndroidFallbackLink
      (\ s a -> s{_aiAndroidFallbackLink = a})

-- | If specified, this overrides the ‘link’ parameter on Android.
aiAndroidLink :: Lens' AndroidInfo (Maybe Text)
aiAndroidLink
  = lens _aiAndroidLink
      (\ s a -> s{_aiAndroidLink = a})

-- | Android package name of the app.
aiAndroidPackageName :: Lens' AndroidInfo (Maybe Text)
aiAndroidPackageName
  = lens _aiAndroidPackageName
      (\ s a -> s{_aiAndroidPackageName = a})

instance FromJSON AndroidInfo where
        parseJSON
          = withObject "AndroidInfo"
              (\ o ->
                 AndroidInfo' <$>
                   (o .:? "androidMinPackageVersionCode") <*>
                     (o .:? "androidFallbackLink")
                     <*> (o .:? "androidLink")
                     <*> (o .:? "androidPackageName"))

instance ToJSON AndroidInfo where
        toJSON AndroidInfo'{..}
          = object
              (catMaybes
                 [("androidMinPackageVersionCode" .=) <$>
                    _aiAndroidMinPackageVersionCode,
                  ("androidFallbackLink" .=) <$>
                    _aiAndroidFallbackLink,
                  ("androidLink" .=) <$> _aiAndroidLink,
                  ("androidPackageName" .=) <$> _aiAndroidPackageName])

-- | Tracking parameters supported by Dynamic Link.
--
-- /See:/ 'analyticsInfo' smart constructor.
data AnalyticsInfo = AnalyticsInfo'
    { _aiItunesConnectAnalytics :: !(Maybe ITunesConnectAnalytics)
    , _aiGooglePlayAnalytics :: !(Maybe GooglePlayAnalytics)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'AnalyticsInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiItunesConnectAnalytics'
--
-- * 'aiGooglePlayAnalytics'
analyticsInfo
    :: AnalyticsInfo
analyticsInfo = 
    AnalyticsInfo'
    { _aiItunesConnectAnalytics = Nothing
    , _aiGooglePlayAnalytics = Nothing
    }

-- | iTunes Connect App Analytics.
aiItunesConnectAnalytics :: Lens' AnalyticsInfo (Maybe ITunesConnectAnalytics)
aiItunesConnectAnalytics
  = lens _aiItunesConnectAnalytics
      (\ s a -> s{_aiItunesConnectAnalytics = a})

-- | Google Play Campaign Measurements.
aiGooglePlayAnalytics :: Lens' AnalyticsInfo (Maybe GooglePlayAnalytics)
aiGooglePlayAnalytics
  = lens _aiGooglePlayAnalytics
      (\ s a -> s{_aiGooglePlayAnalytics = a})

instance FromJSON AnalyticsInfo where
        parseJSON
          = withObject "AnalyticsInfo"
              (\ o ->
                 AnalyticsInfo' <$>
                   (o .:? "itunesConnectAnalytics") <*>
                     (o .:? "googlePlayAnalytics"))

instance ToJSON AnalyticsInfo where
        toJSON AnalyticsInfo'{..}
          = object
              (catMaybes
                 [("itunesConnectAnalytics" .=) <$>
                    _aiItunesConnectAnalytics,
                  ("googlePlayAnalytics" .=) <$>
                    _aiGooglePlayAnalytics])

-- | Parameters for iTunes Connect App Analytics.
--
-- /See:/ 'iTunesConnectAnalytics' smart constructor.
data ITunesConnectAnalytics = ITunesConnectAnalytics'
    { _itcaAt :: !(Maybe Text)
    , _itcaMt :: !(Maybe Text)
    , _itcaPt :: !(Maybe Text)
    , _itcaCt :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ITunesConnectAnalytics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itcaAt'
--
-- * 'itcaMt'
--
-- * 'itcaPt'
--
-- * 'itcaCt'
iTunesConnectAnalytics
    :: ITunesConnectAnalytics
iTunesConnectAnalytics = 
    ITunesConnectAnalytics'
    { _itcaAt = Nothing
    , _itcaMt = Nothing
    , _itcaPt = Nothing
    , _itcaCt = Nothing
    }

-- | Affiliate token used to create affiliate-coded links.
itcaAt :: Lens' ITunesConnectAnalytics (Maybe Text)
itcaAt = lens _itcaAt (\ s a -> s{_itcaAt = a})

-- | iTune media types, including music, podcasts, audiobooks and so on.
itcaMt :: Lens' ITunesConnectAnalytics (Maybe Text)
itcaMt = lens _itcaMt (\ s a -> s{_itcaMt = a})

-- | Provider token that enables analytics for Dynamic Links from within
-- iTunes Connect.
itcaPt :: Lens' ITunesConnectAnalytics (Maybe Text)
itcaPt = lens _itcaPt (\ s a -> s{_itcaPt = a})

-- | Campaign text that developers can optionally add to any link in order to
-- track sales from a specific marketing campaign.
itcaCt :: Lens' ITunesConnectAnalytics (Maybe Text)
itcaCt = lens _itcaCt (\ s a -> s{_itcaCt = a})

instance FromJSON ITunesConnectAnalytics where
        parseJSON
          = withObject "ITunesConnectAnalytics"
              (\ o ->
                 ITunesConnectAnalytics' <$>
                   (o .:? "at") <*> (o .:? "mt") <*> (o .:? "pt") <*>
                     (o .:? "ct"))

instance ToJSON ITunesConnectAnalytics where
        toJSON ITunesConnectAnalytics'{..}
          = object
              (catMaybes
                 [("at" .=) <$> _itcaAt, ("mt" .=) <$> _itcaMt,
                  ("pt" .=) <$> _itcaPt, ("ct" .=) <$> _itcaCt])

-- | Response for iSDK to execute strong match flow for post-install
-- attribution.
--
-- /See:/ 'getIosPostInstallAttributionResponse' smart constructor.
data GetIosPostInstallAttributionResponse = GetIosPostInstallAttributionResponse'
    { _gipiarDeepLink :: !(Maybe Text)
    , _gipiarAppMinimumVersion :: !(Maybe Text)
    , _gipiarAttributionConfidence :: !(Maybe GetIosPostInstallAttributionResponseAttributionConfidence)
    , _gipiarExternalBrowserDestinationLink :: !(Maybe Text)
    , _gipiarResolvedLink :: !(Maybe Text)
    , _gipiarRequestedLink :: !(Maybe Text)
    , _gipiarUtmMedium :: !(Maybe Text)
    , _gipiarFallbackLink :: !(Maybe Text)
    , _gipiarInvitationId :: !(Maybe Text)
    , _gipiarIsStrongMatchExecutable :: !(Maybe Bool)
    , _gipiarUtmCampaign :: !(Maybe Text)
    , _gipiarMatchMessage :: !(Maybe Text)
    , _gipiarUtmSource :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetIosPostInstallAttributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gipiarDeepLink'
--
-- * 'gipiarAppMinimumVersion'
--
-- * 'gipiarAttributionConfidence'
--
-- * 'gipiarExternalBrowserDestinationLink'
--
-- * 'gipiarResolvedLink'
--
-- * 'gipiarRequestedLink'
--
-- * 'gipiarUtmMedium'
--
-- * 'gipiarFallbackLink'
--
-- * 'gipiarInvitationId'
--
-- * 'gipiarIsStrongMatchExecutable'
--
-- * 'gipiarUtmCampaign'
--
-- * 'gipiarMatchMessage'
--
-- * 'gipiarUtmSource'
getIosPostInstallAttributionResponse
    :: GetIosPostInstallAttributionResponse
getIosPostInstallAttributionResponse = 
    GetIosPostInstallAttributionResponse'
    { _gipiarDeepLink = Nothing
    , _gipiarAppMinimumVersion = Nothing
    , _gipiarAttributionConfidence = Nothing
    , _gipiarExternalBrowserDestinationLink = Nothing
    , _gipiarResolvedLink = Nothing
    , _gipiarRequestedLink = Nothing
    , _gipiarUtmMedium = Nothing
    , _gipiarFallbackLink = Nothing
    , _gipiarInvitationId = Nothing
    , _gipiarIsStrongMatchExecutable = Nothing
    , _gipiarUtmCampaign = Nothing
    , _gipiarMatchMessage = Nothing
    , _gipiarUtmSource = Nothing
    }

-- | The deep-link attributed post-install via one of several techniques
-- (fingerprint, copy unique).
gipiarDeepLink :: Lens' GetIosPostInstallAttributionResponse (Maybe Text)
gipiarDeepLink
  = lens _gipiarDeepLink
      (\ s a -> s{_gipiarDeepLink = a})

-- | The minimum version for app, specified by dev through ?imv= parameter.
-- Return to iSDK to allow app to evaluate if current version meets this.
gipiarAppMinimumVersion :: Lens' GetIosPostInstallAttributionResponse (Maybe Text)
gipiarAppMinimumVersion
  = lens _gipiarAppMinimumVersion
      (\ s a -> s{_gipiarAppMinimumVersion = a})

-- | The confidence of the returned attribution.
gipiarAttributionConfidence :: Lens' GetIosPostInstallAttributionResponse (Maybe GetIosPostInstallAttributionResponseAttributionConfidence)
gipiarAttributionConfidence
  = lens _gipiarAttributionConfidence
      (\ s a -> s{_gipiarAttributionConfidence = a})

-- | User-agent specific custom-scheme URIs for iSDK to open. This will be
-- set according to the user-agent tha the click was originally made in.
-- There is no Safari-equivalent custom-scheme open URLs. ie:
-- googlechrome:\/\/www.example.com ie:
-- firefox:\/\/open-url?url=http:\/\/www.example.com ie:
-- opera-http:\/\/example.com
gipiarExternalBrowserDestinationLink :: Lens' GetIosPostInstallAttributionResponse (Maybe Text)
gipiarExternalBrowserDestinationLink
  = lens _gipiarExternalBrowserDestinationLink
      (\ s a ->
         s{_gipiarExternalBrowserDestinationLink = a})

-- | The entire FDL, expanded from a short link. It is the same as the
-- requested_link, if it is long. Parameters from this should not be used
-- directly (ie: server can default utm_[campaign|medium|source] to a value
-- when requested_link lack them, server determine the best fallback_link
-- when requested_link specifies >1 fallback links).
gipiarResolvedLink :: Lens' GetIosPostInstallAttributionResponse (Maybe Text)
gipiarResolvedLink
  = lens _gipiarResolvedLink
      (\ s a -> s{_gipiarResolvedLink = a})

-- | Entire FDL (short or long) attributed post-install via one of several
-- techniques (fingerprint, copy unique).
gipiarRequestedLink :: Lens' GetIosPostInstallAttributionResponse (Maybe Text)
gipiarRequestedLink
  = lens _gipiarRequestedLink
      (\ s a -> s{_gipiarRequestedLink = a})

-- | Scion medium value to be propagated by iSDK to Scion at post-install.
gipiarUtmMedium :: Lens' GetIosPostInstallAttributionResponse (Maybe Text)
gipiarUtmMedium
  = lens _gipiarUtmMedium
      (\ s a -> s{_gipiarUtmMedium = a})

-- | The link to navigate to update the app if min version is not met. This
-- is either (in order): 1) fallback link (from ?ifl= parameter, if
-- specified by developer) or 2) AppStore URL (from ?isi= parameter, if
-- specified), or 3) the payload link (from required link= parameter).
gipiarFallbackLink :: Lens' GetIosPostInstallAttributionResponse (Maybe Text)
gipiarFallbackLink
  = lens _gipiarFallbackLink
      (\ s a -> s{_gipiarFallbackLink = a})

-- | Invitation ID attributed post-install via one of several techniques
-- (fingerprint, copy unique).
gipiarInvitationId :: Lens' GetIosPostInstallAttributionResponse (Maybe Text)
gipiarInvitationId
  = lens _gipiarInvitationId
      (\ s a -> s{_gipiarInvitationId = a})

-- | Instruction for iSDK to attemmpt to perform strong match. For instance,
-- if browser does not support\/allow cookie or outside of support
-- browsers, this will be false.
gipiarIsStrongMatchExecutable :: Lens' GetIosPostInstallAttributionResponse (Maybe Bool)
gipiarIsStrongMatchExecutable
  = lens _gipiarIsStrongMatchExecutable
      (\ s a -> s{_gipiarIsStrongMatchExecutable = a})

-- | Scion campaign value to be propagated by iSDK to Scion at post-install.
gipiarUtmCampaign :: Lens' GetIosPostInstallAttributionResponse (Maybe Text)
gipiarUtmCampaign
  = lens _gipiarUtmCampaign
      (\ s a -> s{_gipiarUtmCampaign = a})

-- | Describes why match failed, ie: \"discarded due to low confidence\".
-- This message will be publicly visible.
gipiarMatchMessage :: Lens' GetIosPostInstallAttributionResponse (Maybe Text)
gipiarMatchMessage
  = lens _gipiarMatchMessage
      (\ s a -> s{_gipiarMatchMessage = a})

-- | Scion source value to be propagated by iSDK to Scion at post-install.
gipiarUtmSource :: Lens' GetIosPostInstallAttributionResponse (Maybe Text)
gipiarUtmSource
  = lens _gipiarUtmSource
      (\ s a -> s{_gipiarUtmSource = a})

instance FromJSON
         GetIosPostInstallAttributionResponse where
        parseJSON
          = withObject "GetIosPostInstallAttributionResponse"
              (\ o ->
                 GetIosPostInstallAttributionResponse' <$>
                   (o .:? "deepLink") <*> (o .:? "appMinimumVersion")
                     <*> (o .:? "attributionConfidence")
                     <*> (o .:? "externalBrowserDestinationLink")
                     <*> (o .:? "resolvedLink")
                     <*> (o .:? "requestedLink")
                     <*> (o .:? "utmMedium")
                     <*> (o .:? "fallbackLink")
                     <*> (o .:? "invitationId")
                     <*> (o .:? "isStrongMatchExecutable")
                     <*> (o .:? "utmCampaign")
                     <*> (o .:? "matchMessage")
                     <*> (o .:? "utmSource"))

instance ToJSON GetIosPostInstallAttributionResponse
         where
        toJSON GetIosPostInstallAttributionResponse'{..}
          = object
              (catMaybes
                 [("deepLink" .=) <$> _gipiarDeepLink,
                  ("appMinimumVersion" .=) <$>
                    _gipiarAppMinimumVersion,
                  ("attributionConfidence" .=) <$>
                    _gipiarAttributionConfidence,
                  ("externalBrowserDestinationLink" .=) <$>
                    _gipiarExternalBrowserDestinationLink,
                  ("resolvedLink" .=) <$> _gipiarResolvedLink,
                  ("requestedLink" .=) <$> _gipiarRequestedLink,
                  ("utmMedium" .=) <$> _gipiarUtmMedium,
                  ("fallbackLink" .=) <$> _gipiarFallbackLink,
                  ("invitationId" .=) <$> _gipiarInvitationId,
                  ("isStrongMatchExecutable" .=) <$>
                    _gipiarIsStrongMatchExecutable,
                  ("utmCampaign" .=) <$> _gipiarUtmCampaign,
                  ("matchMessage" .=) <$> _gipiarMatchMessage,
                  ("utmSource" .=) <$> _gipiarUtmSource])

-- | Parameters for Google Play Campaign Measurements. [Learn
-- more](https:\/\/developers.google.com\/analytics\/devguides\/collection\/android\/v4\/campaigns#campaign-params)
--
-- /See:/ 'googlePlayAnalytics' smart constructor.
data GooglePlayAnalytics = GooglePlayAnalytics'
    { _gpaUtmContent :: !(Maybe Text)
    , _gpaUtmMedium :: !(Maybe Text)
    , _gpaUtmTerm :: !(Maybe Text)
    , _gpaUtmCampaign :: !(Maybe Text)
    , _gpaGclid :: !(Maybe Text)
    , _gpaUtmSource :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GooglePlayAnalytics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpaUtmContent'
--
-- * 'gpaUtmMedium'
--
-- * 'gpaUtmTerm'
--
-- * 'gpaUtmCampaign'
--
-- * 'gpaGclid'
--
-- * 'gpaUtmSource'
googlePlayAnalytics
    :: GooglePlayAnalytics
googlePlayAnalytics = 
    GooglePlayAnalytics'
    { _gpaUtmContent = Nothing
    , _gpaUtmMedium = Nothing
    , _gpaUtmTerm = Nothing
    , _gpaUtmCampaign = Nothing
    , _gpaGclid = Nothing
    , _gpaUtmSource = Nothing
    }

-- | Campaign content; used for A\/B testing and content-targeted ads to
-- differentiate ads or links that point to the same URL.
gpaUtmContent :: Lens' GooglePlayAnalytics (Maybe Text)
gpaUtmContent
  = lens _gpaUtmContent
      (\ s a -> s{_gpaUtmContent = a})

-- | Campaign medium; used to identify a medium such as email or
-- cost-per-click.
gpaUtmMedium :: Lens' GooglePlayAnalytics (Maybe Text)
gpaUtmMedium
  = lens _gpaUtmMedium (\ s a -> s{_gpaUtmMedium = a})

-- | Campaign term; used with paid search to supply the keywords for ads.
gpaUtmTerm :: Lens' GooglePlayAnalytics (Maybe Text)
gpaUtmTerm
  = lens _gpaUtmTerm (\ s a -> s{_gpaUtmTerm = a})

-- | Campaign name; used for keyword analysis to identify a specific product
-- promotion or strategic campaign.
gpaUtmCampaign :: Lens' GooglePlayAnalytics (Maybe Text)
gpaUtmCampaign
  = lens _gpaUtmCampaign
      (\ s a -> s{_gpaUtmCampaign = a})

-- | [AdWords autotagging
-- parameter](https:\/\/support.google.com\/analytics\/answer\/1033981?hl=en);
-- used to measure Google AdWords ads. This value is generated dynamically
-- and should never be modified.
gpaGclid :: Lens' GooglePlayAnalytics (Maybe Text)
gpaGclid = lens _gpaGclid (\ s a -> s{_gpaGclid = a})

-- | Campaign source; used to identify a search engine, newsletter, or other
-- source.
gpaUtmSource :: Lens' GooglePlayAnalytics (Maybe Text)
gpaUtmSource
  = lens _gpaUtmSource (\ s a -> s{_gpaUtmSource = a})

instance FromJSON GooglePlayAnalytics where
        parseJSON
          = withObject "GooglePlayAnalytics"
              (\ o ->
                 GooglePlayAnalytics' <$>
                   (o .:? "utmContent") <*> (o .:? "utmMedium") <*>
                     (o .:? "utmTerm")
                     <*> (o .:? "utmCampaign")
                     <*> (o .:? "gclid")
                     <*> (o .:? "utmSource"))

instance ToJSON GooglePlayAnalytics where
        toJSON GooglePlayAnalytics'{..}
          = object
              (catMaybes
                 [("utmContent" .=) <$> _gpaUtmContent,
                  ("utmMedium" .=) <$> _gpaUtmMedium,
                  ("utmTerm" .=) <$> _gpaUtmTerm,
                  ("utmCampaign" .=) <$> _gpaUtmCampaign,
                  ("gclid" .=) <$> _gpaGclid,
                  ("utmSource" .=) <$> _gpaUtmSource])

-- | Signals associated with the device making the request.
--
-- /See:/ 'deviceInfo' smart constructor.
data DeviceInfo = DeviceInfo'
    { _diScreenResolutionWidth :: !(Maybe (Textual Int64))
    , _diLanguageCode :: !(Maybe Text)
    , _diDeviceModelName :: !(Maybe Text)
    , _diScreenResolutionHeight :: !(Maybe (Textual Int64))
    , _diLanguageCodeRaw :: !(Maybe Text)
    , _diTimezone :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeviceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diScreenResolutionWidth'
--
-- * 'diLanguageCode'
--
-- * 'diDeviceModelName'
--
-- * 'diScreenResolutionHeight'
--
-- * 'diLanguageCodeRaw'
--
-- * 'diTimezone'
deviceInfo
    :: DeviceInfo
deviceInfo = 
    DeviceInfo'
    { _diScreenResolutionWidth = Nothing
    , _diLanguageCode = Nothing
    , _diDeviceModelName = Nothing
    , _diScreenResolutionHeight = Nothing
    , _diLanguageCodeRaw = Nothing
    , _diTimezone = Nothing
    }

-- | Device display resolution width.
diScreenResolutionWidth :: Lens' DeviceInfo (Maybe Int64)
diScreenResolutionWidth
  = lens _diScreenResolutionWidth
      (\ s a -> s{_diScreenResolutionWidth = a})
      . mapping _Coerce

-- | Device language code setting.
diLanguageCode :: Lens' DeviceInfo (Maybe Text)
diLanguageCode
  = lens _diLanguageCode
      (\ s a -> s{_diLanguageCode = a})

-- | Device model name.
diDeviceModelName :: Lens' DeviceInfo (Maybe Text)
diDeviceModelName
  = lens _diDeviceModelName
      (\ s a -> s{_diDeviceModelName = a})

-- | Device display resolution height.
diScreenResolutionHeight :: Lens' DeviceInfo (Maybe Int64)
diScreenResolutionHeight
  = lens _diScreenResolutionHeight
      (\ s a -> s{_diScreenResolutionHeight = a})
      . mapping _Coerce

-- | Device language code raw setting. iOS does returns language code in
-- different format than iOS WebView. For example WebView returns en_US,
-- but iOS returns en-US. Field below will return raw value returned by
-- iOS.
diLanguageCodeRaw :: Lens' DeviceInfo (Maybe Text)
diLanguageCodeRaw
  = lens _diLanguageCodeRaw
      (\ s a -> s{_diLanguageCodeRaw = a})

-- | Device timezone setting.
diTimezone :: Lens' DeviceInfo (Maybe Text)
diTimezone
  = lens _diTimezone (\ s a -> s{_diTimezone = a})

instance FromJSON DeviceInfo where
        parseJSON
          = withObject "DeviceInfo"
              (\ o ->
                 DeviceInfo' <$>
                   (o .:? "screenResolutionWidth") <*>
                     (o .:? "languageCode")
                     <*> (o .:? "deviceModelName")
                     <*> (o .:? "screenResolutionHeight")
                     <*> (o .:? "languageCodeRaw")
                     <*> (o .:? "timezone"))

instance ToJSON DeviceInfo where
        toJSON DeviceInfo'{..}
          = object
              (catMaybes
                 [("screenResolutionWidth" .=) <$>
                    _diScreenResolutionWidth,
                  ("languageCode" .=) <$> _diLanguageCode,
                  ("deviceModelName" .=) <$> _diDeviceModelName,
                  ("screenResolutionHeight" .=) <$>
                    _diScreenResolutionHeight,
                  ("languageCodeRaw" .=) <$> _diLanguageCodeRaw,
                  ("timezone" .=) <$> _diTimezone])
