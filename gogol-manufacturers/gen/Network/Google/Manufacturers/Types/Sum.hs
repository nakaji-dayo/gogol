{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.Google.Manufacturers.Types.Sum
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.Google.Manufacturers.Types.Sum where

import Network.Google.Prelude

-- | The status of the image. \'OutputOnly
data ImageStatus
    = StatusUnspecified
      -- ^ @STATUS_UNSPECIFIED@
      -- The image status is unspecified. Should not be used.
    | PendingProcessing
      -- ^ @PENDING_PROCESSING@
      -- The image was uploaded and is being processed.
    | PendingCrawl
      -- ^ @PENDING_CRAWL@
      -- The image crawl is still pending.
    | OK
      -- ^ @OK@
      -- The image was processed and it meets the requirements.
    | Roboted
      -- ^ @ROBOTED@
      -- The image URL is protected by robots.txt file and cannot be crawled.
    | Xroboted
      -- ^ @XROBOTED@
      -- The image URL is protected by X-Robots-Tag and cannot be crawled.
    | CrawlError
      -- ^ @CRAWL_ERROR@
      -- There was an error while crawling the image.
    | ProcessingError
      -- ^ @PROCESSING_ERROR@
      -- The image cannot be processed.
    | DecodingError
      -- ^ @DECODING_ERROR@
      -- The image cannot be decoded.
    | TooBig
      -- ^ @TOO_BIG@
      -- The image is too big.
    | CrawlSkipped
      -- ^ @CRAWL_SKIPPED@
      -- The image was manually overridden and will not be crawled.
    | HostLoaded
      -- ^ @HOSTLOADED@
      -- The image crawl was postponed to avoid overloading the host.
    | HTTP404
      -- ^ @HTTP_404@
      -- The image URL returned a \"404 Not Found\" error.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable ImageStatus

instance FromHttpApiData ImageStatus where
    parseQueryParam = \case
        "STATUS_UNSPECIFIED" -> Right StatusUnspecified
        "PENDING_PROCESSING" -> Right PendingProcessing
        "PENDING_CRAWL" -> Right PendingCrawl
        "OK" -> Right OK
        "ROBOTED" -> Right Roboted
        "XROBOTED" -> Right Xroboted
        "CRAWL_ERROR" -> Right CrawlError
        "PROCESSING_ERROR" -> Right ProcessingError
        "DECODING_ERROR" -> Right DecodingError
        "TOO_BIG" -> Right TooBig
        "CRAWL_SKIPPED" -> Right CrawlSkipped
        "HOSTLOADED" -> Right HostLoaded
        "HTTP_404" -> Right HTTP404
        x -> Left ("Unable to parse ImageStatus from: " <> x)

instance ToHttpApiData ImageStatus where
    toQueryParam = \case
        StatusUnspecified -> "STATUS_UNSPECIFIED"
        PendingProcessing -> "PENDING_PROCESSING"
        PendingCrawl -> "PENDING_CRAWL"
        OK -> "OK"
        Roboted -> "ROBOTED"
        Xroboted -> "XROBOTED"
        CrawlError -> "CRAWL_ERROR"
        ProcessingError -> "PROCESSING_ERROR"
        DecodingError -> "DECODING_ERROR"
        TooBig -> "TOO_BIG"
        CrawlSkipped -> "CRAWL_SKIPPED"
        HostLoaded -> "HOSTLOADED"
        HTTP404 -> "HTTP_404"

instance FromJSON ImageStatus where
    parseJSON = parseJSONText "ImageStatus"

instance ToJSON ImageStatus where
    toJSON = toJSONText

-- | The type of the image, i.e., crawled or uploaded. \'OutputOnly
data ImageType
    = TypeUnspecified
      -- ^ @TYPE_UNSPECIFIED@
      -- Type is unspecified. Should not be used.
    | Crawled
      -- ^ @CRAWLED@
      -- The image was crawled from a provided URL.
    | Uploaded
      -- ^ @UPLOADED@
      -- The image was uploaded.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable ImageType

instance FromHttpApiData ImageType where
    parseQueryParam = \case
        "TYPE_UNSPECIFIED" -> Right TypeUnspecified
        "CRAWLED" -> Right Crawled
        "UPLOADED" -> Right Uploaded
        x -> Left ("Unable to parse ImageType from: " <> x)

instance ToHttpApiData ImageType where
    toQueryParam = \case
        TypeUnspecified -> "TYPE_UNSPECIFIED"
        Crawled -> "CRAWLED"
        Uploaded -> "UPLOADED"

instance FromJSON ImageType where
    parseJSON = parseJSONText "ImageType"

instance ToJSON ImageType where
    toJSON = toJSONText

-- | V1 error format.
data Xgafv
    = X1
      -- ^ @1@
      -- v1 error format
    | X2
      -- ^ @2@
      -- v2 error format
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable Xgafv

instance FromHttpApiData Xgafv where
    parseQueryParam = \case
        "1" -> Right X1
        "2" -> Right X2
        x -> Left ("Unable to parse Xgafv from: " <> x)

instance ToHttpApiData Xgafv where
    toQueryParam = \case
        X1 -> "1"
        X2 -> "2"

instance FromJSON Xgafv where
    parseJSON = parseJSONText "Xgafv"

instance ToJSON Xgafv where
    toJSON = toJSONText

-- | The severity of the issue.
data IssueSeverity
    = SeverityUnspecified
      -- ^ @SEVERITY_UNSPECIFIED@
      -- Unspecified severity, never used.
    | Error'
      -- ^ @ERROR@
      -- Error severity. The issue prevents the usage of the whole item.
    | Warning
      -- ^ @WARNING@
      -- Warning severity. The issue is either one that prevents the usage of the
      -- attribute that triggered it or one that will soon prevent the usage of
      -- the whole item.
    | Info
      -- ^ @INFO@
      -- Info severity. The issue is one that doesn\'t require immediate
      -- attention. It is, for example, used to communicate which attributes are
      -- still pending review.
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, Generic)

instance Hashable IssueSeverity

instance FromHttpApiData IssueSeverity where
    parseQueryParam = \case
        "SEVERITY_UNSPECIFIED" -> Right SeverityUnspecified
        "ERROR" -> Right Error'
        "WARNING" -> Right Warning
        "INFO" -> Right Info
        x -> Left ("Unable to parse IssueSeverity from: " <> x)

instance ToHttpApiData IssueSeverity where
    toQueryParam = \case
        SeverityUnspecified -> "SEVERITY_UNSPECIFIED"
        Error' -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"

instance FromJSON IssueSeverity where
    parseJSON = parseJSONText "IssueSeverity"

instance ToJSON IssueSeverity where
    toJSON = toJSONText
