{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Network.Google.Vision
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Integrates Google Vision features, including image labeling, face, logo,
-- and landmark detection, optical character recognition (OCR), and
-- detection of explicit content, into applications.
--
-- /See:/ <https://cloud.google.com/vision/ Google Cloud Vision API Reference>
module Network.Google.Vision
    (
    -- * Service Configuration
      visionService

    -- * OAuth Scopes
    , cloudVisionScope
    , cloudPlatformScope

    -- * API Declaration
    , VisionAPI

    -- * Resources

    -- ** vision.images.annotate
    , module Network.Google.Resource.Vision.Images.Annotate

    -- * Types

    -- ** LatLng
    , LatLng
    , latLng
    , llLatitude
    , llLongitude

    -- ** FaceAnnotationUnderExposedLikelihood
    , FaceAnnotationUnderExposedLikelihood (..)

    -- ** Feature
    , Feature
    , feature
    , fType
    , fMaxResults

    -- ** Status
    , Status
    , status
    , sDetails
    , sCode
    , sMessage

    -- ** Property
    , Property
    , property
    , pUint64Value
    , pValue
    , pName

    -- ** Image
    , Image
    , image
    , iContent
    , iSource

    -- ** Landmark
    , Landmark
    , landmark
    , lType
    , lPosition

    -- ** CropHintsParams
    , CropHintsParams
    , cropHintsParams
    , chpAspectRatios

    -- ** TextProperty
    , TextProperty
    , textProperty
    , tpDetectedLanguages
    , tpDetectedBreak

    -- ** TextAnnotation
    , TextAnnotation
    , textAnnotation
    , taText
    , taPages

    -- ** Color
    , Color
    , color
    , cRed
    , cAlpha
    , cGreen
    , cBlue

    -- ** FaceAnnotationHeadwearLikelihood
    , FaceAnnotationHeadwearLikelihood (..)

    -- ** BlockBlockType
    , BlockBlockType (..)

    -- ** BoundingPoly
    , BoundingPoly
    , boundingPoly
    , bpVertices

    -- ** SafeSearchAnnotationAdult
    , SafeSearchAnnotationAdult (..)

    -- ** Vertex
    , Vertex
    , vertex
    , vX
    , vY

    -- ** WebEntity
    , WebEntity
    , webEntity
    , weScore
    , weEntityId
    , weDescription

    -- ** FaceAnnotationAngerLikelihood
    , FaceAnnotationAngerLikelihood (..)

    -- ** LocationInfo
    , LocationInfo
    , locationInfo
    , liLatLng

    -- ** SafeSearchAnnotationMedical
    , SafeSearchAnnotationMedical (..)

    -- ** StatusDetailsItem
    , StatusDetailsItem
    , statusDetailsItem
    , sdiAddtional

    -- ** BatchAnnotateImagesRequest
    , BatchAnnotateImagesRequest
    , batchAnnotateImagesRequest
    , bairRequests

    -- ** Page
    , Page
    , page
    , pProperty
    , pHeight
    , pBlocks
    , pWidth

    -- ** ColorInfo
    , ColorInfo
    , colorInfo
    , ciColor
    , ciScore
    , ciPixelFraction

    -- ** Paragraph
    , Paragraph
    , paragraph
    , parProperty
    , parBoundingBox
    , parWords

    -- ** Symbol
    , Symbol
    , symbol
    , sProperty
    , sBoundingBox
    , sText

    -- ** FaceAnnotationBlurredLikelihood
    , FaceAnnotationBlurredLikelihood (..)

    -- ** AnnotateImageResponse
    , AnnotateImageResponse
    , annotateImageResponse
    , airLogoAnnotations
    , airLabelAnnotations
    , airFaceAnnotations
    , airError
    , airWebDetection
    , airSafeSearchAnnotation
    , airLandmarkAnnotations
    , airTextAnnotations
    , airCropHintsAnnotation
    , airFullTextAnnotation
    , airImagePropertiesAnnotation

    -- ** ImageProperties
    , ImageProperties
    , imageProperties
    , ipDominantColors

    -- ** FaceAnnotation
    , FaceAnnotation
    , faceAnnotation
    , faTiltAngle
    , faBlurredLikelihood
    , faBoundingPoly
    , faSurpriseLikelihood
    , faLandmarkingConfidence
    , faPanAngle
    , faRollAngle
    , faUnderExposedLikelihood
    , faFdBoundingPoly
    , faAngerLikelihood
    , faDetectionConfidence
    , faHeadwearLikelihood
    , faSorrowLikelihood
    , faJoyLikelihood
    , faLandmarks

    -- ** DetectedBreak
    , DetectedBreak
    , detectedBreak
    , dbIsPrefix
    , dbType

    -- ** Block
    , Block
    , block
    , bProperty
    , bBoundingBox
    , bParagraphs
    , bBlockType

    -- ** SafeSearchAnnotationViolence
    , SafeSearchAnnotationViolence (..)

    -- ** EntityAnnotation
    , EntityAnnotation
    , entityAnnotation
    , eaScore
    , eaTopicality
    , eaLocale
    , eaBoundingPoly
    , eaConfidence
    , eaMid
    , eaLocations
    , eaDescription
    , eaProperties

    -- ** FeatureType
    , FeatureType (..)

    -- ** AnnotateImageRequest
    , AnnotateImageRequest
    , annotateImageRequest
    , airImage
    , airFeatures
    , airImageContext

    -- ** DetectedLanguage
    , DetectedLanguage
    , detectedLanguage
    , dlLanguageCode
    , dlConfidence

    -- ** WebImage
    , WebImage
    , webImage
    , wiScore
    , wiURL

    -- ** WebDetection
    , WebDetection
    , webDetection
    , wdVisuallySimilarImages
    , wdPagesWithMatchingImages
    , wdPartialMatchingImages
    , wdFullMatchingImages
    , wdWebEntities

    -- ** LandmarkType
    , LandmarkType (..)

    -- ** Xgafv
    , Xgafv (..)

    -- ** ImageSource
    , ImageSource
    , imageSource
    , isGcsImageURI
    , isImageURI

    -- ** CropHint
    , CropHint
    , cropHint
    , chBoundingPoly
    , chConfidence
    , chImportanceFraction

    -- ** SafeSearchAnnotationSpoof
    , SafeSearchAnnotationSpoof (..)

    -- ** FaceAnnotationSurpriseLikelihood
    , FaceAnnotationSurpriseLikelihood (..)

    -- ** SafeSearchAnnotation
    , SafeSearchAnnotation
    , safeSearchAnnotation
    , ssaSpoof
    , ssaAdult
    , ssaMedical
    , ssaViolence

    -- ** FaceAnnotationSorrowLikelihood
    , FaceAnnotationSorrowLikelihood (..)

    -- ** FaceAnnotationJoyLikelihood
    , FaceAnnotationJoyLikelihood (..)

    -- ** ImageContext
    , ImageContext
    , imageContext
    , icCropHintsParams
    , icLanguageHints
    , icLatLongRect

    -- ** WebPage
    , WebPage
    , webPage
    , wpScore
    , wpURL

    -- ** DominantColorsAnnotation
    , DominantColorsAnnotation
    , dominantColorsAnnotation
    , dcaColors

    -- ** LatLongRect
    , LatLongRect
    , latLongRect
    , llrMaxLatLng
    , llrMinLatLng

    -- ** Word
    , Word
    , word
    , wProperty
    , wBoundingBox
    , wSymbols

    -- ** DetectedBreakType
    , DetectedBreakType (..)

    -- ** BatchAnnotateImagesResponse
    , BatchAnnotateImagesResponse
    , batchAnnotateImagesResponse
    , bairResponses

    -- ** CropHintsAnnotation
    , CropHintsAnnotation
    , cropHintsAnnotation
    , chaCropHints

    -- ** Position
    , Position
    , position
    , pZ
    , pX
    , pY
    ) where

import Network.Google.Prelude
import Network.Google.Resource.Vision.Images.Annotate
import Network.Google.Vision.Types

{- $resources
TODO
-}

-- | Represents the entirety of the methods and resources available for the Google Cloud Vision API service.
type VisionAPI = ImagesAnnotateResource
