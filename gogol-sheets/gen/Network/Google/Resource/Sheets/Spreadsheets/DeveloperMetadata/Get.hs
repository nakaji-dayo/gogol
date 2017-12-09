{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- |
-- Module      : Network.Google.Resource.Sheets.Spreadsheets.DeveloperMetadata.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the developer metadata with the specified ID. The caller must
-- specify the spreadsheet ID and the developer metadata\'s unique
-- metadataId.
--
-- /See:/ <https://developers.google.com/sheets/ Google Sheets API Reference> for @sheets.spreadsheets.developerMetadata.get@.
module Network.Google.Resource.Sheets.Spreadsheets.DeveloperMetadata.Get
    (
    -- * REST Resource
      SpreadsheetsDeveloperMetadataGetResource

    -- * Creating a Request
    , spreadsheetsDeveloperMetadataGet
    , SpreadsheetsDeveloperMetadataGet

    -- * Request Lenses
    , sdmgXgafv
    , sdmgUploadProtocol
    , sdmgMetadataId
    , sdmgPp
    , sdmgAccessToken
    , sdmgSpreadsheetId
    , sdmgUploadType
    , sdmgBearerToken
    , sdmgFields
    , sdmgCallback
    ) where

import Network.Google.Prelude
import Network.Google.Sheets.Types

-- | A resource alias for @sheets.spreadsheets.developerMetadata.get@ method which the
-- 'SpreadsheetsDeveloperMetadataGet' request conforms to.
type SpreadsheetsDeveloperMetadataGetResource =
     "v4" :>
       "spreadsheets" :>
         Capture "spreadsheetId" Text :>
           "developerMetadata" :>
             Capture "metadataId" (Textual Int32) :>
               QueryParam "$.xgafv" Xgafv :>
                 QueryParam "upload_protocol" Text :>
                   QueryParam "pp" Bool :>
                     QueryParam "access_token" Text :>
                       QueryParam "uploadType" Text :>
                         QueryParam "bearer_token" Text :>
                           QueryParam "callback" Text :>
                             QueryParam "fields" Text :>
                               QueryParam "alt" AltJSON :>
                                 Get '[JSON] DeveloperMetadata

-- | Returns the developer metadata with the specified ID. The caller must
-- specify the spreadsheet ID and the developer metadata\'s unique
-- metadataId.
--
-- /See:/ 'spreadsheetsDeveloperMetadataGet' smart constructor.
data SpreadsheetsDeveloperMetadataGet = SpreadsheetsDeveloperMetadataGet'
    { _sdmgXgafv :: !(Maybe Xgafv)
    , _sdmgUploadProtocol :: !(Maybe Text)
    , _sdmgMetadataId :: !(Textual Int32)
    , _sdmgPp :: !Bool
    , _sdmgAccessToken :: !(Maybe Text)
    , _sdmgSpreadsheetId :: !Text
    , _sdmgUploadType :: !(Maybe Text)
    , _sdmgBearerToken :: !(Maybe Text)
    , _sdmgFields :: !(Maybe Text)
    , _sdmgCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SpreadsheetsDeveloperMetadataGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdmgXgafv'
--
-- * 'sdmgUploadProtocol'
--
-- * 'sdmgMetadataId'
--
-- * 'sdmgPp'
--
-- * 'sdmgAccessToken'
--
-- * 'sdmgSpreadsheetId'
--
-- * 'sdmgUploadType'
--
-- * 'sdmgBearerToken'
--
-- * 'sdmgFields'
--
-- * 'sdmgCallback'
spreadsheetsDeveloperMetadataGet
    :: Int32 -- ^ 'sdmgMetadataId'
    -> Text -- ^ 'sdmgSpreadsheetId'
    -> SpreadsheetsDeveloperMetadataGet
spreadsheetsDeveloperMetadataGet pSdmgMetadataId_ pSdmgSpreadsheetId_ = 
    SpreadsheetsDeveloperMetadataGet'
    { _sdmgXgafv = Nothing
    , _sdmgUploadProtocol = Nothing
    , _sdmgMetadataId = _Coerce # pSdmgMetadataId_
    , _sdmgPp = True
    , _sdmgAccessToken = Nothing
    , _sdmgSpreadsheetId = pSdmgSpreadsheetId_
    , _sdmgUploadType = Nothing
    , _sdmgBearerToken = Nothing
    , _sdmgFields = Nothing
    , _sdmgCallback = Nothing
    }

-- | V1 error format.
sdmgXgafv :: Lens' SpreadsheetsDeveloperMetadataGet (Maybe Xgafv)
sdmgXgafv
  = lens _sdmgXgafv (\ s a -> s{_sdmgXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
sdmgUploadProtocol :: Lens' SpreadsheetsDeveloperMetadataGet (Maybe Text)
sdmgUploadProtocol
  = lens _sdmgUploadProtocol
      (\ s a -> s{_sdmgUploadProtocol = a})

-- | The ID of the developer metadata to retrieve.
sdmgMetadataId :: Lens' SpreadsheetsDeveloperMetadataGet Int32
sdmgMetadataId
  = lens _sdmgMetadataId
      (\ s a -> s{_sdmgMetadataId = a})
      . _Coerce

-- | Pretty-print response.
sdmgPp :: Lens' SpreadsheetsDeveloperMetadataGet Bool
sdmgPp = lens _sdmgPp (\ s a -> s{_sdmgPp = a})

-- | OAuth access token.
sdmgAccessToken :: Lens' SpreadsheetsDeveloperMetadataGet (Maybe Text)
sdmgAccessToken
  = lens _sdmgAccessToken
      (\ s a -> s{_sdmgAccessToken = a})

-- | The ID of the spreadsheet to retrieve metadata from.
sdmgSpreadsheetId :: Lens' SpreadsheetsDeveloperMetadataGet Text
sdmgSpreadsheetId
  = lens _sdmgSpreadsheetId
      (\ s a -> s{_sdmgSpreadsheetId = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
sdmgUploadType :: Lens' SpreadsheetsDeveloperMetadataGet (Maybe Text)
sdmgUploadType
  = lens _sdmgUploadType
      (\ s a -> s{_sdmgUploadType = a})

-- | OAuth bearer token.
sdmgBearerToken :: Lens' SpreadsheetsDeveloperMetadataGet (Maybe Text)
sdmgBearerToken
  = lens _sdmgBearerToken
      (\ s a -> s{_sdmgBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
sdmgFields :: Lens' SpreadsheetsDeveloperMetadataGet (Maybe Text)
sdmgFields
  = lens _sdmgFields (\ s a -> s{_sdmgFields = a})

-- | JSONP
sdmgCallback :: Lens' SpreadsheetsDeveloperMetadataGet (Maybe Text)
sdmgCallback
  = lens _sdmgCallback (\ s a -> s{_sdmgCallback = a})

instance GoogleRequest
         SpreadsheetsDeveloperMetadataGet where
        type Rs SpreadsheetsDeveloperMetadataGet =
             DeveloperMetadata
        type Scopes SpreadsheetsDeveloperMetadataGet =
             '["https://www.googleapis.com/auth/drive",
               "https://www.googleapis.com/auth/drive.file",
               "https://www.googleapis.com/auth/spreadsheets"]
        requestClient SpreadsheetsDeveloperMetadataGet'{..}
          = go _sdmgSpreadsheetId _sdmgMetadataId _sdmgXgafv
              _sdmgUploadProtocol
              (Just _sdmgPp)
              _sdmgAccessToken
              _sdmgUploadType
              _sdmgBearerToken
              _sdmgCallback
              _sdmgFields
              (Just AltJSON)
              sheetsService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy SpreadsheetsDeveloperMetadataGetResource)
                      mempty
