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
-- Module      : Network.Google.Resource.Sheets.Spreadsheets.Values.BatchUpdateByDataFilter
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets values in one or more ranges of a spreadsheet. The caller must
-- specify the spreadsheet ID, a valueInputOption, and one or more
-- DataFilterValueRanges.
--
-- /See:/ <https://developers.google.com/sheets/ Google Sheets API Reference> for @sheets.spreadsheets.values.batchUpdateByDataFilter@.
module Network.Google.Resource.Sheets.Spreadsheets.Values.BatchUpdateByDataFilter
    (
    -- * REST Resource
      SpreadsheetsValuesBatchUpdateByDataFilterResource

    -- * Creating a Request
    , spreadsheetsValuesBatchUpdateByDataFilter
    , SpreadsheetsValuesBatchUpdateByDataFilter

    -- * Request Lenses
    , svbubdfXgafv
    , svbubdfUploadProtocol
    , svbubdfPp
    , svbubdfAccessToken
    , svbubdfSpreadsheetId
    , svbubdfUploadType
    , svbubdfPayload
    , svbubdfBearerToken
    , svbubdfFields
    , svbubdfCallback
    ) where

import Network.Google.Prelude
import Network.Google.Sheets.Types

-- | A resource alias for @sheets.spreadsheets.values.batchUpdateByDataFilter@ method which the
-- 'SpreadsheetsValuesBatchUpdateByDataFilter' request conforms to.
type SpreadsheetsValuesBatchUpdateByDataFilterResource
     =
     "v4" :>
       "spreadsheets" :>
         Capture "spreadsheetId" Text :>
           "values:batchUpdateByDataFilter" :>
             QueryParam "$.xgafv" Xgafv :>
               QueryParam "upload_protocol" Text :>
                 QueryParam "pp" Bool :>
                   QueryParam "access_token" Text :>
                     QueryParam "uploadType" Text :>
                       QueryParam "bearer_token" Text :>
                         QueryParam "callback" Text :>
                           QueryParam "fields" Text :>
                             QueryParam "alt" AltJSON :>
                               ReqBody '[JSON]
                                 BatchUpdateValuesByDataFilterRequest
                                 :>
                                 Post '[JSON]
                                   BatchUpdateValuesByDataFilterResponse

-- | Sets values in one or more ranges of a spreadsheet. The caller must
-- specify the spreadsheet ID, a valueInputOption, and one or more
-- DataFilterValueRanges.
--
-- /See:/ 'spreadsheetsValuesBatchUpdateByDataFilter' smart constructor.
data SpreadsheetsValuesBatchUpdateByDataFilter = SpreadsheetsValuesBatchUpdateByDataFilter'
    { _svbubdfXgafv :: !(Maybe Xgafv)
    , _svbubdfUploadProtocol :: !(Maybe Text)
    , _svbubdfPp :: !Bool
    , _svbubdfAccessToken :: !(Maybe Text)
    , _svbubdfSpreadsheetId :: !Text
    , _svbubdfUploadType :: !(Maybe Text)
    , _svbubdfPayload :: !BatchUpdateValuesByDataFilterRequest
    , _svbubdfBearerToken :: !(Maybe Text)
    , _svbubdfFields :: !(Maybe Text)
    , _svbubdfCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'SpreadsheetsValuesBatchUpdateByDataFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svbubdfXgafv'
--
-- * 'svbubdfUploadProtocol'
--
-- * 'svbubdfPp'
--
-- * 'svbubdfAccessToken'
--
-- * 'svbubdfSpreadsheetId'
--
-- * 'svbubdfUploadType'
--
-- * 'svbubdfPayload'
--
-- * 'svbubdfBearerToken'
--
-- * 'svbubdfFields'
--
-- * 'svbubdfCallback'
spreadsheetsValuesBatchUpdateByDataFilter
    :: Text -- ^ 'svbubdfSpreadsheetId'
    -> BatchUpdateValuesByDataFilterRequest -- ^ 'svbubdfPayload'
    -> SpreadsheetsValuesBatchUpdateByDataFilter
spreadsheetsValuesBatchUpdateByDataFilter pSvbubdfSpreadsheetId_ pSvbubdfPayload_ = 
    SpreadsheetsValuesBatchUpdateByDataFilter'
    { _svbubdfXgafv = Nothing
    , _svbubdfUploadProtocol = Nothing
    , _svbubdfPp = True
    , _svbubdfAccessToken = Nothing
    , _svbubdfSpreadsheetId = pSvbubdfSpreadsheetId_
    , _svbubdfUploadType = Nothing
    , _svbubdfPayload = pSvbubdfPayload_
    , _svbubdfBearerToken = Nothing
    , _svbubdfFields = Nothing
    , _svbubdfCallback = Nothing
    }

-- | V1 error format.
svbubdfXgafv :: Lens' SpreadsheetsValuesBatchUpdateByDataFilter (Maybe Xgafv)
svbubdfXgafv
  = lens _svbubdfXgafv (\ s a -> s{_svbubdfXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
svbubdfUploadProtocol :: Lens' SpreadsheetsValuesBatchUpdateByDataFilter (Maybe Text)
svbubdfUploadProtocol
  = lens _svbubdfUploadProtocol
      (\ s a -> s{_svbubdfUploadProtocol = a})

-- | Pretty-print response.
svbubdfPp :: Lens' SpreadsheetsValuesBatchUpdateByDataFilter Bool
svbubdfPp
  = lens _svbubdfPp (\ s a -> s{_svbubdfPp = a})

-- | OAuth access token.
svbubdfAccessToken :: Lens' SpreadsheetsValuesBatchUpdateByDataFilter (Maybe Text)
svbubdfAccessToken
  = lens _svbubdfAccessToken
      (\ s a -> s{_svbubdfAccessToken = a})

-- | The ID of the spreadsheet to update.
svbubdfSpreadsheetId :: Lens' SpreadsheetsValuesBatchUpdateByDataFilter Text
svbubdfSpreadsheetId
  = lens _svbubdfSpreadsheetId
      (\ s a -> s{_svbubdfSpreadsheetId = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
svbubdfUploadType :: Lens' SpreadsheetsValuesBatchUpdateByDataFilter (Maybe Text)
svbubdfUploadType
  = lens _svbubdfUploadType
      (\ s a -> s{_svbubdfUploadType = a})

-- | Multipart request metadata.
svbubdfPayload :: Lens' SpreadsheetsValuesBatchUpdateByDataFilter BatchUpdateValuesByDataFilterRequest
svbubdfPayload
  = lens _svbubdfPayload
      (\ s a -> s{_svbubdfPayload = a})

-- | OAuth bearer token.
svbubdfBearerToken :: Lens' SpreadsheetsValuesBatchUpdateByDataFilter (Maybe Text)
svbubdfBearerToken
  = lens _svbubdfBearerToken
      (\ s a -> s{_svbubdfBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
svbubdfFields :: Lens' SpreadsheetsValuesBatchUpdateByDataFilter (Maybe Text)
svbubdfFields
  = lens _svbubdfFields
      (\ s a -> s{_svbubdfFields = a})

-- | JSONP
svbubdfCallback :: Lens' SpreadsheetsValuesBatchUpdateByDataFilter (Maybe Text)
svbubdfCallback
  = lens _svbubdfCallback
      (\ s a -> s{_svbubdfCallback = a})

instance GoogleRequest
         SpreadsheetsValuesBatchUpdateByDataFilter where
        type Rs SpreadsheetsValuesBatchUpdateByDataFilter =
             BatchUpdateValuesByDataFilterResponse
        type Scopes SpreadsheetsValuesBatchUpdateByDataFilter
             =
             '["https://www.googleapis.com/auth/drive",
               "https://www.googleapis.com/auth/drive.file",
               "https://www.googleapis.com/auth/spreadsheets"]
        requestClient
          SpreadsheetsValuesBatchUpdateByDataFilter'{..}
          = go _svbubdfSpreadsheetId _svbubdfXgafv
              _svbubdfUploadProtocol
              (Just _svbubdfPp)
              _svbubdfAccessToken
              _svbubdfUploadType
              _svbubdfBearerToken
              _svbubdfCallback
              _svbubdfFields
              (Just AltJSON)
              _svbubdfPayload
              sheetsService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           SpreadsheetsValuesBatchUpdateByDataFilterResource)
                      mempty
