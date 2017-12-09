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
-- Module      : Network.Google.Resource.Language.Documents.AnalyzeEntitySentiment
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds entities, similar to AnalyzeEntities in the text and analyzes
-- sentiment associated with each entity and its mentions.
--
-- /See:/ <https://cloud.google.com/natural-language/ Google Cloud Natural Language API Reference> for @language.documents.analyzeEntitySentiment@.
module Network.Google.Resource.Language.Documents.AnalyzeEntitySentiment
    (
    -- * REST Resource
      DocumentsAnalyzeEntitySentimentResource

    -- * Creating a Request
    , documentsAnalyzeEntitySentiment
    , DocumentsAnalyzeEntitySentiment

    -- * Request Lenses
    , daesXgafv
    , daesUploadProtocol
    , daesPp
    , daesAccessToken
    , daesUploadType
    , daesPayload
    , daesBearerToken
    , daesFields
    , daesCallback
    ) where

import Network.Google.Language.Types
import Network.Google.Prelude

-- | A resource alias for @language.documents.analyzeEntitySentiment@ method which the
-- 'DocumentsAnalyzeEntitySentiment' request conforms to.
type DocumentsAnalyzeEntitySentimentResource =
     "v1" :>
       "documents:analyzeEntitySentiment" :>
         QueryParam "$.xgafv" Xgafv :>
           QueryParam "upload_protocol" Text :>
             QueryParam "pp" Bool :>
               QueryParam "access_token" Text :>
                 QueryParam "uploadType" Text :>
                   QueryParam "bearer_token" Text :>
                     QueryParam "callback" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] AnalyzeEntitySentimentRequest :>
                             Post '[JSON] AnalyzeEntitySentimentResponse

-- | Finds entities, similar to AnalyzeEntities in the text and analyzes
-- sentiment associated with each entity and its mentions.
--
-- /See:/ 'documentsAnalyzeEntitySentiment' smart constructor.
data DocumentsAnalyzeEntitySentiment = DocumentsAnalyzeEntitySentiment'
    { _daesXgafv :: !(Maybe Xgafv)
    , _daesUploadProtocol :: !(Maybe Text)
    , _daesPp :: !Bool
    , _daesAccessToken :: !(Maybe Text)
    , _daesUploadType :: !(Maybe Text)
    , _daesPayload :: !AnalyzeEntitySentimentRequest
    , _daesBearerToken :: !(Maybe Text)
    , _daesFields :: !(Maybe Text)
    , _daesCallback :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentsAnalyzeEntitySentiment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daesXgafv'
--
-- * 'daesUploadProtocol'
--
-- * 'daesPp'
--
-- * 'daesAccessToken'
--
-- * 'daesUploadType'
--
-- * 'daesPayload'
--
-- * 'daesBearerToken'
--
-- * 'daesFields'
--
-- * 'daesCallback'
documentsAnalyzeEntitySentiment
    :: AnalyzeEntitySentimentRequest -- ^ 'daesPayload'
    -> DocumentsAnalyzeEntitySentiment
documentsAnalyzeEntitySentiment pDaesPayload_ = 
    DocumentsAnalyzeEntitySentiment'
    { _daesXgafv = Nothing
    , _daesUploadProtocol = Nothing
    , _daesPp = True
    , _daesAccessToken = Nothing
    , _daesUploadType = Nothing
    , _daesPayload = pDaesPayload_
    , _daesBearerToken = Nothing
    , _daesFields = Nothing
    , _daesCallback = Nothing
    }

-- | V1 error format.
daesXgafv :: Lens' DocumentsAnalyzeEntitySentiment (Maybe Xgafv)
daesXgafv
  = lens _daesXgafv (\ s a -> s{_daesXgafv = a})

-- | Upload protocol for media (e.g. \"raw\", \"multipart\").
daesUploadProtocol :: Lens' DocumentsAnalyzeEntitySentiment (Maybe Text)
daesUploadProtocol
  = lens _daesUploadProtocol
      (\ s a -> s{_daesUploadProtocol = a})

-- | Pretty-print response.
daesPp :: Lens' DocumentsAnalyzeEntitySentiment Bool
daesPp = lens _daesPp (\ s a -> s{_daesPp = a})

-- | OAuth access token.
daesAccessToken :: Lens' DocumentsAnalyzeEntitySentiment (Maybe Text)
daesAccessToken
  = lens _daesAccessToken
      (\ s a -> s{_daesAccessToken = a})

-- | Legacy upload protocol for media (e.g. \"media\", \"multipart\").
daesUploadType :: Lens' DocumentsAnalyzeEntitySentiment (Maybe Text)
daesUploadType
  = lens _daesUploadType
      (\ s a -> s{_daesUploadType = a})

-- | Multipart request metadata.
daesPayload :: Lens' DocumentsAnalyzeEntitySentiment AnalyzeEntitySentimentRequest
daesPayload
  = lens _daesPayload (\ s a -> s{_daesPayload = a})

-- | OAuth bearer token.
daesBearerToken :: Lens' DocumentsAnalyzeEntitySentiment (Maybe Text)
daesBearerToken
  = lens _daesBearerToken
      (\ s a -> s{_daesBearerToken = a})

-- | Selector specifying which fields to include in a partial response.
daesFields :: Lens' DocumentsAnalyzeEntitySentiment (Maybe Text)
daesFields
  = lens _daesFields (\ s a -> s{_daesFields = a})

-- | JSONP
daesCallback :: Lens' DocumentsAnalyzeEntitySentiment (Maybe Text)
daesCallback
  = lens _daesCallback (\ s a -> s{_daesCallback = a})

instance GoogleRequest
         DocumentsAnalyzeEntitySentiment where
        type Rs DocumentsAnalyzeEntitySentiment =
             AnalyzeEntitySentimentResponse
        type Scopes DocumentsAnalyzeEntitySentiment =
             '["https://www.googleapis.com/auth/cloud-language",
               "https://www.googleapis.com/auth/cloud-platform"]
        requestClient DocumentsAnalyzeEntitySentiment'{..}
          = go _daesXgafv _daesUploadProtocol (Just _daesPp)
              _daesAccessToken
              _daesUploadType
              _daesBearerToken
              _daesCallback
              _daesFields
              (Just AltJSON)
              _daesPayload
              languageService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy DocumentsAnalyzeEntitySentimentResource)
                      mempty
