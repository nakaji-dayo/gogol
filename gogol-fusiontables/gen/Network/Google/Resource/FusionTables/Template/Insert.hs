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
-- Module      : Network.Google.Resource.FusionTables.Template.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new template for the table.
--
-- /See:/ <https://developers.google.com/fusiontables Fusion Tables API Reference> for @fusiontables.template.insert@.
module Network.Google.Resource.FusionTables.Template.Insert
    (
    -- * REST Resource
      TemplateInsertResource

    -- * Creating a Request
    , templateInsert
    , TemplateInsert

    -- * Request Lenses
    , tiiPayload
    , tiiTableId
    , tiiFields
    ) where

import Network.Google.FusionTables.Types
import Network.Google.Prelude

-- | A resource alias for @fusiontables.template.insert@ method which the
-- 'TemplateInsert' request conforms to.
type TemplateInsertResource =
     "fusiontables" :>
       "v2" :>
         "tables" :>
           Capture "tableId" Text :>
             "templates" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] Template :> Post '[JSON] Template

-- | Creates a new template for the table.
--
-- /See:/ 'templateInsert' smart constructor.
data TemplateInsert = TemplateInsert'
    { _tiiPayload :: !Template
    , _tiiTableId :: !Text
    , _tiiFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TemplateInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiiPayload'
--
-- * 'tiiTableId'
--
-- * 'tiiFields'
templateInsert
    :: Template -- ^ 'tiiPayload'
    -> Text -- ^ 'tiiTableId'
    -> TemplateInsert
templateInsert pTiiPayload_ pTiiTableId_ = 
    TemplateInsert'
    { _tiiPayload = pTiiPayload_
    , _tiiTableId = pTiiTableId_
    , _tiiFields = Nothing
    }

-- | Multipart request metadata.
tiiPayload :: Lens' TemplateInsert Template
tiiPayload
  = lens _tiiPayload (\ s a -> s{_tiiPayload = a})

-- | Table for which a new template is being created
tiiTableId :: Lens' TemplateInsert Text
tiiTableId
  = lens _tiiTableId (\ s a -> s{_tiiTableId = a})

-- | Selector specifying which fields to include in a partial response.
tiiFields :: Lens' TemplateInsert (Maybe Text)
tiiFields
  = lens _tiiFields (\ s a -> s{_tiiFields = a})

instance GoogleRequest TemplateInsert where
        type Rs TemplateInsert = Template
        type Scopes TemplateInsert =
             '["https://www.googleapis.com/auth/fusiontables"]
        requestClient TemplateInsert'{..}
          = go _tiiTableId _tiiFields (Just AltJSON)
              _tiiPayload
              fusionTablesService
          where go
                  = buildClient (Proxy :: Proxy TemplateInsertResource)
                      mempty
