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
-- Module      : Network.Google.Resource.FusionTables.Template.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a template
--
-- /See:/ <https://developers.google.com/fusiontables Fusion Tables API Reference> for @fusiontables.template.delete@.
module Network.Google.Resource.FusionTables.Template.Delete
    (
    -- * REST Resource
      TemplateDeleteResource

    -- * Creating a Request
    , templateDelete
    , TemplateDelete

    -- * Request Lenses
    , temTemplateId
    , temTableId
    , temFields
    ) where

import Network.Google.FusionTables.Types
import Network.Google.Prelude

-- | A resource alias for @fusiontables.template.delete@ method which the
-- 'TemplateDelete' request conforms to.
type TemplateDeleteResource =
     "fusiontables" :>
       "v2" :>
         "tables" :>
           Capture "tableId" Text :>
             "templates" :>
               Capture "templateId" (Textual Int32) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Deletes a template
--
-- /See:/ 'templateDelete' smart constructor.
data TemplateDelete = TemplateDelete'
    { _temTemplateId :: !(Textual Int32)
    , _temTableId :: !Text
    , _temFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TemplateDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'temTemplateId'
--
-- * 'temTableId'
--
-- * 'temFields'
templateDelete
    :: Int32 -- ^ 'temTemplateId'
    -> Text -- ^ 'temTableId'
    -> TemplateDelete
templateDelete pTemTemplateId_ pTemTableId_ = 
    TemplateDelete'
    { _temTemplateId = _Coerce # pTemTemplateId_
    , _temTableId = pTemTableId_
    , _temFields = Nothing
    }

-- | Identifier for the template which is being deleted
temTemplateId :: Lens' TemplateDelete Int32
temTemplateId
  = lens _temTemplateId
      (\ s a -> s{_temTemplateId = a})
      . _Coerce

-- | Table from which the template is being deleted
temTableId :: Lens' TemplateDelete Text
temTableId
  = lens _temTableId (\ s a -> s{_temTableId = a})

-- | Selector specifying which fields to include in a partial response.
temFields :: Lens' TemplateDelete (Maybe Text)
temFields
  = lens _temFields (\ s a -> s{_temFields = a})

instance GoogleRequest TemplateDelete where
        type Rs TemplateDelete = ()
        type Scopes TemplateDelete =
             '["https://www.googleapis.com/auth/fusiontables"]
        requestClient TemplateDelete'{..}
          = go _temTableId _temTemplateId _temFields
              (Just AltJSON)
              fusionTablesService
          where go
                  = buildClient (Proxy :: Proxy TemplateDeleteResource)
                      mempty
