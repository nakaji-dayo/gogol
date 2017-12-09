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
-- Module      : Network.Google.Resource.AndroidPublisher.Edits.APKs.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /See:/ <https://developers.google.com/android-publisher Google Play Developer API Reference> for @androidpublisher.edits.apks.list@.
module Network.Google.Resource.AndroidPublisher.Edits.APKs.List
    (
    -- * REST Resource
      EditsAPKsListResource

    -- * Creating a Request
    , editsAPKsList
    , EditsAPKsList

    -- * Request Lenses
    , eapklPackageName
    , eapklEditId
    , eapklFields
    ) where

import Network.Google.AndroidPublisher.Types
import Network.Google.Prelude

-- | A resource alias for @androidpublisher.edits.apks.list@ method which the
-- 'EditsAPKsList' request conforms to.
type EditsAPKsListResource =
     "androidpublisher" :>
       "v2" :>
         "applications" :>
           Capture "packageName" Text :>
             "edits" :>
               Capture "editId" Text :>
                 "apks" :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       Get '[JSON] APKsListResponse

--
-- /See:/ 'editsAPKsList' smart constructor.
data EditsAPKsList = EditsAPKsList'
    { _eapklPackageName :: !Text
    , _eapklEditId :: !Text
    , _eapklFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EditsAPKsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eapklPackageName'
--
-- * 'eapklEditId'
--
-- * 'eapklFields'
editsAPKsList
    :: Text -- ^ 'eapklPackageName'
    -> Text -- ^ 'eapklEditId'
    -> EditsAPKsList
editsAPKsList pEapklPackageName_ pEapklEditId_ = 
    EditsAPKsList'
    { _eapklPackageName = pEapklPackageName_
    , _eapklEditId = pEapklEditId_
    , _eapklFields = Nothing
    }

-- | Unique identifier for the Android app that is being updated; for
-- example, \"com.spiffygame\".
eapklPackageName :: Lens' EditsAPKsList Text
eapklPackageName
  = lens _eapklPackageName
      (\ s a -> s{_eapklPackageName = a})

-- | Unique identifier for this edit.
eapklEditId :: Lens' EditsAPKsList Text
eapklEditId
  = lens _eapklEditId (\ s a -> s{_eapklEditId = a})

-- | Selector specifying which fields to include in a partial response.
eapklFields :: Lens' EditsAPKsList (Maybe Text)
eapklFields
  = lens _eapklFields (\ s a -> s{_eapklFields = a})

instance GoogleRequest EditsAPKsList where
        type Rs EditsAPKsList = APKsListResponse
        type Scopes EditsAPKsList =
             '["https://www.googleapis.com/auth/androidpublisher"]
        requestClient EditsAPKsList'{..}
          = go _eapklPackageName _eapklEditId _eapklFields
              (Just AltJSON)
              androidPublisherService
          where go
                  = buildClient (Proxy :: Proxy EditsAPKsListResource)
                      mempty
