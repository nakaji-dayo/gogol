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
-- Module      : Network.Google.Resource.AndroidEnterprise.Storelayoutpages.Insert
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts a new store page.
--
-- /See:/ <https://developers.google.com/android/work/play/emm-api Google Play EMM API Reference> for @androidenterprise.storelayoutpages.insert@.
module Network.Google.Resource.AndroidEnterprise.Storelayoutpages.Insert
    (
    -- * REST Resource
      StorelayoutpagesInsertResource

    -- * Creating a Request
    , storelayoutpagesInsert
    , StorelayoutpagesInsert

    -- * Request Lenses
    , siEnterpriseId
    , siPayload
    , siFields
    ) where

import Network.Google.AndroidEnterprise.Types
import Network.Google.Prelude

-- | A resource alias for @androidenterprise.storelayoutpages.insert@ method which the
-- 'StorelayoutpagesInsert' request conforms to.
type StorelayoutpagesInsertResource =
     "androidenterprise" :>
       "v1" :>
         "enterprises" :>
           Capture "enterpriseId" Text :>
             "storeLayout" :>
               "pages" :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] StorePage :> Post '[JSON] StorePage

-- | Inserts a new store page.
--
-- /See:/ 'storelayoutpagesInsert' smart constructor.
data StorelayoutpagesInsert = StorelayoutpagesInsert'
    { _siEnterpriseId :: !Text
    , _siPayload :: !StorePage
    , _siFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StorelayoutpagesInsert' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siEnterpriseId'
--
-- * 'siPayload'
--
-- * 'siFields'
storelayoutpagesInsert
    :: Text -- ^ 'siEnterpriseId'
    -> StorePage -- ^ 'siPayload'
    -> StorelayoutpagesInsert
storelayoutpagesInsert pSiEnterpriseId_ pSiPayload_ = 
    StorelayoutpagesInsert'
    { _siEnterpriseId = pSiEnterpriseId_
    , _siPayload = pSiPayload_
    , _siFields = Nothing
    }

-- | The ID of the enterprise.
siEnterpriseId :: Lens' StorelayoutpagesInsert Text
siEnterpriseId
  = lens _siEnterpriseId
      (\ s a -> s{_siEnterpriseId = a})

-- | Multipart request metadata.
siPayload :: Lens' StorelayoutpagesInsert StorePage
siPayload
  = lens _siPayload (\ s a -> s{_siPayload = a})

-- | Selector specifying which fields to include in a partial response.
siFields :: Lens' StorelayoutpagesInsert (Maybe Text)
siFields = lens _siFields (\ s a -> s{_siFields = a})

instance GoogleRequest StorelayoutpagesInsert where
        type Rs StorelayoutpagesInsert = StorePage
        type Scopes StorelayoutpagesInsert =
             '["https://www.googleapis.com/auth/androidenterprise"]
        requestClient StorelayoutpagesInsert'{..}
          = go _siEnterpriseId _siFields (Just AltJSON)
              _siPayload
              androidEnterpriseService
          where go
                  = buildClient
                      (Proxy :: Proxy StorelayoutpagesInsertResource)
                      mempty
