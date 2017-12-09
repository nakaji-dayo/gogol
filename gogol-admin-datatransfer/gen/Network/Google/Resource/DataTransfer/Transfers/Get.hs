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
-- Module      : Network.Google.Resource.DataTransfer.Transfers.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a data transfer request by its resource ID.
--
-- /See:/ <https://developers.google.com/admin-sdk/data-transfer/ Admin Data Transfer API Reference> for @datatransfer.transfers.get@.
module Network.Google.Resource.DataTransfer.Transfers.Get
    (
    -- * REST Resource
      TransfersGetResource

    -- * Creating a Request
    , transfersGet
    , TransfersGet

    -- * Request Lenses
    , tgDataTransferId
    , tgFields
    ) where

import Network.Google.DataTransfer.Types
import Network.Google.Prelude

-- | A resource alias for @datatransfer.transfers.get@ method which the
-- 'TransfersGet' request conforms to.
type TransfersGetResource =
     "admin" :>
       "datatransfer" :>
         "v1" :>
           "transfers" :>
             Capture "dataTransferId" Text :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] DataTransfer

-- | Retrieves a data transfer request by its resource ID.
--
-- /See:/ 'transfersGet' smart constructor.
data TransfersGet = TransfersGet'
    { _tgDataTransferId :: !Text
    , _tgFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'TransfersGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgDataTransferId'
--
-- * 'tgFields'
transfersGet
    :: Text -- ^ 'tgDataTransferId'
    -> TransfersGet
transfersGet pTgDataTransferId_ = 
    TransfersGet'
    { _tgDataTransferId = pTgDataTransferId_
    , _tgFields = Nothing
    }

-- | ID of the resource to be retrieved. This is returned in the response
-- from the insert method.
tgDataTransferId :: Lens' TransfersGet Text
tgDataTransferId
  = lens _tgDataTransferId
      (\ s a -> s{_tgDataTransferId = a})

-- | Selector specifying which fields to include in a partial response.
tgFields :: Lens' TransfersGet (Maybe Text)
tgFields = lens _tgFields (\ s a -> s{_tgFields = a})

instance GoogleRequest TransfersGet where
        type Rs TransfersGet = DataTransfer
        type Scopes TransfersGet =
             '["https://www.googleapis.com/auth/admin.datatransfer",
               "https://www.googleapis.com/auth/admin.datatransfer.readonly"]
        requestClient TransfersGet'{..}
          = go _tgDataTransferId _tgFields (Just AltJSON)
              dataTransferService
          where go
                  = buildClient (Proxy :: Proxy TransfersGetResource)
                      mempty
