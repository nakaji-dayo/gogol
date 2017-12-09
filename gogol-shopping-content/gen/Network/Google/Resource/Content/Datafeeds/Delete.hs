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
-- Module      : Network.Google.Resource.Content.Datafeeds.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a datafeed configuration from your Merchant Center account.
--
-- /See:/ <https://developers.google.com/shopping-content Content API for Shopping Reference> for @content.datafeeds.delete@.
module Network.Google.Resource.Content.Datafeeds.Delete
    (
    -- * REST Resource
      DatafeedsDeleteResource

    -- * Creating a Request
    , datafeedsDelete
    , DatafeedsDelete

    -- * Request Lenses
    , ddMerchantId
    , ddDatafeedId
    , ddDryRun
    , ddFields
    ) where

import Network.Google.Prelude
import Network.Google.ShoppingContent.Types

-- | A resource alias for @content.datafeeds.delete@ method which the
-- 'DatafeedsDelete' request conforms to.
type DatafeedsDeleteResource =
     "content" :>
       "v2" :>
         Capture "merchantId" (Textual Word64) :>
           "datafeeds" :>
             Capture "datafeedId" (Textual Word64) :>
               QueryParam "dryRun" Bool :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Deletes a datafeed configuration from your Merchant Center account.
--
-- /See:/ 'datafeedsDelete' smart constructor.
data DatafeedsDelete = DatafeedsDelete'
    { _ddMerchantId :: !(Textual Word64)
    , _ddDatafeedId :: !(Textual Word64)
    , _ddDryRun :: !(Maybe Bool)
    , _ddFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'DatafeedsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddMerchantId'
--
-- * 'ddDatafeedId'
--
-- * 'ddDryRun'
--
-- * 'ddFields'
datafeedsDelete
    :: Word64 -- ^ 'ddMerchantId'
    -> Word64 -- ^ 'ddDatafeedId'
    -> DatafeedsDelete
datafeedsDelete pDdMerchantId_ pDdDatafeedId_ = 
    DatafeedsDelete'
    { _ddMerchantId = _Coerce # pDdMerchantId_
    , _ddDatafeedId = _Coerce # pDdDatafeedId_
    , _ddDryRun = Nothing
    , _ddFields = Nothing
    }

-- | The ID of the account that manages the datafeed. This account cannot be
-- a multi-client account.
ddMerchantId :: Lens' DatafeedsDelete Word64
ddMerchantId
  = lens _ddMerchantId (\ s a -> s{_ddMerchantId = a})
      . _Coerce

-- | The ID of the datafeed.
ddDatafeedId :: Lens' DatafeedsDelete Word64
ddDatafeedId
  = lens _ddDatafeedId (\ s a -> s{_ddDatafeedId = a})
      . _Coerce

-- | Flag to run the request in dry-run mode.
ddDryRun :: Lens' DatafeedsDelete (Maybe Bool)
ddDryRun = lens _ddDryRun (\ s a -> s{_ddDryRun = a})

-- | Selector specifying which fields to include in a partial response.
ddFields :: Lens' DatafeedsDelete (Maybe Text)
ddFields = lens _ddFields (\ s a -> s{_ddFields = a})

instance GoogleRequest DatafeedsDelete where
        type Rs DatafeedsDelete = ()
        type Scopes DatafeedsDelete =
             '["https://www.googleapis.com/auth/content"]
        requestClient DatafeedsDelete'{..}
          = go _ddMerchantId _ddDatafeedId _ddDryRun _ddFields
              (Just AltJSON)
              shoppingContentService
          where go
                  = buildClient
                      (Proxy :: Proxy DatafeedsDeleteResource)
                      mempty
