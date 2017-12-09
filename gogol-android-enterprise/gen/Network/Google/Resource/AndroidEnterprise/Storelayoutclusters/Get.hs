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
-- Module      : Network.Google.Resource.AndroidEnterprise.Storelayoutclusters.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of a cluster.
--
-- /See:/ <https://developers.google.com/android/work/play/emm-api Google Play EMM API Reference> for @androidenterprise.storelayoutclusters.get@.
module Network.Google.Resource.AndroidEnterprise.Storelayoutclusters.Get
    (
    -- * REST Resource
      StorelayoutclustersGetResource

    -- * Creating a Request
    , storelayoutclustersGet
    , StorelayoutclustersGet

    -- * Request Lenses
    , stoEnterpriseId
    , stoPageId
    , stoClusterId
    , stoFields
    ) where

import Network.Google.AndroidEnterprise.Types
import Network.Google.Prelude

-- | A resource alias for @androidenterprise.storelayoutclusters.get@ method which the
-- 'StorelayoutclustersGet' request conforms to.
type StorelayoutclustersGetResource =
     "androidenterprise" :>
       "v1" :>
         "enterprises" :>
           Capture "enterpriseId" Text :>
             "storeLayout" :>
               "pages" :>
                 Capture "pageId" Text :>
                   "clusters" :>
                     Capture "clusterId" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :> Get '[JSON] StoreCluster

-- | Retrieves details of a cluster.
--
-- /See:/ 'storelayoutclustersGet' smart constructor.
data StorelayoutclustersGet = StorelayoutclustersGet'
    { _stoEnterpriseId :: !Text
    , _stoPageId :: !Text
    , _stoClusterId :: !Text
    , _stoFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'StorelayoutclustersGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stoEnterpriseId'
--
-- * 'stoPageId'
--
-- * 'stoClusterId'
--
-- * 'stoFields'
storelayoutclustersGet
    :: Text -- ^ 'stoEnterpriseId'
    -> Text -- ^ 'stoPageId'
    -> Text -- ^ 'stoClusterId'
    -> StorelayoutclustersGet
storelayoutclustersGet pStoEnterpriseId_ pStoPageId_ pStoClusterId_ = 
    StorelayoutclustersGet'
    { _stoEnterpriseId = pStoEnterpriseId_
    , _stoPageId = pStoPageId_
    , _stoClusterId = pStoClusterId_
    , _stoFields = Nothing
    }

-- | The ID of the enterprise.
stoEnterpriseId :: Lens' StorelayoutclustersGet Text
stoEnterpriseId
  = lens _stoEnterpriseId
      (\ s a -> s{_stoEnterpriseId = a})

-- | The ID of the page.
stoPageId :: Lens' StorelayoutclustersGet Text
stoPageId
  = lens _stoPageId (\ s a -> s{_stoPageId = a})

-- | The ID of the cluster.
stoClusterId :: Lens' StorelayoutclustersGet Text
stoClusterId
  = lens _stoClusterId (\ s a -> s{_stoClusterId = a})

-- | Selector specifying which fields to include in a partial response.
stoFields :: Lens' StorelayoutclustersGet (Maybe Text)
stoFields
  = lens _stoFields (\ s a -> s{_stoFields = a})

instance GoogleRequest StorelayoutclustersGet where
        type Rs StorelayoutclustersGet = StoreCluster
        type Scopes StorelayoutclustersGet =
             '["https://www.googleapis.com/auth/androidenterprise"]
        requestClient StorelayoutclustersGet'{..}
          = go _stoEnterpriseId _stoPageId _stoClusterId
              _stoFields
              (Just AltJSON)
              androidEnterpriseService
          where go
                  = buildClient
                      (Proxy :: Proxy StorelayoutclustersGetResource)
                      mempty
