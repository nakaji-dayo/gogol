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
-- Module      : Network.Google.Resource.AndroidEnterprise.Enterprises.GetAndroidDevicePolicyConfig
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Android Device Policy config resource.
--
-- /See:/ <https://developers.google.com/android/work/play/emm-api Google Play EMM API Reference> for @androidenterprise.enterprises.getAndroidDevicePolicyConfig@.
module Network.Google.Resource.AndroidEnterprise.Enterprises.GetAndroidDevicePolicyConfig
    (
    -- * REST Resource
      EnterprisesGetAndroidDevicePolicyConfigResource

    -- * Creating a Request
    , enterprisesGetAndroidDevicePolicyConfig
    , EnterprisesGetAndroidDevicePolicyConfig

    -- * Request Lenses
    , egadpcEnterpriseId
    , egadpcFields
    ) where

import Network.Google.AndroidEnterprise.Types
import Network.Google.Prelude

-- | A resource alias for @androidenterprise.enterprises.getAndroidDevicePolicyConfig@ method which the
-- 'EnterprisesGetAndroidDevicePolicyConfig' request conforms to.
type EnterprisesGetAndroidDevicePolicyConfigResource
     =
     "androidenterprise" :>
       "v1" :>
         "enterprises" :>
           Capture "enterpriseId" Text :>
             "androidDevicePolicyConfig" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   Get '[JSON] AndroidDevicePolicyConfig

-- | Returns the Android Device Policy config resource.
--
-- /See:/ 'enterprisesGetAndroidDevicePolicyConfig' smart constructor.
data EnterprisesGetAndroidDevicePolicyConfig = EnterprisesGetAndroidDevicePolicyConfig'
    { _egadpcEnterpriseId :: !Text
    , _egadpcFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnterprisesGetAndroidDevicePolicyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'egadpcEnterpriseId'
--
-- * 'egadpcFields'
enterprisesGetAndroidDevicePolicyConfig
    :: Text -- ^ 'egadpcEnterpriseId'
    -> EnterprisesGetAndroidDevicePolicyConfig
enterprisesGetAndroidDevicePolicyConfig pEgadpcEnterpriseId_ = 
    EnterprisesGetAndroidDevicePolicyConfig'
    { _egadpcEnterpriseId = pEgadpcEnterpriseId_
    , _egadpcFields = Nothing
    }

-- | The ID of the enterprise.
egadpcEnterpriseId :: Lens' EnterprisesGetAndroidDevicePolicyConfig Text
egadpcEnterpriseId
  = lens _egadpcEnterpriseId
      (\ s a -> s{_egadpcEnterpriseId = a})

-- | Selector specifying which fields to include in a partial response.
egadpcFields :: Lens' EnterprisesGetAndroidDevicePolicyConfig (Maybe Text)
egadpcFields
  = lens _egadpcFields (\ s a -> s{_egadpcFields = a})

instance GoogleRequest
         EnterprisesGetAndroidDevicePolicyConfig where
        type Rs EnterprisesGetAndroidDevicePolicyConfig =
             AndroidDevicePolicyConfig
        type Scopes EnterprisesGetAndroidDevicePolicyConfig =
             '["https://www.googleapis.com/auth/androidenterprise"]
        requestClient
          EnterprisesGetAndroidDevicePolicyConfig'{..}
          = go _egadpcEnterpriseId _egadpcFields (Just AltJSON)
              androidEnterpriseService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           EnterprisesGetAndroidDevicePolicyConfigResource)
                      mempty
