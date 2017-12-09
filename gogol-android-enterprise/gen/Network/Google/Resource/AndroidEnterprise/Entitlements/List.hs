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
-- Module      : Network.Google.Resource.AndroidEnterprise.Entitlements.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all entitlements for the specified user. Only the ID is set.
--
-- /See:/ <https://developers.google.com/android/work/play/emm-api Google Play EMM API Reference> for @androidenterprise.entitlements.list@.
module Network.Google.Resource.AndroidEnterprise.Entitlements.List
    (
    -- * REST Resource
      EntitlementsListResource

    -- * Creating a Request
    , entitlementsList
    , EntitlementsList

    -- * Request Lenses
    , ellEnterpriseId
    , ellUserId
    , ellFields
    ) where

import Network.Google.AndroidEnterprise.Types
import Network.Google.Prelude

-- | A resource alias for @androidenterprise.entitlements.list@ method which the
-- 'EntitlementsList' request conforms to.
type EntitlementsListResource =
     "androidenterprise" :>
       "v1" :>
         "enterprises" :>
           Capture "enterpriseId" Text :>
             "users" :>
               Capture "userId" Text :>
                 "entitlements" :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :>
                       Get '[JSON] EntitlementsListResponse

-- | Lists all entitlements for the specified user. Only the ID is set.
--
-- /See:/ 'entitlementsList' smart constructor.
data EntitlementsList = EntitlementsList'
    { _ellEnterpriseId :: !Text
    , _ellUserId :: !Text
    , _ellFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EntitlementsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ellEnterpriseId'
--
-- * 'ellUserId'
--
-- * 'ellFields'
entitlementsList
    :: Text -- ^ 'ellEnterpriseId'
    -> Text -- ^ 'ellUserId'
    -> EntitlementsList
entitlementsList pEllEnterpriseId_ pEllUserId_ = 
    EntitlementsList'
    { _ellEnterpriseId = pEllEnterpriseId_
    , _ellUserId = pEllUserId_
    , _ellFields = Nothing
    }

-- | The ID of the enterprise.
ellEnterpriseId :: Lens' EntitlementsList Text
ellEnterpriseId
  = lens _ellEnterpriseId
      (\ s a -> s{_ellEnterpriseId = a})

-- | The ID of the user.
ellUserId :: Lens' EntitlementsList Text
ellUserId
  = lens _ellUserId (\ s a -> s{_ellUserId = a})

-- | Selector specifying which fields to include in a partial response.
ellFields :: Lens' EntitlementsList (Maybe Text)
ellFields
  = lens _ellFields (\ s a -> s{_ellFields = a})

instance GoogleRequest EntitlementsList where
        type Rs EntitlementsList = EntitlementsListResponse
        type Scopes EntitlementsList =
             '["https://www.googleapis.com/auth/androidenterprise"]
        requestClient EntitlementsList'{..}
          = go _ellEnterpriseId _ellUserId _ellFields
              (Just AltJSON)
              androidEnterpriseService
          where go
                  = buildClient
                      (Proxy :: Proxy EntitlementsListResource)
                      mempty
