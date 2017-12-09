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
-- Module      : Network.Google.Resource.AndroidEnterprise.Enterprises.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Looks up an enterprise by domain name. This is only supported for
-- enterprises created via the Google-initiated creation flow. Lookup of
-- the id is not needed for enterprises created via the EMM-initiated flow
-- since the EMM learns the enterprise ID in the callback specified in the
-- Enterprises.generateSignupUrl call.
--
-- /See:/ <https://developers.google.com/android/work/play/emm-api Google Play EMM API Reference> for @androidenterprise.enterprises.list@.
module Network.Google.Resource.AndroidEnterprise.Enterprises.List
    (
    -- * REST Resource
      EnterprisesListResource

    -- * Creating a Request
    , enterprisesList
    , EnterprisesList

    -- * Request Lenses
    , elDomain
    , elFields
    ) where

import Network.Google.AndroidEnterprise.Types
import Network.Google.Prelude

-- | A resource alias for @androidenterprise.enterprises.list@ method which the
-- 'EnterprisesList' request conforms to.
type EnterprisesListResource =
     "androidenterprise" :>
       "v1" :>
         "enterprises" :>
           QueryParam "domain" Text :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 Get '[JSON] EnterprisesListResponse

-- | Looks up an enterprise by domain name. This is only supported for
-- enterprises created via the Google-initiated creation flow. Lookup of
-- the id is not needed for enterprises created via the EMM-initiated flow
-- since the EMM learns the enterprise ID in the callback specified in the
-- Enterprises.generateSignupUrl call.
--
-- /See:/ 'enterprisesList' smart constructor.
data EnterprisesList = EnterprisesList'
    { _elDomain :: !Text
    , _elFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnterprisesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elDomain'
--
-- * 'elFields'
enterprisesList
    :: Text -- ^ 'elDomain'
    -> EnterprisesList
enterprisesList pElDomain_ = 
    EnterprisesList'
    { _elDomain = pElDomain_
    , _elFields = Nothing
    }

-- | The exact primary domain name of the enterprise to look up.
elDomain :: Lens' EnterprisesList Text
elDomain = lens _elDomain (\ s a -> s{_elDomain = a})

-- | Selector specifying which fields to include in a partial response.
elFields :: Lens' EnterprisesList (Maybe Text)
elFields = lens _elFields (\ s a -> s{_elFields = a})

instance GoogleRequest EnterprisesList where
        type Rs EnterprisesList = EnterprisesListResponse
        type Scopes EnterprisesList =
             '["https://www.googleapis.com/auth/androidenterprise"]
        requestClient EnterprisesList'{..}
          = go (Just _elDomain) _elFields (Just AltJSON)
              androidEnterpriseService
          where go
                  = buildClient
                      (Proxy :: Proxy EnterprisesListResource)
                      mempty
