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
-- Module      : Network.Google.Resource.AndroidEnterprise.Grouplicenses.List
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves IDs of all products for which the enterprise has a group
-- license.
--
-- /See:/ <https://developers.google.com/android/work/play/emm-api Google Play EMM API Reference> for @androidenterprise.grouplicenses.list@.
module Network.Google.Resource.AndroidEnterprise.Grouplicenses.List
    (
    -- * REST Resource
      GrouplicensesListResource

    -- * Creating a Request
    , grouplicensesList
    , GrouplicensesList

    -- * Request Lenses
    , glEnterpriseId
    , glFields
    ) where

import Network.Google.AndroidEnterprise.Types
import Network.Google.Prelude

-- | A resource alias for @androidenterprise.grouplicenses.list@ method which the
-- 'GrouplicensesList' request conforms to.
type GrouplicensesListResource =
     "androidenterprise" :>
       "v1" :>
         "enterprises" :>
           Capture "enterpriseId" Text :>
             "groupLicenses" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   Get '[JSON] GroupLicensesListResponse

-- | Retrieves IDs of all products for which the enterprise has a group
-- license.
--
-- /See:/ 'grouplicensesList' smart constructor.
data GrouplicensesList = GrouplicensesList'
    { _glEnterpriseId :: !Text
    , _glFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GrouplicensesList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glEnterpriseId'
--
-- * 'glFields'
grouplicensesList
    :: Text -- ^ 'glEnterpriseId'
    -> GrouplicensesList
grouplicensesList pGlEnterpriseId_ = 
    GrouplicensesList'
    { _glEnterpriseId = pGlEnterpriseId_
    , _glFields = Nothing
    }

-- | The ID of the enterprise.
glEnterpriseId :: Lens' GrouplicensesList Text
glEnterpriseId
  = lens _glEnterpriseId
      (\ s a -> s{_glEnterpriseId = a})

-- | Selector specifying which fields to include in a partial response.
glFields :: Lens' GrouplicensesList (Maybe Text)
glFields = lens _glFields (\ s a -> s{_glFields = a})

instance GoogleRequest GrouplicensesList where
        type Rs GrouplicensesList = GroupLicensesListResponse
        type Scopes GrouplicensesList =
             '["https://www.googleapis.com/auth/androidenterprise"]
        requestClient GrouplicensesList'{..}
          = go _glEnterpriseId _glFields (Just AltJSON)
              androidEnterpriseService
          where go
                  = buildClient
                      (Proxy :: Proxy GrouplicensesListResource)
                      mempty
