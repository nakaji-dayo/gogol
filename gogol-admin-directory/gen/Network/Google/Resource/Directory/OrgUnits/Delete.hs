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
-- Module      : Network.Google.Resource.Directory.OrgUnits.Delete
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove organizational unit
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.orgunits.delete@.
module Network.Google.Resource.Directory.OrgUnits.Delete
    (
    -- * REST Resource
      OrgUnitsDeleteResource

    -- * Creating a Request
    , orgUnitsDelete
    , OrgUnitsDelete

    -- * Request Lenses
    , oudOrgUnitPath
    , oudCustomerId
    , oudFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.orgunits.delete@ method which the
-- 'OrgUnitsDelete' request conforms to.
type OrgUnitsDeleteResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "customer" :>
             Capture "customerId" Text :>
               "orgunits" :>
                 Captures "orgUnitPath" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :> Delete '[JSON] ()

-- | Remove organizational unit
--
-- /See:/ 'orgUnitsDelete' smart constructor.
data OrgUnitsDelete = OrgUnitsDelete'
    { _oudOrgUnitPath :: ![Text]
    , _oudCustomerId :: !Text
    , _oudFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrgUnitsDelete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oudOrgUnitPath'
--
-- * 'oudCustomerId'
--
-- * 'oudFields'
orgUnitsDelete
    :: [Text] -- ^ 'oudOrgUnitPath'
    -> Text -- ^ 'oudCustomerId'
    -> OrgUnitsDelete
orgUnitsDelete pOudOrgUnitPath_ pOudCustomerId_ = 
    OrgUnitsDelete'
    { _oudOrgUnitPath = _Coerce # pOudOrgUnitPath_
    , _oudCustomerId = pOudCustomerId_
    , _oudFields = Nothing
    }

-- | Full path of the organizational unit or its ID
oudOrgUnitPath :: Lens' OrgUnitsDelete [Text]
oudOrgUnitPath
  = lens _oudOrgUnitPath
      (\ s a -> s{_oudOrgUnitPath = a})
      . _Coerce

-- | Immutable ID of the G Suite account
oudCustomerId :: Lens' OrgUnitsDelete Text
oudCustomerId
  = lens _oudCustomerId
      (\ s a -> s{_oudCustomerId = a})

-- | Selector specifying which fields to include in a partial response.
oudFields :: Lens' OrgUnitsDelete (Maybe Text)
oudFields
  = lens _oudFields (\ s a -> s{_oudFields = a})

instance GoogleRequest OrgUnitsDelete where
        type Rs OrgUnitsDelete = ()
        type Scopes OrgUnitsDelete =
             '["https://www.googleapis.com/auth/admin.directory.orgunit"]
        requestClient OrgUnitsDelete'{..}
          = go _oudCustomerId _oudOrgUnitPath _oudFields
              (Just AltJSON)
              directoryService
          where go
                  = buildClient (Proxy :: Proxy OrgUnitsDeleteResource)
                      mempty
