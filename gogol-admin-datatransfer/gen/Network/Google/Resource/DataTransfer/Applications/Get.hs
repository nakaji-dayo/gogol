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
-- Module      : Network.Google.Resource.DataTransfer.Applications.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an application for the given application ID.
--
-- /See:/ <https://developers.google.com/admin-sdk/data-transfer/ Admin Data Transfer API Reference> for @datatransfer.applications.get@.
module Network.Google.Resource.DataTransfer.Applications.Get
    (
    -- * REST Resource
      ApplicationsGetResource

    -- * Creating a Request
    , applicationsGet
    , ApplicationsGet

    -- * Request Lenses
    , agApplicationId
    , agFields
    ) where

import Network.Google.DataTransfer.Types
import Network.Google.Prelude

-- | A resource alias for @datatransfer.applications.get@ method which the
-- 'ApplicationsGet' request conforms to.
type ApplicationsGetResource =
     "admin" :>
       "datatransfer" :>
         "v1" :>
           "applications" :>
             Capture "applicationId" (Textual Int64) :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :> Get '[JSON] Application

-- | Retrieves information about an application for the given application ID.
--
-- /See:/ 'applicationsGet' smart constructor.
data ApplicationsGet = ApplicationsGet'
    { _agApplicationId :: !(Textual Int64)
    , _agFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ApplicationsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agApplicationId'
--
-- * 'agFields'
applicationsGet
    :: Int64 -- ^ 'agApplicationId'
    -> ApplicationsGet
applicationsGet pAgApplicationId_ = 
    ApplicationsGet'
    { _agApplicationId = _Coerce # pAgApplicationId_
    , _agFields = Nothing
    }

-- | ID of the application resource to be retrieved.
agApplicationId :: Lens' ApplicationsGet Int64
agApplicationId
  = lens _agApplicationId
      (\ s a -> s{_agApplicationId = a})
      . _Coerce

-- | Selector specifying which fields to include in a partial response.
agFields :: Lens' ApplicationsGet (Maybe Text)
agFields = lens _agFields (\ s a -> s{_agFields = a})

instance GoogleRequest ApplicationsGet where
        type Rs ApplicationsGet = Application
        type Scopes ApplicationsGet =
             '["https://www.googleapis.com/auth/admin.datatransfer",
               "https://www.googleapis.com/auth/admin.datatransfer.readonly"]
        requestClient ApplicationsGet'{..}
          = go _agApplicationId _agFields (Just AltJSON)
              dataTransferService
          where go
                  = buildClient
                      (Proxy :: Proxy ApplicationsGetResource)
                      mempty
