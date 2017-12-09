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
-- Module      : Network.Google.Resource.Games.Applications.Get
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata of the application with the given ID. If the
-- requested application is not available for the specified platformType,
-- the returned response will not include any instance data.
--
-- /See:/ <https://developers.google.com/games/services/ Google Play Game Services API Reference> for @games.applications.get@.
module Network.Google.Resource.Games.Applications.Get
    (
    -- * REST Resource
      ApplicationsGetResource

    -- * Creating a Request
    , applicationsGet
    , ApplicationsGet

    -- * Request Lenses
    , agConsistencyToken
    , agApplicationId
    , agPlatformType
    , agLanguage
    , agFields
    ) where

import Network.Google.Games.Types
import Network.Google.Prelude

-- | A resource alias for @games.applications.get@ method which the
-- 'ApplicationsGet' request conforms to.
type ApplicationsGetResource =
     "games" :>
       "v1" :>
         "applications" :>
           Capture "applicationId" Text :>
             QueryParam "consistencyToken" (Textual Int64) :>
               QueryParam "platformType" ApplicationsGetPlatformType
                 :>
                 QueryParam "language" Text :>
                   QueryParam "fields" Text :>
                     QueryParam "alt" AltJSON :> Get '[JSON] Application

-- | Retrieves the metadata of the application with the given ID. If the
-- requested application is not available for the specified platformType,
-- the returned response will not include any instance data.
--
-- /See:/ 'applicationsGet' smart constructor.
data ApplicationsGet = ApplicationsGet'
    { _agConsistencyToken :: !(Maybe (Textual Int64))
    , _agApplicationId :: !Text
    , _agPlatformType :: !(Maybe ApplicationsGetPlatformType)
    , _agLanguage :: !(Maybe Text)
    , _agFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ApplicationsGet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agConsistencyToken'
--
-- * 'agApplicationId'
--
-- * 'agPlatformType'
--
-- * 'agLanguage'
--
-- * 'agFields'
applicationsGet
    :: Text -- ^ 'agApplicationId'
    -> ApplicationsGet
applicationsGet pAgApplicationId_ = 
    ApplicationsGet'
    { _agConsistencyToken = Nothing
    , _agApplicationId = pAgApplicationId_
    , _agPlatformType = Nothing
    , _agLanguage = Nothing
    , _agFields = Nothing
    }

-- | The last-seen mutation timestamp.
agConsistencyToken :: Lens' ApplicationsGet (Maybe Int64)
agConsistencyToken
  = lens _agConsistencyToken
      (\ s a -> s{_agConsistencyToken = a})
      . mapping _Coerce

-- | The application ID from the Google Play developer console.
agApplicationId :: Lens' ApplicationsGet Text
agApplicationId
  = lens _agApplicationId
      (\ s a -> s{_agApplicationId = a})

-- | Restrict application details returned to the specific platform.
agPlatformType :: Lens' ApplicationsGet (Maybe ApplicationsGetPlatformType)
agPlatformType
  = lens _agPlatformType
      (\ s a -> s{_agPlatformType = a})

-- | The preferred language to use for strings returned by this method.
agLanguage :: Lens' ApplicationsGet (Maybe Text)
agLanguage
  = lens _agLanguage (\ s a -> s{_agLanguage = a})

-- | Selector specifying which fields to include in a partial response.
agFields :: Lens' ApplicationsGet (Maybe Text)
agFields = lens _agFields (\ s a -> s{_agFields = a})

instance GoogleRequest ApplicationsGet where
        type Rs ApplicationsGet = Application
        type Scopes ApplicationsGet =
             '["https://www.googleapis.com/auth/games",
               "https://www.googleapis.com/auth/plus.login"]
        requestClient ApplicationsGet'{..}
          = go _agApplicationId _agConsistencyToken
              _agPlatformType
              _agLanguage
              _agFields
              (Just AltJSON)
              gamesService
          where go
                  = buildClient
                      (Proxy :: Proxy ApplicationsGetResource)
                      mempty
