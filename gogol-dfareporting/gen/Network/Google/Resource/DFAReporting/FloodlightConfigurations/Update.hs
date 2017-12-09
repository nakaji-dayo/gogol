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
-- Module      : Network.Google.Resource.DFAReporting.FloodlightConfigurations.Update
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing floodlight configuration.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.floodlightConfigurations.update@.
module Network.Google.Resource.DFAReporting.FloodlightConfigurations.Update
    (
    -- * REST Resource
      FloodlightConfigurationsUpdateResource

    -- * Creating a Request
    , floodlightConfigurationsUpdate
    , FloodlightConfigurationsUpdate

    -- * Request Lenses
    , fcuProFileId
    , fcuPayload
    , fcuFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.floodlightConfigurations.update@ method which the
-- 'FloodlightConfigurationsUpdate' request conforms to.
type FloodlightConfigurationsUpdateResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "floodlightConfigurations" :>
               QueryParam "fields" Text :>
                 QueryParam "alt" AltJSON :>
                   ReqBody '[JSON] FloodlightConfiguration :>
                     Put '[JSON] FloodlightConfiguration

-- | Updates an existing floodlight configuration.
--
-- /See:/ 'floodlightConfigurationsUpdate' smart constructor.
data FloodlightConfigurationsUpdate = FloodlightConfigurationsUpdate'
    { _fcuProFileId :: !(Textual Int64)
    , _fcuPayload :: !FloodlightConfiguration
    , _fcuFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FloodlightConfigurationsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcuProFileId'
--
-- * 'fcuPayload'
--
-- * 'fcuFields'
floodlightConfigurationsUpdate
    :: Int64 -- ^ 'fcuProFileId'
    -> FloodlightConfiguration -- ^ 'fcuPayload'
    -> FloodlightConfigurationsUpdate
floodlightConfigurationsUpdate pFcuProFileId_ pFcuPayload_ = 
    FloodlightConfigurationsUpdate'
    { _fcuProFileId = _Coerce # pFcuProFileId_
    , _fcuPayload = pFcuPayload_
    , _fcuFields = Nothing
    }

-- | User profile ID associated with this request.
fcuProFileId :: Lens' FloodlightConfigurationsUpdate Int64
fcuProFileId
  = lens _fcuProFileId (\ s a -> s{_fcuProFileId = a})
      . _Coerce

-- | Multipart request metadata.
fcuPayload :: Lens' FloodlightConfigurationsUpdate FloodlightConfiguration
fcuPayload
  = lens _fcuPayload (\ s a -> s{_fcuPayload = a})

-- | Selector specifying which fields to include in a partial response.
fcuFields :: Lens' FloodlightConfigurationsUpdate (Maybe Text)
fcuFields
  = lens _fcuFields (\ s a -> s{_fcuFields = a})

instance GoogleRequest FloodlightConfigurationsUpdate
         where
        type Rs FloodlightConfigurationsUpdate =
             FloodlightConfiguration
        type Scopes FloodlightConfigurationsUpdate =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient FloodlightConfigurationsUpdate'{..}
          = go _fcuProFileId _fcuFields (Just AltJSON)
              _fcuPayload
              dFAReportingService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy FloodlightConfigurationsUpdateResource)
                      mempty
