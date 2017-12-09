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
-- Module      : Network.Google.Resource.DFAReporting.FloodlightConfigurations.Patch
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing floodlight configuration. This method supports patch
-- semantics.
--
-- /See:/ <https://developers.google.com/doubleclick-advertisers/ DCM/DFA Reporting And Trafficking API Reference> for @dfareporting.floodlightConfigurations.patch@.
module Network.Google.Resource.DFAReporting.FloodlightConfigurations.Patch
    (
    -- * REST Resource
      FloodlightConfigurationsPatchResource

    -- * Creating a Request
    , floodlightConfigurationsPatch
    , FloodlightConfigurationsPatch

    -- * Request Lenses
    , fcpProFileId
    , fcpPayload
    , fcpId
    , fcpFields
    ) where

import Network.Google.DFAReporting.Types
import Network.Google.Prelude

-- | A resource alias for @dfareporting.floodlightConfigurations.patch@ method which the
-- 'FloodlightConfigurationsPatch' request conforms to.
type FloodlightConfigurationsPatchResource =
     "dfareporting" :>
       "v3.0" :>
         "userprofiles" :>
           Capture "profileId" (Textual Int64) :>
             "floodlightConfigurations" :>
               QueryParam "id" (Textual Int64) :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     ReqBody '[JSON] FloodlightConfiguration :>
                       Patch '[JSON] FloodlightConfiguration

-- | Updates an existing floodlight configuration. This method supports patch
-- semantics.
--
-- /See:/ 'floodlightConfigurationsPatch' smart constructor.
data FloodlightConfigurationsPatch = FloodlightConfigurationsPatch'
    { _fcpProFileId :: !(Textual Int64)
    , _fcpPayload :: !FloodlightConfiguration
    , _fcpId :: !(Textual Int64)
    , _fcpFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'FloodlightConfigurationsPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcpProFileId'
--
-- * 'fcpPayload'
--
-- * 'fcpId'
--
-- * 'fcpFields'
floodlightConfigurationsPatch
    :: Int64 -- ^ 'fcpProFileId'
    -> FloodlightConfiguration -- ^ 'fcpPayload'
    -> Int64 -- ^ 'fcpId'
    -> FloodlightConfigurationsPatch
floodlightConfigurationsPatch pFcpProFileId_ pFcpPayload_ pFcpId_ = 
    FloodlightConfigurationsPatch'
    { _fcpProFileId = _Coerce # pFcpProFileId_
    , _fcpPayload = pFcpPayload_
    , _fcpId = _Coerce # pFcpId_
    , _fcpFields = Nothing
    }

-- | User profile ID associated with this request.
fcpProFileId :: Lens' FloodlightConfigurationsPatch Int64
fcpProFileId
  = lens _fcpProFileId (\ s a -> s{_fcpProFileId = a})
      . _Coerce

-- | Multipart request metadata.
fcpPayload :: Lens' FloodlightConfigurationsPatch FloodlightConfiguration
fcpPayload
  = lens _fcpPayload (\ s a -> s{_fcpPayload = a})

-- | Floodlight configuration ID.
fcpId :: Lens' FloodlightConfigurationsPatch Int64
fcpId
  = lens _fcpId (\ s a -> s{_fcpId = a}) . _Coerce

-- | Selector specifying which fields to include in a partial response.
fcpFields :: Lens' FloodlightConfigurationsPatch (Maybe Text)
fcpFields
  = lens _fcpFields (\ s a -> s{_fcpFields = a})

instance GoogleRequest FloodlightConfigurationsPatch
         where
        type Rs FloodlightConfigurationsPatch =
             FloodlightConfiguration
        type Scopes FloodlightConfigurationsPatch =
             '["https://www.googleapis.com/auth/dfatrafficking"]
        requestClient FloodlightConfigurationsPatch'{..}
          = go _fcpProFileId (Just _fcpId) _fcpFields
              (Just AltJSON)
              _fcpPayload
              dFAReportingService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy FloodlightConfigurationsPatchResource)
                      mempty
