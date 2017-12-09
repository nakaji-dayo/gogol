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
-- Module      : Network.Google.Resource.Directory.ChromeosDevices.MoveDevicesToOu
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Move or insert multiple Chrome OS Devices to organizational unit
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.chromeosdevices.moveDevicesToOu@.
module Network.Google.Resource.Directory.ChromeosDevices.MoveDevicesToOu
    (
    -- * REST Resource
      ChromeosDevicesMoveDevicesToOuResource

    -- * Creating a Request
    , chromeosDevicesMoveDevicesToOu
    , ChromeosDevicesMoveDevicesToOu

    -- * Request Lenses
    , cdmdtoPayload
    , cdmdtoOrgUnitPath
    , cdmdtoCustomerId
    , cdmdtoFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.chromeosdevices.moveDevicesToOu@ method which the
-- 'ChromeosDevicesMoveDevicesToOu' request conforms to.
type ChromeosDevicesMoveDevicesToOuResource =
     "admin" :>
       "directory" :>
         "v1" :>
           "customer" :>
             Capture "customerId" Text :>
               "devices" :>
                 "chromeos" :>
                   "moveDevicesToOu" :>
                     QueryParam "orgUnitPath" Text :>
                       QueryParam "fields" Text :>
                         QueryParam "alt" AltJSON :>
                           ReqBody '[JSON] ChromeOSMoveDevicesToOu :>
                             Post '[JSON] ()

-- | Move or insert multiple Chrome OS Devices to organizational unit
--
-- /See:/ 'chromeosDevicesMoveDevicesToOu' smart constructor.
data ChromeosDevicesMoveDevicesToOu = ChromeosDevicesMoveDevicesToOu'
    { _cdmdtoPayload :: !ChromeOSMoveDevicesToOu
    , _cdmdtoOrgUnitPath :: !Text
    , _cdmdtoCustomerId :: !Text
    , _cdmdtoFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChromeosDevicesMoveDevicesToOu' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdmdtoPayload'
--
-- * 'cdmdtoOrgUnitPath'
--
-- * 'cdmdtoCustomerId'
--
-- * 'cdmdtoFields'
chromeosDevicesMoveDevicesToOu
    :: ChromeOSMoveDevicesToOu -- ^ 'cdmdtoPayload'
    -> Text -- ^ 'cdmdtoOrgUnitPath'
    -> Text -- ^ 'cdmdtoCustomerId'
    -> ChromeosDevicesMoveDevicesToOu
chromeosDevicesMoveDevicesToOu pCdmdtoPayload_ pCdmdtoOrgUnitPath_ pCdmdtoCustomerId_ = 
    ChromeosDevicesMoveDevicesToOu'
    { _cdmdtoPayload = pCdmdtoPayload_
    , _cdmdtoOrgUnitPath = pCdmdtoOrgUnitPath_
    , _cdmdtoCustomerId = pCdmdtoCustomerId_
    , _cdmdtoFields = Nothing
    }

-- | Multipart request metadata.
cdmdtoPayload :: Lens' ChromeosDevicesMoveDevicesToOu ChromeOSMoveDevicesToOu
cdmdtoPayload
  = lens _cdmdtoPayload
      (\ s a -> s{_cdmdtoPayload = a})

-- | Full path of the target organizational unit or its ID
cdmdtoOrgUnitPath :: Lens' ChromeosDevicesMoveDevicesToOu Text
cdmdtoOrgUnitPath
  = lens _cdmdtoOrgUnitPath
      (\ s a -> s{_cdmdtoOrgUnitPath = a})

-- | Immutable ID of the G Suite account
cdmdtoCustomerId :: Lens' ChromeosDevicesMoveDevicesToOu Text
cdmdtoCustomerId
  = lens _cdmdtoCustomerId
      (\ s a -> s{_cdmdtoCustomerId = a})

-- | Selector specifying which fields to include in a partial response.
cdmdtoFields :: Lens' ChromeosDevicesMoveDevicesToOu (Maybe Text)
cdmdtoFields
  = lens _cdmdtoFields (\ s a -> s{_cdmdtoFields = a})

instance GoogleRequest ChromeosDevicesMoveDevicesToOu
         where
        type Rs ChromeosDevicesMoveDevicesToOu = ()
        type Scopes ChromeosDevicesMoveDevicesToOu =
             '["https://www.googleapis.com/auth/admin.directory.device.chromeos"]
        requestClient ChromeosDevicesMoveDevicesToOu'{..}
          = go _cdmdtoCustomerId (Just _cdmdtoOrgUnitPath)
              _cdmdtoFields
              (Just AltJSON)
              _cdmdtoPayload
              directoryService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy ChromeosDevicesMoveDevicesToOuResource)
                      mempty
