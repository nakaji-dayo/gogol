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
-- Module      : Network.Google.Resource.Directory.ResolvedAppAccessSettings.ListTrustedApps
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of apps trusted by the admin of the logged in user.
--
-- /See:/ <https://developers.google.com/admin-sdk/directory/ Admin Directory API Reference> for @directory.resolvedAppAccessSettings.ListTrustedApps@.
module Network.Google.Resource.Directory.ResolvedAppAccessSettings.ListTrustedApps
    (
    -- * REST Resource
      ResolvedAppAccessSettingsListTrustedAppsResource

    -- * Creating a Request
    , resolvedAppAccessSettingsListTrustedApps
    , ResolvedAppAccessSettingsListTrustedApps

    -- * Request Lenses
    , raasltaFields
    ) where

import Network.Google.Directory.Types
import Network.Google.Prelude

-- | A resource alias for @directory.resolvedAppAccessSettings.ListTrustedApps@ method which the
-- 'ResolvedAppAccessSettingsListTrustedApps' request conforms to.
type ResolvedAppAccessSettingsListTrustedAppsResource
     =
     "admin" :>
       "directory" :>
         "v1" :>
           "trustedapps" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :> Get '[JSON] TrustedApps

-- | Retrieves the list of apps trusted by the admin of the logged in user.
--
-- /See:/ 'resolvedAppAccessSettingsListTrustedApps' smart constructor.
newtype ResolvedAppAccessSettingsListTrustedApps = ResolvedAppAccessSettingsListTrustedApps'
    { _raasltaFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResolvedAppAccessSettingsListTrustedApps' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raasltaFields'
resolvedAppAccessSettingsListTrustedApps
    :: ResolvedAppAccessSettingsListTrustedApps
resolvedAppAccessSettingsListTrustedApps = 
    ResolvedAppAccessSettingsListTrustedApps'
    { _raasltaFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
raasltaFields :: Lens' ResolvedAppAccessSettingsListTrustedApps (Maybe Text)
raasltaFields
  = lens _raasltaFields
      (\ s a -> s{_raasltaFields = a})

instance GoogleRequest
         ResolvedAppAccessSettingsListTrustedApps where
        type Rs ResolvedAppAccessSettingsListTrustedApps =
             TrustedApps
        type Scopes ResolvedAppAccessSettingsListTrustedApps
             = '[]
        requestClient
          ResolvedAppAccessSettingsListTrustedApps'{..}
          = go _raasltaFields (Just AltJSON) directoryService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy
                           ResolvedAppAccessSettingsListTrustedAppsResource)
                      mempty
