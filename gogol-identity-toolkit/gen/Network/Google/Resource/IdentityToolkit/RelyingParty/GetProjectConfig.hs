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
-- Module      : Network.Google.Resource.IdentityToolkit.RelyingParty.GetProjectConfig
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get project configuration.
--
-- /See:/ <https://developers.google.com/identity-toolkit/v3/ Google Identity Toolkit API Reference> for @identitytoolkit.relyingparty.getProjectConfig@.
module Network.Google.Resource.IdentityToolkit.RelyingParty.GetProjectConfig
    (
    -- * REST Resource
      RelyingPartyGetProjectConfigResource

    -- * Creating a Request
    , relyingPartyGetProjectConfig
    , RelyingPartyGetProjectConfig

    -- * Request Lenses
    , rpgpcDelegatedProjectNumber
    , rpgpcProjectNumber
    , rpgpcFields
    ) where

import Network.Google.IdentityToolkit.Types
import Network.Google.Prelude

-- | A resource alias for @identitytoolkit.relyingparty.getProjectConfig@ method which the
-- 'RelyingPartyGetProjectConfig' request conforms to.
type RelyingPartyGetProjectConfigResource =
     "identitytoolkit" :>
       "v3" :>
         "relyingparty" :>
           "getProjectConfig" :>
             QueryParam "delegatedProjectNumber" Text :>
               QueryParam "projectNumber" Text :>
                 QueryParam "fields" Text :>
                   QueryParam "alt" AltJSON :>
                     Get '[JSON]
                       IdentitytoolkitRelyingPartyGetProjectConfigResponse

-- | Get project configuration.
--
-- /See:/ 'relyingPartyGetProjectConfig' smart constructor.
data RelyingPartyGetProjectConfig = RelyingPartyGetProjectConfig'
    { _rpgpcDelegatedProjectNumber :: !(Maybe Text)
    , _rpgpcProjectNumber :: !(Maybe Text)
    , _rpgpcFields :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RelyingPartyGetProjectConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpgpcDelegatedProjectNumber'
--
-- * 'rpgpcProjectNumber'
--
-- * 'rpgpcFields'
relyingPartyGetProjectConfig
    :: RelyingPartyGetProjectConfig
relyingPartyGetProjectConfig = 
    RelyingPartyGetProjectConfig'
    { _rpgpcDelegatedProjectNumber = Nothing
    , _rpgpcProjectNumber = Nothing
    , _rpgpcFields = Nothing
    }

-- | Delegated GCP project number of the request.
rpgpcDelegatedProjectNumber :: Lens' RelyingPartyGetProjectConfig (Maybe Text)
rpgpcDelegatedProjectNumber
  = lens _rpgpcDelegatedProjectNumber
      (\ s a -> s{_rpgpcDelegatedProjectNumber = a})

-- | GCP project number of the request.
rpgpcProjectNumber :: Lens' RelyingPartyGetProjectConfig (Maybe Text)
rpgpcProjectNumber
  = lens _rpgpcProjectNumber
      (\ s a -> s{_rpgpcProjectNumber = a})

-- | Selector specifying which fields to include in a partial response.
rpgpcFields :: Lens' RelyingPartyGetProjectConfig (Maybe Text)
rpgpcFields
  = lens _rpgpcFields (\ s a -> s{_rpgpcFields = a})

instance GoogleRequest RelyingPartyGetProjectConfig
         where
        type Rs RelyingPartyGetProjectConfig =
             IdentitytoolkitRelyingPartyGetProjectConfigResponse
        type Scopes RelyingPartyGetProjectConfig =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient RelyingPartyGetProjectConfig'{..}
          = go _rpgpcDelegatedProjectNumber _rpgpcProjectNumber
              _rpgpcFields
              (Just AltJSON)
              identityToolkitService
          where go
                  = buildClient
                      (Proxy :: Proxy RelyingPartyGetProjectConfigResource)
                      mempty
