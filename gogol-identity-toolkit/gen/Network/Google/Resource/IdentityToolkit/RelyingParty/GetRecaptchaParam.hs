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
-- Module      : Network.Google.Resource.IdentityToolkit.RelyingParty.GetRecaptchaParam
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get recaptcha secure param.
--
-- /See:/ <https://developers.google.com/identity-toolkit/v3/ Google Identity Toolkit API Reference> for @identitytoolkit.relyingparty.getRecaptchaParam@.
module Network.Google.Resource.IdentityToolkit.RelyingParty.GetRecaptchaParam
    (
    -- * REST Resource
      RelyingPartyGetRecaptchaParamResource

    -- * Creating a Request
    , relyingPartyGetRecaptchaParam
    , RelyingPartyGetRecaptchaParam

    -- * Request Lenses
    , rpgrpFields
    ) where

import Network.Google.IdentityToolkit.Types
import Network.Google.Prelude

-- | A resource alias for @identitytoolkit.relyingparty.getRecaptchaParam@ method which the
-- 'RelyingPartyGetRecaptchaParam' request conforms to.
type RelyingPartyGetRecaptchaParamResource =
     "identitytoolkit" :>
       "v3" :>
         "relyingparty" :>
           "getRecaptchaParam" :>
             QueryParam "fields" Text :>
               QueryParam "alt" AltJSON :>
                 Get '[JSON] GetRecaptchaParamResponse

-- | Get recaptcha secure param.
--
-- /See:/ 'relyingPartyGetRecaptchaParam' smart constructor.
newtype RelyingPartyGetRecaptchaParam = RelyingPartyGetRecaptchaParam'
    { _rpgrpFields :: Maybe Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'RelyingPartyGetRecaptchaParam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpgrpFields'
relyingPartyGetRecaptchaParam
    :: RelyingPartyGetRecaptchaParam
relyingPartyGetRecaptchaParam = 
    RelyingPartyGetRecaptchaParam'
    { _rpgrpFields = Nothing
    }

-- | Selector specifying which fields to include in a partial response.
rpgrpFields :: Lens' RelyingPartyGetRecaptchaParam (Maybe Text)
rpgrpFields
  = lens _rpgrpFields (\ s a -> s{_rpgrpFields = a})

instance GoogleRequest RelyingPartyGetRecaptchaParam
         where
        type Rs RelyingPartyGetRecaptchaParam =
             GetRecaptchaParamResponse
        type Scopes RelyingPartyGetRecaptchaParam =
             '["https://www.googleapis.com/auth/cloud-platform"]
        requestClient RelyingPartyGetRecaptchaParam'{..}
          = go _rpgrpFields (Just AltJSON)
              identityToolkitService
          where go
                  = buildClient
                      (Proxy ::
                         Proxy RelyingPartyGetRecaptchaParamResource)
                      mempty
